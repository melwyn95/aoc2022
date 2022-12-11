let read_input file =
  let chan = open_in file in
  let rec acc lines =
    try
      let line = input_line chan in
      acc (line :: lines)
    with End_of_file -> List.rev lines
  in
  let lines = acc [] in
  let () = close_in chan in
  lines

let lines = read_input "./inputs/day11.txt"

type op = Var | Lit of int | Mul of op * op | Add of op * op

let rec evaluate_op ~old op =
  match op with
  | Var -> old
  | Lit n -> n
  | Mul (o1, o2) -> 
    (evaluate_op ~old o1) * (evaluate_op ~old o2)
  | Add (o1, o2) ->
    (evaluate_op ~old o1) + (evaluate_op ~old o2)

type monkey = {
  id        : int ;
  items     : int list ;
  operation : op ;
  divisible : int ;
  yes       : int ;
  no        : int ;
  inspect   : int64 ;
}

let print_monkey ({ id ; items ; inspect ; _ }) =
  Printf.printf "==== Monkey %d ====\n"id;
  (* Printf.printf "[ %s ]\n" (String.concat ", " (List.map (string_of_int) items)); *)
  Printf.printf "inspect: %s\n" (Int64.to_string inspect)

let parse_id id =
     String.sub id 7 (String.length id - 8)
  |> int_of_string 

let parse_items items = 
     String.sub items 18 (String.length items - 18)
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.map int_of_string

let parse_lit = function
  | "old" -> Var
  | n     -> Lit (int_of_string n)

let parse_operation operation =
     String.sub operation 19 (String.length operation - 19)
  |> String.split_on_char ' '
  |> List.map String.trim
  |> function 
     | [ o1 ; "+" ; o3 ] ->
        Add (parse_lit o1, parse_lit o3)
     | [ o1 ; "*" ; o3 ] ->
        Mul (parse_lit o1, parse_lit o3)
     | _ -> failwith "invald operation"

let parse_divisible divisible =
     String.sub divisible 21 (String.length divisible - 21)
  |> int_of_string

let parse_yes yes =
     String.sub yes 29 (String.length yes - 29)
  |> int_of_string

let parse_no no =
     String.sub no 30 (String.length no - 30)
  |> int_of_string

let monkeys_og = lines
  |> List.fold_left (fun (curr, all) line ->
    if line = "" then
      [], curr :: all
    else
      line :: curr, all  
  ) ([], [])
  |> fun (curr, all) -> curr :: all
  |> List.rev_map (fun lines ->
    match lines with
    | [ no ; yes ; divisible ; operation ; items ; id ] ->
      { id = parse_id id
      ; items = parse_items items
      ; operation = parse_operation operation
      ; divisible = parse_divisible divisible
      ; yes = parse_yes yes
      ; no = parse_no no
      ; inspect = Int64.zero
      }
    | _ -> failwith "invalid monkey"
  )

let update_monkeys xs (id, worry) = List.map (fun monkey ->
  if monkey.id = id
  then { monkey with items = worry :: monkey.items }
  else monkey
) xs

let rec round monkeys prev =
  match monkeys with
  | [] -> prev
  | monkey :: monkeys ->
    let next_list = List.map (fun old ->
      let item = evaluate_op ~old monkey.operation in
      let item = item / 3 in
      if item mod monkey.divisible = 0
      then item, monkey.yes
      else item, monkey.no 
    ) monkey.items in
    let monkey = { monkey 
      with items = [] ; inspect = Int64.add monkey.inspect (Int64.of_int @@ List.length next_list) } in
    let prev, monkeys = List.fold_left (fun (prev, monkeys) (worry, next) ->
      if next > monkey.id 
      then prev, update_monkeys monkeys (next, worry)
      else update_monkeys prev (next, worry), monkeys
    ) 
    (prev, monkeys) next_list in
    let prev = prev @ [monkey] in
    round monkeys prev

let update_inspect inspects monkeys =
  List.map2 (fun inspect monkey ->
    inspect + List.length monkey.items  
  ) inspects monkeys

let rec times n init =
  if n = 1 then
    let monkeys = round init [] in
    monkeys
  else
    let init = round init [] in
    times (n - 1) init

let monkeys = times 20 monkeys_og
(* let () = List.iter print_monkey monkeys *)

let inspects = monkeys
  |> List.map (fun monkey -> monkey.inspect) 
  |> List.sort Int64.compare
  |> List.rev

let i1, i2 = List.hd inspects, List.hd (List.tl inspects)

let () = Printf.printf "==== Day 11 ====\n"
let () = Printf.printf "Part1> %s\n" (Int64.to_string @@ Int64.mul i1 i2)

let rec round_new monkeys div prev =
  match monkeys with
  | [] -> prev
  | monkey :: monkeys ->
    let next_list = List.map (fun old ->
      let item = evaluate_op ~old monkey.operation in
      let item = item mod div in
      if item mod monkey.divisible = 0
      then item, monkey.yes
      else item, monkey.no 
    ) monkey.items in
    let monkey = { monkey 
      with items = [] ; inspect = Int64.add monkey.inspect (Int64.of_int @@ List.length next_list) } in
    let prev, monkeys = List.fold_left (fun (prev, monkeys) (worry, next) ->
      if next > monkey.id 
      then prev, update_monkeys monkeys (next, worry)
      else update_monkeys prev (next, worry), monkeys
    ) 
    (prev, monkeys) next_list in
    let prev = prev @ [monkey] in
    round_new monkeys div prev

let rec times n div init =
  if n = 1 then
    let monkeys = round_new init div [] in
    monkeys
  else
    let init = round_new init div [] in
    times (n - 1) div init

let div = List.fold_left (fun acc monkey -> acc * monkey.divisible) 1 monkeys_og
  
let monkeys = times 10000 div monkeys_og
let inspects = monkeys
  |> List.map (fun monkey -> monkey.inspect) 
  |> List.sort Int64.compare
  |> List.rev

let i1, i2 = List.hd inspects, List.hd (List.tl inspects)

let () = Printf.printf "Part2> %s\n" (Int64.to_string @@ Int64.mul i1 i2)
