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

let lines = read_input "./inputs/day05.txt"

let b = ref true
let stacks, moves = List.partition (fun s ->
  if s = ""
  then 
    let () = b := false in
    !b
  else !b) lines

type move = int * int * int
let moves = moves
  |> List.tl
  |> List.map (fun move ->
      match String.split_on_char ' ' move with
      | ["move" ; qty ; "from" ; from ; "to" ; to_] ->
        int_of_string qty, int_of_string from, int_of_string to_
      | _ -> failwith "invalid move"
    )

let normalize_spaces line =
  let _, line = List.fold_right (fun line (p, s) ->
    if line = "" && p = false then
      true, "" :: s
    else if line = "" && p = true then
      false, s
    else
      false, line :: s
  ) line (false, []) in
  line

let stacks = stacks
  |> List.rev
  |> List.tl
  |> List.fold_left (fun stacks line ->
      match stacks with
      | [] ->
        String.split_on_char ' ' line
        |> List.map (fun s -> [String.get s 1])
      | _ ->  
        let line = String.split_on_char ' ' line in
        let line = normalize_spaces line in
        let line = normalize_spaces line in
        List.map2 (fun s stack ->
          if s = "" then stack else
          let s = String.get s 1 in
          s :: stack
        ) line stacks
    ) []

let rec take n xs =
  if n = 0 then [] else
  match xs with
  | [] when n > 0 -> failwith "can't take"
  | x :: xs when n > 0 ->
    x :: take (n - 1) xs
  | _ -> failwith "invalid take"

let rec drop n xs =
  if n = 0 then xs else
  drop (n - 1) (List.tl xs)

let apply_move ?(rev = true) stacks (qty, from, to_) =
  let f = List.nth stacks (from - 1) in
  let t = 
    let t = take qty f in
    if rev 
    then (List.rev t) @ List.nth stacks (to_ - 1)
    else t @ List.nth stacks (to_ - 1)
  in
  let f = drop qty f in
  List.mapi (fun i stack -> 
    if (i + 1) = from then f else
    if (i + 1) = to_ then t else stack
  ) stacks

let stacks' = List.fold_left apply_move stacks moves
let part1 = String.of_seq @@ List.to_seq @@ List.map List.hd stacks'

let stacks' = List.fold_left (apply_move ~rev:false) stacks moves
let part2 = String.of_seq @@ List.to_seq @@ List.map List.hd stacks'

let () = Printf.printf "==== Day 05 ====\n"
let () = Printf.printf "Part1> %s\n" part1
let () = Printf.printf "Part1> %s\n" part2
