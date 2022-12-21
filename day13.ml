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

let lines = read_input "./inputs/day13.txt"

type lst = Int of int | List of lst list

type tlst = Int of int | ListStart | List of tlst list

let rec print_tlst = function
| ListStart -> Printf.printf "[ "
| Int n -> Printf.printf "%d, " n
| List xs -> 
  Printf.printf "[ ";
  List.iter print_tlst xs;
  Printf.printf " ]"

let rec print_lst : lst -> unit = 
function
| Int n -> Printf.printf "%d " n
| List xs -> 
  Printf.printf "[ ";
  List.iter print_lst xs;
  Printf.printf "] "

let pairs = List.fold_right (fun line (pairs, pair) ->
    if line = "" then pair :: pairs, []
    else pairs, line :: pair
  ) lines ([], [])
  |> fun (pairs, pair) -> pair :: pairs

(* let () = List.iter (fun pair -> List.iter (Printf.printf "%s\n") pair; Printf.printf "\n") pairs *)

let parse_list lst =
  let len = String.length lst in
  let stack = Stack.create () in
  let rec aux i =
    if i >= len - 1 then
      let xs = ref [] in
      while Stack.top stack <> ListStart do
        xs := (Stack.pop stack) :: !xs
      done;
      let _ = Stack.pop stack in
      assert (Stack.is_empty stack);
      List !xs
    else if lst.[i] = ']' then 
      let xs = ref [] in
      while Stack.top stack <> ListStart do
        xs := (Stack.pop stack) :: !xs
      done;
      let _ = Stack.pop stack in
      Stack.push (List !xs) stack;
      aux (i + 1)
    else if lst.[i] = '[' then 
      let () = Stack.push ListStart stack in
      aux (i + 1)
    else if lst.[i] = ',' then aux (i + 1)
    else
      let size = ref 0 in
      while lst.[i + !size] <> ',' && lst.[i + !size] <> ']' do
        incr size
      done;
      let () = if !size > 0 then
        let n = String.sub lst i !size in
        let n = int_of_string n in
        Stack.push (Int n) stack
      in
      aux (i + !size)
  in
  let tlst = aux 0 in
  let rec to_lst tlst =
    match tlst with
    | Int n -> (Int n : lst)
    | List xs -> (List (List.map to_lst xs) : lst)
    | ListStart -> failwith "unexpected ListStart"
  in
  to_lst tlst

type status = T | F | C

let rec compare : lst list -> lst list -> status = 
  fun xs ys ->
  match xs, ys with
  | [], _ :: _ -> T
  | _ :: _, [] -> F
  | [], []     -> C
  | Int n :: xs, Int m :: ys ->
    if n < m then T else
    if n = m then compare xs ys
    else F
  | List xs' :: xs, List ys' :: ys ->
    (match compare xs' ys' with
    | T -> T
    | F -> F
    | C -> compare xs ys)
  | Int n :: xs, List ys' :: ys ->
    (match compare [Int n] ys' with
    | T -> T
    | F -> F
    | C -> compare xs ys)
  | List xs' :: xs, Int n :: ys ->
    (match compare xs' [Int n] with
    | T -> T
    | F -> F
    | C -> compare xs ys)

let compare_lst : lst -> lst -> bool =
  fun xs ys ->
  match xs, ys with
  | List xs, List ys -> 
    (match compare xs ys with
    | T -> true
    | F -> false
    | C -> failwith "invalid C"
    )
  | _ -> failwith "dunno how to compare"


let () = assert (compare_lst (parse_list "[1,1,3,1,1]") (parse_list "[1,1,5,1,1]"))
let () = assert (compare_lst (parse_list "[[1],[2,3,4]]") (parse_list "[[1],4]"))
let () = assert (not @@ compare_lst (parse_list "[9]") (parse_list "[[8,7,6]]"))
let () = assert (compare_lst (parse_list "[[4,4],4,4]") (parse_list "[[4,4],4,4,4]"))
let () = assert (not @@ compare_lst (parse_list "[7,7,7,7]") (parse_list "[7,7,7]"))
let () = assert (compare_lst (parse_list "[]") (parse_list "[3]"))
let () = assert (not @@ compare_lst (parse_list "[[[]]]") (parse_list "[[]]"))
let () = assert (not @@ compare_lst (parse_list "[1,[2,[3,[4,[5,6,7]]]],8,9]") (parse_list "[1,[2,[3,[4,[5,6,0]]]],8,9]"))

let ids = pairs
  |> List.mapi (fun i ps ->
    match ps with
    | [ l ; r ] ->
      let l, r = parse_list l, parse_list r in
      if compare_lst l r then Some i else None 
    | _ -> None
    )
  |> List.filter Option.is_some
  |> List.fold_left (fun s o ->
      match o with
      | Some n -> s + (n + 1)
      | None -> failwith "unexpected None"
    ) 0

let () = Printf.printf "==== Day 13 ====\n"
let () = Printf.printf "Part1> %d\n" ids

let s_divider = parse_list "[[2]]"
let e_divider = parse_list "[[6]]"
let packets = s_divider :: e_divider :: (List.map parse_list (List.concat pairs))

let packets = List.sort (fun p1 p2 -> if compare_lst p1 p2 then -1 else 1) packets

let _, si, ei = List.fold_left (fun (curr, si, ei) packet -> 
    if packet = s_divider then (curr + 1, curr, ei)
    else if packet = e_divider then (curr + 1, si, curr)
    else (curr + 1, si, ei)
  ) (0, -1, -1) packets

let decoder_key = (si + 1) * (ei + 1)
let () = Printf.printf "Part2> %d\n" decoder_key