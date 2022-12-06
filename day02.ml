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

let lines = read_input "./inputs/day02.txt"

type move = Rock | Paper | Scissors

let of_string = function 
| "A" | "X" -> Rock 
| "B" | "Y" -> Paper 
| "C" | "Z" -> Scissors
| _ -> failwith "cannot convert string to move"

let to_string = function
| Rock -> "R"
| Paper -> "P"
| Scissors -> "S"

let move_point = function Rock -> 1 | Paper -> 2 | Scissors -> 3

type win_lose_draw = L | W | D

let eval_move = function
| Rock, Paper -> W
| Rock, Scissors -> L
| Rock, Rock -> D
| Paper, Rock -> L
| Paper, Scissors -> W
| Paper, Paper -> D
| Scissors, Paper -> L
| Scissors, Rock -> W
| Scissors, Scissors -> D

let outcome_point = function L -> 0 | W -> 6 | D -> 3

let outcome_of_string = function
| "X" -> L
| "Y" -> D
| "Z" -> W
| _ -> failwith "connot convert string to outcome"

let moves = List.map (fun line -> 
  match String.split_on_char ' ' line with
  | [f ; s] -> of_string f, of_string s
  | _ -> failwith "invalid move") lines

let list_sum = List.fold_left (+) 0 

let total_score moves =
  List.map (fun m ->
    let outcome = eval_move m in
    let outcome_point = outcome_point outcome in
    let (_, s) = m in
    let move_point = move_point s in
    outcome_point + move_point) moves
  |> list_sum

let find_move_for_outcome o m =
  match o, m with
  | W, Rock -> Paper
  | W, Paper -> Scissors
  | W, Scissors -> Rock
  | D, Rock -> Rock
  | D, Paper -> Paper
  | D, Scissors -> Scissors
  | L, Paper -> Rock
  | L, Rock -> Scissors
  | L, Scissors -> Paper

let new_moves = List.map (fun line ->
  match String.split_on_char ' ' line with
  | [m ; o] -> 
    let outcome = outcome_of_string o in
    let opponent_move = of_string m in
    let my_move = find_move_for_outcome outcome opponent_move in
    opponent_move, my_move
  | _ -> failwith "invalid move"
) lines

let () = Printf.printf "==== Day 02 ====\n"
let () = Printf.printf "Part1> %d\n" (total_score moves)
let () = Printf.printf "Part2> %d\n" (total_score new_moves)