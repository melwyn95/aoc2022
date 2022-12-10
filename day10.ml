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

let lines = read_input "./inputs/day10.txt"

type instr = Noop | Addx of int

let instrs = lines
  |> List.map (fun line -> 
    match line with
    | "noop" -> Noop
    | _ when String.starts_with ~prefix:"addx" line ->
      let x = String.sub line 5 (String.length line - 5) in
      let x = int_of_string x in
      Addx x
    | _ -> failwith "invalid instruction"
  )

type state = {
  reg   : int ;
  clock : int ;
}

let rec exec ~prev ~state instrs =
  match instrs with
  | [] -> []
  | Noop :: instrs ->
    let state = { reg = state.reg + prev ; clock = state.clock + 1 } in
    state :: exec ~state ~prev:0 instrs
  | Addx x :: instrs ->
    let state1 = { reg = state.reg + prev ; clock = state.clock + 1 } in
    let state2 = { state1 with clock = state1.clock + 1 } in
    state1 :: state2 :: exec ~state:state2 ~prev:x instrs

let state = { reg = 1 ; clock = 0 }
let states = exec ~state ~prev:0 instrs

let sum = List.fold_left ( + ) 0
let part1 = states
 |> List.filter (fun ({ clock ; _ }) ->
     clock = 20 
  || clock = 60 
  || clock = 100 
  || clock = 140 
  || clock = 180 
  || clock = 220
  )
|> List.map (fun ({ reg ; clock }) -> reg * clock)
|> sum

(* let () = List.iter (fun ({ clock ; reg }) -> Printf.printf "%d> %d\n" clock reg) states *)

let () = Printf.printf "==== Day 10 ====\n"
let () = Printf.printf "Part1> %d\n" part1
let () = Printf.printf "Part2>\n"

let rec take n xs =
  if n = 0 then [], xs
  else match xs with
  | [] -> [], []
  | x :: xs -> 
    let ys, xs = take (n - 1) xs in 
    x :: ys, xs

let row1, states = take 40 states
let row2, states = take 40 states
let row3, states = take 40 states
let row4, states = take 40 states
let row5, states = take 40 states
let row6, states = take 40 states


let () = List.iter (fun ({ clock ; reg }) -> 
  if clock >= reg && clock <= (reg + 2)
  then Printf.printf "#"
  else Printf.printf "."
) row1
let () = Printf.printf "\n"
let () = List.iter (fun ({ clock ; reg }) ->
  let clock = clock - 40 in
  if clock >= reg && clock <= (reg + 2)
  then Printf.printf "#"
  else Printf.printf "."  
) row2
let () = Printf.printf "\n"
let () = List.iter (fun ({ clock ; reg }) ->
  let clock = clock - 80 in
  if clock >= reg && clock <= (reg + 2)
  then Printf.printf "#"
  else Printf.printf "."  
) row3
let () = Printf.printf "\n"
let () = List.iter (fun ({ clock ; reg }) ->
  let clock = clock - 120 in
  if clock >= reg && clock <= (reg + 2)
  then Printf.printf "#"
  else Printf.printf "."  
) row4
let () = Printf.printf "\n"
let () = List.iter (fun ({ clock ; reg }) ->
  let clock = clock - 160 in
  if clock >= reg && clock <= (reg + 2)
  then Printf.printf "#"
  else Printf.printf "."  
) row5
let () = Printf.printf "\n"
let () = List.iter (fun ({ clock ; reg }) ->
  let clock = clock - 200 in
  if clock >= reg && clock <= (reg + 2)
  then Printf.printf "#"
  else Printf.printf "."  
) row6
let () = Printf.printf "\n"