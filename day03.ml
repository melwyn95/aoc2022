module CSet = Set.Make(Char)

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

let rucksacks = read_input "./inputs/day03.txt"

let rec range ?(inc = 1) s e =
  if s >= e then []
  else
    s :: range ~inc (s + inc) e

let chars s = String.fold_right (List.cons) s []

let compartmentalize ?(n = 2) ~len ~take s =
  let size = len / n in
  let range = range ~inc:size 0 len in
  let compartments = List.map (fun idx ->
    take s idx size  
  ) range in
  compartments

let compartments = rucksacks
  |> List.map (fun s -> 
    compartmentalize ~len:(String.length s) ~take:String.sub s)
  |> List.map (List.map chars)
  |> List.map (List.map CSet.of_list)

let find_common css = css
  |> List.fold_left CSet.inter (List.hd css)
  |> CSet.elements
  |> List.hd

let find_priority c =
  match c with
  | 'a' .. 'z' -> 1 + (Char.code c - Char.code 'a')
  | 'A' .. 'Z' -> 27 + (Char.code c - Char.code 'A')
  | _ -> failwith "invalid char"

let mistakes = compartments
  |> List.map find_common
  |> List.map find_priority
  |> List.fold_left (+) 0

let split_into_grps rs =
  let range = range ~inc:3 0 (List.length rs) in
  let rec aux i range xs acc =
    match range, xs with
    | [], [] -> [acc]
    | r :: rs, x :: xs when r = i ->
      acc :: aux (i + 1) rs xs [x]
    | rs, x :: xs ->
      aux (i + 1) rs xs (x :: acc)
    | _ -> failwith "unexpected"
  in
  aux 0 (List.tl range) rs []

let gs = split_into_grps rucksacks
let priority = gs
  |> List.map (List.map chars)
  |> List.map (List.map CSet.of_list)
  |> List.map find_common
  |> List.map find_priority
  |> List.fold_left (+) 0

let () = Printf.printf "==== Day 03 ====\n"
let () = Printf.printf "Part1> %d\n" mistakes
let () = Printf.printf "Part2> %d\n" priority