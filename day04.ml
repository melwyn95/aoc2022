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

let lines = read_input "./inputs/day04.txt"

let elf_pairs = lines
  |> List.map (String.split_on_char ',')
  |> List.map (fun es ->
      match es with
      | [e1 ; e2] ->
        let p1 = String.split_on_char '-' e1 in
        let p2 = String.split_on_char '-' e2 in
        (match p1, p2 with
        | [s1;s2], [s3;s4] ->
          let int = int_of_string in
          (int s1, int s2), (int s3, int s4)
        | _ -> failwith "invalid elf pair -"
        )
      | _ -> failwith "invalid elf pair"
    )

let overlaps = elf_pairs
  |> List.map (fun ((s1, s2), (s3, s4)) ->
      let b1 = s1 - s3 in
      let b2 = s2 - s4 in
      b1 >= 0 && b2 <= 0 || b1 <= 0 && b2 >= 0
    )
  |> List.filter (fun x -> x)
  |> List.length

let overlaps_at_all = elf_pairs
  |> List.map (fun ((s1, s2), (s3, s4)) ->
      if s1 < s3 then
        s3 <= s2
      else
        s4 >= s1
    )
  |> List.filter (fun x -> x)
  |> List.length


let () = Printf.printf "==== Day 04 ====\n"
let () = Printf.printf "Part1> %d\n" overlaps
let () = Printf.printf "Part1> %d\n" overlaps_at_all



(* 
        ....
      ....

      ....
        ....

      ....
      ....

*)