let read_input file =
  let chan = open_in file in
  let line = input_line chan in
  let () = close_in chan in
  line

let line = read_input "./inputs/day06.txt"

let is_unique (a, b, c, d) =
  a <> b && a <> c && a <> d &&
  b <> c && b <> d &&
  c <> d

let find_start_of_packet signal =
  let chars = List.of_seq @@ String.to_seq signal in
  let rec sliding_window (a, b, c, d) processed chars =
    match chars with
    | [] -> failwith "dunno wat to do"
    | e :: chars ->
      if is_unique (b, c, d, e) then processed + 1
      else sliding_window (b, c, d, e) (processed + 1) chars
  in
  let window, chars = 
    match chars with
    | a :: b :: c :: d :: chars -> (a, b, c, d), chars
    | _ -> failwith "not enought chars to start"
  in
  if is_unique window then 4
  else sliding_window window 4 chars

let is_unique (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) =
     c1 <> c2   
  && c1 <> c3  && c2 <> c3  
  && c1 <> c4  && c2 <> c4  && c3 <> c4  
  && c1 <> c5  && c2 <> c5  && c3 <> c5  && c4 <> c5  
  && c1 <> c6  && c2 <> c6  && c3 <> c6  && c4 <> c6  && c5 <> c6  
  && c1 <> c7  && c2 <> c7  && c3 <> c7  && c4 <> c7  && c5 <> c7  && c6 <> c7  
  && c1 <> c8  && c2 <> c8  && c3 <> c8  && c4 <> c8  && c5 <> c8  && c6 <> c8  && c7 <> c8  
  && c1 <> c9  && c2 <> c9  && c3 <> c9  && c4 <> c9  && c5 <> c9  && c6 <> c9  && c7 <> c9  && c8 <> c9  
  && c1 <> c10 && c2 <> c10 && c3 <> c10 && c4 <> c10 && c5 <> c10 && c6 <> c10 && c7 <> c10 && c8 <> c10 && c9 <> c10 
  && c1 <> c11 && c2 <> c11 && c3 <> c11 && c4 <> c11 && c5 <> c11 && c6 <> c11 && c7 <> c11 && c8 <> c11 && c9 <> c11 && c10 <> c11
  && c1 <> c12 && c2 <> c12 && c3 <> c12 && c4 <> c12 && c5 <> c12 && c6 <> c12 && c7 <> c12 && c8 <> c12 && c9 <> c12 && c10 <> c12 && c11 <> c12 
  && c1 <> c13 && c2 <> c13 && c3 <> c13 && c4 <> c13 && c5 <> c13 && c6 <> c13 && c7 <> c13 && c8 <> c13 && c9 <> c13 && c10 <> c13 && c11 <> c13 && c12 <> c13 
  && c1 <> c14 && c2 <> c14 && c3 <> c14 && c4 <> c14 && c5 <> c14 && c6 <> c14 && c7 <> c14 && c8 <> c14 && c9 <> c14 && c10 <> c14 && c11 <> c14 && c12 <> c14 && c13 <> c14

let find_start_of_message signal =
  let chars = List.of_seq @@ String.to_seq signal in
  let rec sliding_window (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) processed chars =
    match chars with
    | [] -> failwith "dunno wat to do"
    | e :: chars ->
      if is_unique (c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, e) then processed + 1
      else sliding_window (c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, e) (processed + 1) chars
  in
  let window, chars = 
    match chars with
    | c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: c12 :: c13 :: c14 :: chars -> 
     (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14), chars
    | _ -> failwith "not enought chars to start"
  in
  if is_unique window then 14
  else sliding_window window 14 chars

let () = Printf.printf "==== Day 06 ====\n"
let () = Printf.printf "Part1> %d\n" (find_start_of_packet line)
let () = Printf.printf "Part1> %d\n" (find_start_of_message line)
