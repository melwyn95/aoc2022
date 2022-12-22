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

let lines = read_input "./inputs/day12.txt"

type cell = Step of char | Start | End

let n_rows = List.length lines
let n_cols = String.length (List.hd lines)
let grid = Array.make_matrix n_rows n_cols End
let start = ref (-1, -1)
let last = ref (-1, -1)

let az = ref []

let () = 
  List.iteri (fun r cols ->
    String.iteri (fun c cell ->
      match cell with
      | 'a' as ch -> 
        let () = az := (r, c) :: !az in
        grid.(r).(c) <- Step ch
      | 'b' .. 'z' as ch -> grid.(r).(c) <- Step ch
      | 'S' -> 
        let () = az := (r, c) :: !az in
        grid.(r).(c) <- let () = start := (r, c) in Start
      | 'E' -> grid.(r).(c) <- let () = last := (r, c) in End
      | _ -> failwith "invalid cell"
    ) cols
  ) lines

let edges = Hashtbl.create 1024

let is_reachable ~curr next =
  match curr, next with
  | Step 'b', Start
  | Step 'a', Start -> true
  | Step n, Step m  -> (Char.code n) <= (Char.code m) + 1
  | End, Step 'z'   
  | End, Step 'y'   -> true
  | _ -> false  

let is_valid i j = i >= 0 && i < n_rows && j >= 0 && j < n_cols

let () = Array.iteri (fun i cols ->
    Array.iteri (fun j curr ->
      let neighbours =
      (if is_valid (i - 1) j && is_reachable ~curr grid.(i - 1).(j) 
      then       [(i - 1, j)] else []) @
      (if is_valid (i + 1) j && is_reachable ~curr grid.(i + 1).(j) 
      then       [(i + 1, j)] else []) @
      (if is_valid i (j - 1) && is_reachable ~curr grid.(i).(j - 1) 
      then      [(i, j - 1)]  else []) @
      (if is_valid i (j + 1) && is_reachable ~curr grid.(i).(j + 1) 
      then      [(i, j + 1)]  else [])       
      in
      (* Printf.printf "(%d,%d) -> " i j;
      List.iter (fun (i, j) -> Printf.printf "(%d,%d) " i j) neighbours;
      Printf.printf "\n"; *)
      Hashtbl.add edges (i, j) neighbours
    ) cols
  ) grid

module TSet = Set.Make(struct 
  type t = int * int
  let compare (a1, b1) (a2, b2) =
    let c = Int.compare a1 a2 in
    if c = 0 then Int.compare b1 b2 else c
end)

let bfs dest (r, c) =
  let q = Queue.create () in
  let () = Queue.add ((r, c), 0) q in
  let v = ref TSet.empty in
  let ans = ref max_int in
  let () = 
  while not @@ Queue.is_empty q do
    let curr, cost = Queue.take q in
    let x, y = curr in
    if grid.(x).(y) = dest then ans := min !ans cost;
    if not @@ TSet.mem curr !v then
      let () = v := TSet.add curr !v in
      let ns = Hashtbl.find edges curr in
      let () = List.iter (fun n -> Queue.add (n, cost + 1) q) ns in
      ()
  done in
  !ans

let () = Printf.printf "==== Day 12 ====\n"

let shortest_path = bfs Start !last
let () = Printf.printf "Part1> %d\n" shortest_path

let shortest_path = bfs (Step 'a') !last
let () = Printf.printf "Part2> %d\n" shortest_path