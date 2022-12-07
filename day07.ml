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

let lines = read_input "./inputs/day07.txt"

type log = File of string * int | Dir of string

type cmd = CD of string | LS of log list

let commands =
  let rec parse_logs lines = 
    match lines with
    | [] -> [], []
    | line :: lines when String.starts_with ~prefix:"$" line ->
      [], line :: lines
    | line :: lines ->
      let log =
        if String.starts_with ~prefix:"dir" line
        then
          let dirname = String.sub line 4 (String.length line - 4) in
          Dir dirname 
        else
          match String.split_on_char ' ' line with
          | [size ; file] ->
            let size = int_of_string size in
            File (file, size)
          | _ -> failwith "invalid log"
      in
      let logs, lines = parse_logs lines in
      log :: logs, lines
  in
  let rec aux lines =
    match lines with
    | [] -> []
    | line :: lines ->
      if String.starts_with ~prefix:"$ cd " line
      then
        let dirname = String.sub line 5 (String.length line - 5) in
        CD dirname :: aux lines
      else if String.starts_with ~prefix:"$ ls" line 
      then
        let logs, lines = parse_logs lines in
      LS logs :: aux lines
      else failwith "invalid command"
  in
  aux lines

let string_of_log log =
  match log with
  | File (file, size) -> Printf.sprintf "%d %s" size file
  | Dir dir -> Printf.sprintf "dir %s" dir
let string_of_logs logs = String.concat "\n" @@ List.map string_of_log logs
let print_command cmd =
  match cmd with
  | CD dir -> Printf.printf "$ cd %s\n" dir
  | LS logs -> Printf.printf "$ ls\n%s\n" (string_of_logs logs)
let print_commands cmds = List.iter print_command cmds
(* let () = print_commands commands *)

type parent = Parent of string
type node = File of parent * string * int | Dir of parent * string

let create_nodes_from_logs parent logs =
  List.map (fun (log : log) ->
    match log with
    | File (file, size) -> File (Parent parent, file, size)
    | Dir dir           -> Dir  (Parent parent, dir)
  ) logs

let pwd curr ancestors =
  if curr = "/" then curr else 
  match ancestors with
  | [] -> failwith "invalid ancestors"
  | ["/"] -> "/"
  | parent :: _ -> Printf.sprintf "%s/%s" parent curr

let rec create_nodes_from_cmds ancestors cmds =
  match cmds with
  | []              -> []
  | CD ".." :: cmds -> create_nodes_from_cmds (List.tl ancestors) cmds
  | CD dir  :: cmds -> create_nodes_from_cmds (pwd dir ancestors :: ancestors) cmds
  | LS logs :: cmds -> 
    let nodes = create_nodes_from_logs (List.hd ancestors) logs in
    nodes @ create_nodes_from_cmds ancestors cmds

let nodes = create_nodes_from_cmds ["/"] commands

let print_node node =
  match node with
  | File (Parent p, file, size) -> Printf.printf "%s -> %s %d\n" p file size
  | Dir  (Parent p, dir)        -> Printf.printf "%s -> %s\n" p dir
let print_nodes nodes = List.iter print_node nodes
(* let () = print_nodes nodes *)

type fs = File of string * int | Dir of string * fs list
let rec get_nodes root nodes =
  List.partition (fun (node : node) ->
    match node with
    | File (Parent p, _, _)
    | Dir  (Parent p, _) -> p = root
  ) nodes

let pwd p c = Printf.sprintf "%s/%s" p c 
let rec file_system root nodes =
  let curr, nodes = get_nodes root nodes in
  let nodes, fss =
    List.fold_left (fun (nodes, fss) (node : node) ->
      match node with
      | File (_, file, size) -> nodes, File (file, size) :: fss
      | Dir  (Parent p, dir)        -> 
        let fs, nodes = file_system (pwd p dir) nodes in
        nodes, fs :: fss
    ) (nodes, []) curr
  in
  Dir (root, fss), nodes

let fs, nodes = file_system "/" nodes
let () = assert (nodes = [])

let rec print_fs tabs fs =
  match fs with
  | File (file, size) -> Printf.printf "%s %s %d\n" tabs file size
  | Dir (dir, fss) ->
    Printf.printf "%s %s\n" tabs dir;
    List.iter (print_fs (tabs ^ "\t")) fss
(* let () = print_fs "" fs *)

let rec unzip xs =
  match xs with
  | [] -> [], []
  | (a, b) :: xs ->
    let az, bz = unzip xs in
    a :: az, b :: bz
let sum xs = List.fold_left (+) 0 xs
let rec get_all_dirs_size fs =
  match fs with
  | File (_, s) -> s, []
  | Dir (dir, fss) ->
    let size, dirs = List.fold_left (fun (sofar, dirs) fs -> 
      let size, dirs' = get_all_dirs_size fs in
      size + sofar, dirs' @ dirs
    ) (0, []) fss in
    size, size :: dirs
let size, dirs = get_all_dirs_size fs
let at_most = List.filter (fun size -> size <= 100000) dirs

let dirs = List.rev @@ List.sort Int.compare dirs
let free_space = 70000000 - List.hd dirs
let rec find_dir_to_delete dirs =
  match dirs with
  | [] -> failwith "cant find dir to detele"
  | dir :: dirs ->
    if free_space + dir >= 30000000 then dir
    else find_dir_to_delete dirs

let () = Printf.printf "==== Day 07 ====\n"
let () = Printf.printf "Part1> %d\n" (sum at_most)
let () = Printf.printf "Part2> %d\n" (find_dir_to_delete (List.rev dirs))