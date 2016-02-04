open Commands
open Printf
open ISO8601
open Str

let get_file_i f =
  let file_re = regexp "[0-9]\\+\\(-\\([0-9]\\)\\)\\.csv" in
  if (string_match file_re f 0) then
    int_of_string (matched_group 2 f)
  else 0
;;


(* get all the csv files from a path regarding the current date *)
let get_all_csv csv_path date =
  (* create file pattern for the `ls` command *)
  let filesPattern = sprintf "%s*.csv" (Permissive.string_of_date_basic (Unix.gettimeofday ()))
  (* use current directory as copy destination *)
  and dst_dir = Direct(".") in
  (* copy the files from where they are to destination directory *)
  ignore(copy (append_to_path csv_path filesPattern) dst_dir);
  (* return the list of files we got sorted by creation date *)
  List.sort (fun f1 f2 -> Pervasives.compare (get_file_i f1) (get_file_i f2)) (ls dst_dir)

(* create a file listing of the folder corresponding to a folder to rsync *)
and make_file_listing_folder folder actions =
  let get_keys h =
    let keys = ref [] in
    Hashtbl.iter (fun k _ -> keys := k :: !keys) h;
    !keys
  in
  prerr_endline (Printf.sprintf "make listing - %s hash has %d files" folder (Hashtbl.length actions));
  let files_list = get_keys actions in
  prerr_endline (Printf.sprintf "%s has %d files" folder (List.length files_list));
  (* Generate file listing for rsync from one file path *)
  let make_file_listing oc folder file_path =
    output_string oc (file_path ^ "\n")
  in
  let oc = open_out (folder ^ ".list") in
  List.iter (make_file_listing oc folder) files_list
;;

let folder_list = ref []
and files_to_sync = Hashtbl.create 10 (* file list by folder, they will be passed to rsync for synchronisation *)
and rsync_src = ref ""
and rsync_dst = ref ""
and since = ref (Unix.gettimeofday ())
and csv_path = ref (Direct("."))
and excludes = ref []
and dry_run = ref true
and actions_file = ref ""
;;

(* function to gather the files to treat, sorted by folder *)
let push_from_csv csv_file =
  (* Get only the actions we want *)
  let actions = Qnap_actions.parse_csv ~kept_actions:["Write";"Delete";"Rename"] csv_file in
  
  (* This function dispatches actions to the right folder container *)
  let dispatch h folders action =
    (* function that return the folder matched and the action without the folder in its path *)
    let rec find_and_truncate_path action = function
      | [] -> raise Not_found
      | folder::other_folders -> begin
        try
          (* Try to truncate the path with the folder *)
          let n_action = Qnap_actions.headcut_resource_path action folder in
          (folder, n_action)
        with Not_found ->
          (* if the exception Not_found is raised, then folder is not matching *)
          find_and_truncate_path action other_folders
      end
    in
    let push_action_in_folder f action =
      let folder_hash =
        try
          Hashtbl.find h f
        with Not_found -> begin
          let n_h = Hashtbl.create 1000 in
          Hashtbl.add h f n_h;
          n_h
        end
      in
      Qnap_actions.merge_to_action_list action folder_hash
    in
    match folders with
    | [] -> push_action_in_folder "" action
    | folders -> begin
      try
        let (folder, n_action) = find_and_truncate_path action folders in
        push_action_in_folder folder n_action
      with Not_found -> () (* ignoring actions made in folders that are not in the list *)
    end
  in
  (* Apply the function to all the actions *)
  List.iter (dispatch files_to_sync !folder_list) actions
;;

(* Various application parameters' functions *)
let set_since_date s =
  try begin
    since := Permissive.date s
  end with Failure(_) -> begin
    prerr_endline "Please use year-month-day date format";
    exit 1
  end
and set_folders s =
  let separator = regexp "[ \t]*,[ \t]*" in
  folder_list := split separator s
;;


Arg.parse [
  ("--folders", Arg.String(set_folders), "Folders to sync (after the base URL). These are used as a filter at the beginning of the path of the files in the CSV." );
  ("--rsync-src", Arg.Set_string(rsync_src), "rsync's source folder/url." );
  ("--output-actions", Arg.Set_string(actions_file), "write all the actions merged into this file");
  ("--rsync-dst", Arg.Set_string(rsync_dst), "rsync's destination folder/url." );
  ("--path-to-csv", Arg.String(fun s -> csv_path := parse_path s), "where to get the csv files. May be a ssh path.");
  ("--exclude", Arg.String(fun s -> excludes := (regexp s) :: !excludes ), "regexp to exclude from synchronisation.");
  ("--not-dry", Arg.Clear(dry_run), "Prevent rsync to run in dry-run. Do it when you are sure of what you do.");
  (*("--since", Arg.String(set_since_date), "Set the since date (default today only)"); *)
  ] ignore "Usage: daily_sync.ml <csv file> ..."
;;

(* Check if we have enough information to run the program *)
if !rsync_src = "" || !rsync_dst = "" then begin
  prerr_endline "Please provide a source and a destination for rsync";
  exit 1
end;

(* First: create a temporary directory where to work *)
let working_dir = "/tmp/daily_sync-" ^ (Permissive.string_of_datetime_basic (Unix.gettimeofday ()))
and old_wdir = Sys.getcwd () in
Unix.mkdir working_dir 0o700;
Sys.chdir working_dir;

(* Get the csv_files *)
let csv_files = get_all_csv !csv_path !since in
(* Get the files from all the csv_files *)
List.iter (push_from_csv) csv_files;

Hashtbl.iter (fun folder actions -> printf "Got %d files to sync for folder: %s.\n" (Hashtbl.length actions) folder) files_to_sync;
(if !actions_file <> "" then
  let oc = open_out !actions_file in
  let fput_action oc _ action =
    let s = Qnap_actions.string_of_action action
    in output_string oc s; output_char oc '\n'
  in
  let fput_actions oc folder actions =
    let folder_header = Printf.sprintf
"
######################################
##### %26s #####
######################################
" folder in
    output_string oc folder_header;
    Hashtbl.iter (fput_action oc) actions
  in
  Hashtbl.iter (fput_actions oc) files_to_sync
);
Hashtbl.iter make_file_listing_folder files_to_sync;
(* creating path from rsync_src and rsync_dst *)
let rsync_src_path = parse_path !rsync_src
and rsync_dst_path = parse_path !rsync_dst in
Hashtbl.iter (fun folder _ -> rsync ~dry_run:(!dry_run) ~files_from:(folder ^ ".list") (append_to_path rsync_src_path folder) rsync_dst_path) files_to_sync;


(* Ending by cleaning working directory *)
Sys.chdir old_wdir;
(*Sys.command ("rm -fr " ^ working_dir) *)
