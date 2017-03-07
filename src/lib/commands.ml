open Printf
open Str
open Unix

type ssh_t = {
  host: string;
  user: string option;
  path: string;
}
(* Type that describe a path *)
type path_t =
  | Ssh of ssh_t
  | Direct of string

(* convert a path to it's classic form : *)
(* SSH : user@host:path when user is not empty, host:path else *)
(* Local: just as it is *)
let string_of_path ?(ending_slash=false) v =
  let res = (match v with
    | Ssh(s) -> begin
      match s.user with
      | None -> sprintf "%s:%s" s.host s.path
      | Some(user) -> sprintf "%s@%s:%s" user s.host s.path
    end
    | Direct(s) -> s) in
  if ending_slash && ((String.get res ((String.length res) - 1)) <> '/') then
    res ^ "/"
  else res

(** Run given shell command. Get return code & STDOUT *)
and syscall cmd =
    let ic, oc = Unix.open_process cmd in
    let buf = Buffer.create 16 in
    (try
        while true do
            Buffer.add_channel buf ic 1
        done
    with End_of_file -> ());
    let ret_status = Unix.close_process (ic, oc) in
    ( ret_status, Buffer.contents buf)

(* append a string to the path *)
and append_to_path path to_append =
  match path with
  | Direct(s) -> begin Direct(
    if string_match (regexp ".*/$") s 0 then
      s ^ to_append (* end with / *)
    else
      sprintf "%s/%s" s to_append (* no ending /, then we add it *)
  ) end
  | Ssh(s) -> Ssh({ user=s.user ; host=s.host ; path = (
    if string_match (regexp ".*/$") s.path 0 then
      s.path ^ to_append (* end with / *)
    else
      sprintf "%s/%s" s.path to_append (* no ending /, then we add it *)
  )})

(* parse a string path *)
let parse_path p =
  let ssh_re = regexp "^ssh://\\(\\([^@]+\\)@\\)?\\([^:]+\\)\\(:\\(.+\\)\\)?" in
  if string_match ssh_re p 0 then begin
    let user = try Some(matched_group 2 p) with Not_found -> None
    and host = try matched_group 3 p with Not_found -> failwith (sprintf "Unable to parse ssh path (no host found): %s" p)
    and path = try matched_group 5 p with Not_found -> "."
    in Ssh({host= host; user = user; path = path})
  end else Direct(p)

(* Fonction to copy from a path to an other *)
(* not recursive *)
and copy src dst =
  let cmd = sprintf "scp -q %s %s" (string_of_path src) (string_of_path dst) in
  print_endline cmd;
  if Sys.command cmd = 0 then
   true
  else false

(* list files matched by the path *)
and ls path =
  let ic = Unix.open_process_in (match path with
  | Direct(s) -> sprintf "ls %s" s
  | Ssh(s) -> begin
    match s.user with
    | None -> sprintf "ssh %s ls %s" s.host s.path
    | Some(user) -> sprintf "ssh %s@%s ls %s" user s.host s.path
  end)
  and l_file = ref [] in
  begin try while true do
    let file = input_line ic in
    l_file := file :: !l_file
  done
  with End_of_file -> ()
  end;
  List.rev !l_file

(* launch rsync command *)
and rsync_cmd ?(ignore_errors=false) ?delete ?(dry_run=true)
          ?(itemize_changes=false) ?(compress=true)
          ?(info="flist2,progress1") ?files_from
          ?(excludes=[]) ?(verbose=false)
          src dst =
  let src_s = string_of_path ~ending_slash:true src
  and dst_s = string_of_path ~ending_slash:true dst
  and opt = ref ""
  in
  (* adding verbose options *)
  (if verbose then opt := sprintf "%s -vvv" !opt);
  (* adding compress option *)
  (if compress then opt := !opt ^ " -z");
  (* adding excludes options *)
  (if excludes <> [] then
    List.iter (fun e -> opt := sprintf "%s --exclude='%s'" !opt e) excludes);
  (* adding itemize-changes options *)
  (if itemize_changes then opt := sprintf "%s --itemize-changes" !opt);
  (* adding dry_run options *)
  (if dry_run then opt := sprintf "%s --dry-run" !opt);
  (* adding ignore-errors options *)
  (if ignore_errors then opt := sprintf "%s --ignore-errors" !opt);
  (* adding delete options *)
  (match delete with
  | None -> ()
  | Some(`Delete) -> opt := sprintf "%s --delete" !opt
  | Some(`During) -> opt := sprintf "%s --delete-during" !opt
  | Some(`Before) -> opt := sprintf "%s --delete-before" !opt
  | Some(`After) -> opt := sprintf "%s --delete-after" !opt
  );
  (* adding info options *)
  (if info <> "" then opt := sprintf "%s --info=%s" !opt info);
  (* adding files-from option *)
  (match files_from with
    | None -> ()
    | Some(ff) -> opt := sprintf "%s --files-from=\"%s\"" !opt ff);
  let cmd = sprintf "rsync -a -h %s %s %s >> %s.log 2>&1" !opt src_s dst_s (ISO8601.Permissive.string_of_datetime_basic (Unix.gettimeofday ())) in
  cmd
;;
let rsync ?(ignore_errors=false) ?(delete=None) ?(dry_run=true)
          ?(itemize_changes=false) ?(compress=true) 
          ?(info="flist2,progress1") ?files_from
          ?(excludes=[]) ?(verbose=false)
          src dst =
  let cmd = match (delete,files_from) with
    | (None,None) -> rsync_cmd ~compress ~ignore_errors ~dry_run ~itemize_changes ~info ~excludes ~verbose src dst
    | (None,Some(files_from_some)) -> rsync_cmd ~compress ~ignore_errors ~dry_run ~itemize_changes ~info ~files_from:files_from_some ~excludes ~verbose src dst
    | (Some(delete_some),None) -> rsync_cmd ~compress ~ignore_errors ~delete:delete_some ~dry_run ~itemize_changes ~info ~excludes ~verbose src dst
    | (Some(delete_some),Some(files_from_some)) -> rsync_cmd ~compress ~ignore_errors ~delete:delete_some ~dry_run ~itemize_changes ~info ~files_from:files_from_some ~excludes ~verbose src dst
  in
  print_endline cmd;
  ignore (syscall cmd)
;;
let clear_wait () =
  try
    while true do
      ignore (wait ())
    done
  with Unix.Unix_error(Unix.ECHILD, "wait", _) -> ()
;;
