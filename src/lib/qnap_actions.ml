open Str

type action_desc_t =
  | Delete of string
  | Write of string
  | Rename of string * string
  | Other of string * string

type action_t = {
  id: int;
  kind: string;
  date: string;
  time: string;
  user: string;
  user_ip: string;
  descr: action_desc_t;
}

let column_id                 = 0
and column_kind               = 1
and column_date               = 2
and column_time               = 3
and column_user               = 4
and column_user_ip            = 5
and column_computer_name      = 6
and column_con_type           = 8
and column_accessed_ressource = 7
and column_action             = 9

(* Create a line from CSV Qnap log format*)
let create line =
(*     0   ,   1  ,   2  ,   3  ,   4   ,     5     ,       6       ,        8        ,         7         ,    9    *)
(* "Number","Type","Date","Time","Users","Source IP","Computer name","Connection type","Accessd resources","Action" *)
(* It seems that Connection type and accessed ressource are inverted *)
  let action = List.nth line column_action
  and acc_res = List.nth line column_accessed_ressource
  in
  let descr = (match action with
    | "Write" -> Write(acc_res)
    | "Delete" -> Delete(acc_res)
    | "Rename" -> let acc_res_l = split (regexp "[ \\t]+->[ \\t]+") acc_res in
      Rename(List.nth acc_res_l 0, List.nth acc_res_l 1)
    | _ -> Other(action, acc_res)) in
  {
    id = int_of_string (List.nth line column_id);
    kind = List.nth line column_kind;
    date = List.nth line column_date;
    time = List.nth line column_time;
    user = List.nth line column_user;
    user_ip = List.nth line column_user_ip;
    descr = descr
  }
;;

(* compare function for actions *)
(* it only compares id of the action and allow a chronologic sort *)
let compare a1 a2 = Pervasives.compare a1.id a2.id
;;

(* Parse a csv file and keep only the specified list of actions *)
(* if kept_actions is empty, keeps all the actions *)
(* kept_actions is a capitalized string list of the actions  :
   - Write
   - Delete
   - Rename
   - Read ...
*)
(* return a list of action_t *)
let parse_csv ?(kept_actions=[]) file =

  (* Filter an action regarding it's presence in the kept_actions list *)
  let keep action =
    match kept_actions with
    | [] -> true
    | l -> List.exists (fun a -> a = action) l
  in

  (* add an action to the list if it passes the keep function *)
  let add_to_list loa a =
    let action = List.nth a column_action in
    if keep action then loa := (create a) :: !loa
  in

  (* create in_channel *)
  let ic = open_in file
  (* create accumulator for the actions *)
  and loa = ref [] in
  (* open csv parser's channel *)
  let ic_csv = Csv.of_channel ~has_header:true ic in
  (* add each lines to the accumulator*)
  Csv.iter (add_to_list loa) ic_csv;
  (* close both parser's channels and in_channel *)
  Csv.close_in ic_csv; close_in ic;
  (* Sort action list *)
  List.sort compare !loa
;;

let string_of_descr = function
| Rename(s,d) -> Printf.sprintf "Rename '%s' to '%s'" s d
| Write(ar) -> "Write " ^ ar
| Delete(ar) -> "Delete " ^ ar
| Other(a, p) -> Printf.sprintf "%s %s" a p
;;
let string_of_action a =
  Printf.sprintf "%10d-%s" a.id (string_of_descr a.descr)
;;

let replace_descr a ndescr =
  { id = a.id; kind=a.kind; date=a.date; time=a.time; user=a.user; user_ip=a.user_ip;
  descr=ndescr}
;;

(* function that remove the head of the ressource path *)
(* If the resource path doesn't start with the given cuting_path, then it raises Not_found *)
let headcut_resource_path a cuting_path =
  let cut_re = regexp (Printf.sprintf "^%s/\\(.*\\)$" cuting_path) in
  (* create a new descr record *)
  let n_desc = match a.descr with
  | Rename(s,d) ->
    let ns = if string_match cut_re s 0 then matched_group 1 s
      else raise Not_found
    and nd = if string_match cut_re d 0 then matched_group 1 d
      else raise Not_found
    in Rename(ns,nd)
  | Delete(p) ->
    if string_match cut_re p 0 then Delete(matched_group 1 p)
    else raise Not_found
  | Write(p) ->
    if string_match cut_re p 0 then Write(matched_group 1 p)
    else raise Not_found
  | Other(a,p) ->
    if string_match cut_re p 0 then Other(a,matched_group 1 p)
    else raise Not_found
  in
  (* return a copy of the action *)
  replace_descr a n_desc
;;

let get_acc_res a =
  match a.descr with
  | Rename(_,a) | Other(_,a)-> a
  | Delete(a) | Write(a) -> a
;;

let merge_to_action_list action actions =
  let acc_res = get_acc_res action in
  (*prerr_endline (Printf.sprintf "Merge '%s' to actions(%d)" (string_of_action action) (Hashtbl.length actions));*)
  (match action.descr with
  | Rename(src,_) -> if Hashtbl.mem actions src then Hashtbl.remove actions src
  | _ -> ());
  try
    let entry = Hashtbl.find actions acc_res in
    (match (entry.descr, action.descr) with
    | (_, Delete(_)) -> Hashtbl.remove actions acc_res
    | (_, Rename(_,_)) ->
      Hashtbl.remove actions acc_res;
      Hashtbl.add actions acc_res (replace_descr action (Write(acc_res)))
    | (Write(_), Write(_)) -> ()
    | (Other(a,_),Write(_)) ->
      Hashtbl.remove actions acc_res;
      Hashtbl.add actions acc_res (replace_descr action (Other(a,acc_res)))
    | (Delete(_),Write(_)) ->
      Hashtbl.remove actions acc_res;
      Hashtbl.add actions acc_res action
    | (Rename(_,_),Write(_)) ->
      Hashtbl.remove actions acc_res;
      Hashtbl.add actions acc_res action
    | (Other(a,_), Other(b,_)) ->
      Hashtbl.remove actions acc_res;
      let new_a = Printf.sprintf "%s then %s" a b in
      Hashtbl.add actions acc_res (replace_descr action (Other(new_a,acc_res)))
    | (Rename(_,_),Other(a,_)) ->
      Hashtbl.remove actions acc_res;
      Hashtbl.add actions acc_res action
    | (Delete(_),Other(a,_)) ->
      Hashtbl.remove actions acc_res;
      Hashtbl.add actions acc_res action
    | (Write(_),Other(a,_)) ->
      Hashtbl.remove actions acc_res;
      Hashtbl.add actions acc_res action
    (*| (b,a) -> failwith (
      Printf.sprintf "Unable to merge %s with %s" (string_of_action entry) (string_of_action action)
      )*)
    );
    (*prerr_endline (Printf.sprintf "\t-> MERGED %s" (string_of_action action))*)
  with Not_found -> begin
    (*prerr_endline "\t-> NEW";*)
    Hashtbl.add actions acc_res action
  end
;;
  
