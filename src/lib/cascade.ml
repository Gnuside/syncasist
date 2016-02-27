open Commands
open Printf

exception No_more_files

(* Feeder section *)
let run dry_run excludes rsync_src_path rsync_dst_path file_list_file maxr =
  let file_num = ref 0
  and pids = Hashtbl.create maxr
  in
  if maxr > 1 then begin
    let fic = open_in file_list_file in
    let get_file () =
      (* return a file name where we put one file to sync only *)
      let filelist_name = sprintf "uniq-file-%08d.list" !file_num in
      incr file_num;
      let oc = open_out filelist_name in
      try
        output_string oc (input_line fic);
        output_char oc '\n';
        close_out oc;
        filelist_name
      with End_of_file -> begin
        close_out oc;
        raise No_more_files
      end

    and launch_run_child_process file =
      match Unix.fork () with
      | 0 -> (* child process *) begin
        rsync ~verbose:false ~ignore_errors:true ~itemize_changes:true ~dry_run
          ~files_from:(file) ~excludes
          rsync_src_path rsync_dst_path;
        exit 0
      end
      | pid -> (* parent process *) begin
        prerr_endline (sprintf "launch_run_child_process: pid:%d" pid);
        Hashtbl.add pids pid file
      end

    and wait_and_end_child_process () =
      let (pid,_) = Unix.wait () in
      prerr_endline (sprintf "wait_and_end_child_process: pid:%d" pid);
      let file = Hashtbl.find pids pid in
        Sys.remove file;
        Hashtbl.remove pids pid (* remove it from process launched *)
    in

    try while true do

      if Hashtbl.length pids >= maxr then
        (* Then we wait for one to stop and remove its file list *)
        wait_and_end_child_process ()
      else
        let file = get_file () in
        launch_run_child_process file
    done with No_more_files -> begin
      (* wait for the remaining processes *)
      while Hashtbl.length pids > 0 do
        wait_and_end_child_process ()
      done
    end;
    close_in fic
  end else begin
    rsync ~verbose:false ~ignore_errors:true ~itemize_changes:true ~dry_run
      ~files_from:(file_list_file) ~excludes
      rsync_src_path rsync_dst_path;
  end
;;
