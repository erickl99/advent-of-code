open Base

let signal_re = Str.regexp {|\.+|}

let process_file file =
  let rec process_line file =
    match In_channel.input_line file with
    | Some str ->
      Str.split signal_re str |> List.iter ~f:Stdio.print_endline;
      process_line file
    | None -> 0
  in
  process_line file
;;

let matched str =
  ignore (Str.search_forward signal_re str 0);
  Str.matched_string str
;;
