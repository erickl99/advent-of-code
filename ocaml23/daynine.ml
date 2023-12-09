open Base

let rec compute_value ls =
  let rec process_list = function
    | one :: two :: t -> (two - one) :: process_list (two :: t)
    | _ -> []
  in
  match ls with
  | [ value ] -> -value
  | rest -> compute_value (process_list rest)
;;

let extrapolate_value line =
  let nums =
    String.split_on_chars ~on:[ ' ' ] line |> List.map ~f:Int.of_string
  in
  compute_value (0 ::nums)
;;
