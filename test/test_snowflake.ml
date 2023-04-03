let _ =
  let worker = Snowflake.new_worker ~data_center_id:1 ~worker_id:2 ~sequence:0 in
  let id_list = List.init 100 (fun _ -> worker#generate ()) in
  List.iter
    (fun id ->
      if 1
         <> (List.filter (fun other_id -> Snowflake.equal other_id id) id_list
             |> List.length)
      then print_endline "Failure")
    id_list;
  print_endline "Success"
;;
