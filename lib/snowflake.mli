module Algorithm : sig
  class id_worker : object end
end

val new_worker
  :  data_center_id:int
  -> worker_id:int
  -> sequence:int
  -> Algorithm.id_worker

val equal : int64 -> int64 -> bool
val to_string : int64 -> string
