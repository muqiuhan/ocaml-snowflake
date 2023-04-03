(**********************************************************************************)
(* The MIT License (MIT)                                                          *)
(*                                                                                *)
(* Copyright (c) 2023 Muqiu Han                                                   *)
(*                                                                                *)
(* Permission is hereby granted, free of charge, to any person obtaining a copy   *)
(* of this software and associated documentation files (the "Software"), to deal  *)
(* in the Software without restriction, including without limitation the rights   *)
(* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *)
(* copies of the Software, and to permit persons to whom the Software is          *)
(* furnished to do so, subject to the following conditions:                       *)
(*                                                                                *)
(* The above copyright notice and this permission notice shall be included in all *)
(* copies or substantial portions of the Software.                                *)
(*                                                                                *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *)
(* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *)
(* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *)
(* SOFTWARE.                                                                      *)
(**********************************************************************************)

(** Snowflake is an algorithm proposed by Twitter, whose purpose is to generate a 64bit integer:
      1bit: generally the sign bit, not processed
      41bit: used to record timestamp                                                                       
      10bit: used to record the machine ID
      12bit: Cyclic bit, used to generate different IDs for the same millisecond

  * Note: The snowflake algorithm is strongly dependent on time,
      and it is possible to generate duplicate IDs if the time is dialed back.
      If the current time is less than the last time,
      the algorithm will throw an exception if the current time is less than the last time. *)
module Algorithm = struct
  (* Division of 64-bit ID *)
  let worker_id_bits = 5
  and data_center_id_bits = 5
  and sequence_bits = 12

  (* Maximum value calculation *)
  let max_worker_id = -1 lxor (-1 lsl worker_id_bits)
  and max_data_center_id = -1 lxor (-1 lsl data_center_id_bits)

  (* Shift offset calculation *)
  let worker_id_shift = sequence_bits
  and data_center_id_shift = sequence_bits + worker_id_bits
  and timestamp_left_shift = sequence_bits + worker_id_bits + data_center_id_bits

  (* Ordinal cycle mask *)
  let sequence_mask = -1 lxor (-1 lsl sequence_bits)

  (* Twitter first year timestamp *)
  let twepoch = 1288834974657

  class id_worker ~(data_center_id : int) ~(worker_id : int) ~(sequence : int) =
    object (self)
      val data_center_id = data_center_id
      val worker_id = worker_id
      val mutable sequence = sequence
      val mutable last_timestamp = -1.
      method private generate_timestamp () = Unix.gettimeofday () *. 1000.

      method private integrity_check () =
        if worker_id > max_worker_id || worker_id < 0
        then
          raise
            (Invalid_argument
               (Format.sprintf
                  "The reasonable value range of worker_id is: %d > worker_id > 0"
                  max_worker_id))
        else if data_center_id > max_data_center_id || data_center_id < 0
        then
          raise
            (Invalid_argument
               (Format.sprintf
                  "The reasonable value range of data_center_id is: %d > data_center_id \
                   > 0"
                  max_data_center_id))
        else ()

      method private wait_next_millisecond () =
        let timestamp = self#generate_timestamp () |> ref in
        while !timestamp <= last_timestamp do
          timestamp := self#generate_timestamp ()
        done;
        !timestamp

      method private if_time_is_dialed_back (timestamp : float ref) =
        last_timestamp <- !timestamp;
        if !timestamp < last_timestamp
        then
          failwith
            (Format.sprintf
               "Time is dialed back, Rejecting requests until %f"
               last_timestamp)
        else if !timestamp = last_timestamp
        then (
          sequence <- (sequence + 1) land sequence_mask;
          if sequence = 0 then timestamp := self#wait_next_millisecond ())
        else sequence <- 0

      method generate () =
        let timestamp = self#generate_timestamp () |> ref in
        self#if_time_is_dialed_back timestamp;
        self#integrity_check ();
        let timestamp = !timestamp |> Int64.of_float
        and data_center_id = data_center_id |> Int64.of_int
        and worker_id = worker_id |> Int64.of_int
        and twepoch = twepoch |> Int64.of_int
        and sequence = sequence |> Int64.of_int in
        Int64.shift_left (Int64.sub timestamp twepoch) timestamp_left_shift
        |> Int64.logor (Int64.shift_left data_center_id data_center_id_shift)
        |> Int64.logor (Int64.shift_left worker_id worker_id_shift)
        |> Int64.logor sequence
    end
end

let new_worker ~(data_center_id : int) ~(worker_id : int) ~(sequence : int) =
  new Algorithm.id_worker ~data_center_id ~worker_id ~sequence
;;

let equal = Int64.equal
let to_string = Int64.to_string
