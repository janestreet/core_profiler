(** This example demonstrates two ways of using Delta_timer's
    Mode 1: start/stop, measures the time between each start and stop
    Mode 2: start/pause/record, measures the total time between starts and pauses
*)
open! Core.Std
open Core_profiler.Std_offline

let array_time = Delta_timer.create ~name:"array_time"
let total_time = Delta_timer.create ~name:"total_time"

let func () =
  let len = 10 in
  let key = Random.int 100 in
  if key mod 3 <> 0 then begin
    Delta_timer.start array_time;
    ignore (Array.create ~len 10);
    Delta_timer.pause array_time
  end

let () =
  Delta_timer.start total_time;
  for _i = 1 to 10_000_000 do
    func ();
    Profiler.safe_to_delay ();
  done;
  Delta_timer.record array_time;
  Delta_timer.stop total_time;
