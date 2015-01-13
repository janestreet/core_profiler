open! Core.Std
open Core_profiler.Std_offline

let p = Probe.create ~name:"array_len" ~units:Profiler_units.Int

let func () =
  let len = 100 in
  let key = Random.int 100 in
  if key mod 3 <> 0 then begin
    ignore (Array.create ~len 10);
    Probe.record p len
  end

let () =
  for _i = 1 to 10_000_000 do
    func ()
  done
