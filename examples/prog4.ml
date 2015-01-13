open! Core.Std
open Core_profiler.Std_offline


let g = Probe.Group.create ~name:"func" ~units:Profiler_units.Words
let p1 = Probe.Group.add_probe g ~name:"p1" ()
let p2 = Probe.Group.add_probe g ~name:"p2" ()
let px = Probe.Group.add_probe g ~name:"px" ()

let func () =
  Probe.Group.reset g;
  Probe.record p1 (Gc.minor_words ());
  let len = 100 in
  let key = Random.int 100 in
  if key mod 3 <> 0 then begin
    ignore (Array.create ~len 10);
    Probe.record p2 (Gc.minor_words ());
  end;
  Probe.record px (Gc.minor_words ())

let () =
  for _i = 1 to 10_000_000 do
    func ();
  done
