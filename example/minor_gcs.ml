open! Core.Std
open Core_profiler.Std_online

let () = Profiler.configure ~don't_require_core_profiler_env:() ()


(* Track every time at the end of every major GC. *)
let () =
  let majors = Timer.create ~name:"majGC" in
  ignore (Gc.Expert.Alarm.create (fun () ->
    Timer.record majors)
   : Gc.Expert.Alarm.t)

let g  = Probe.Group.create ~name:"g" ~units:Profiler_units.Words
let gx = Probe.Group.add_probe g ~name:"x" ()
let gy = Probe.Group.add_probe g ~sources:[|gx|] ~name:"y" ()
let gz = Probe.Group.add_probe g ~sources:[|gx; gy|] ~name:"z" ()

let a = Probe.create ~name:"a" ~units:Profiler_units.Words
let b = Probe.create ~name:"b" ~units:Profiler_units.Words

let d = Delta_probe.create ~name:"d" ~units:Profiler_units.Words


(* let some_state = ref Int.Map.empty *)

let func () =
  Probe.Group.reset g;
  Probe.record gx (Gc.minor_words ());
  Probe.record a (Gc.minor_words ());
  Delta_probe.start d (Gc.minor_words ());
  let key = Random.int 100 in
  (* some_state := Map.add !some_state ~key ~data:(); *)
  ignore (Array.create ~len:100 10);
  if key mod 3 <> 0 then begin
    (* some_state := Map.add !some_state ~key ~data:(); *)
    ignore (Array.create ~len:100 10);
    Probe.record gy (Gc.minor_words ());
  end;
  Probe.record gz (Gc.minor_words ());
  Delta_probe.stop d (Gc.minor_words ());
  Probe.record b (Gc.minor_words ())

let () =
  for _i = 1 to 10_000_000 do
    Profiler.safe_to_delay ();
    func ()
  done
