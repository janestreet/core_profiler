open Core.Std
open Core_profiler_disabled

let debug = false

type row =
  | Count_only of int
  | Stats of Fstats.t

let row_stats = function
  | Count_only _ -> None
  | Stats st -> Some st

let row_is_empty = function
  | Count_only n -> n = 0
  | Stats st -> Fstats.samples st = 0

let all_rows : ((unit -> row) * Profiler_units.t) String.Map.t ref = ref String.Map.empty

let columns =
  let fstats_count (_name, _units, row) =
    let count =
      match row with
      | Count_only n -> n
      | Stats st -> Fstats.samples st
    in
    Profiler_units.format_int Profiler_units.Int count
  in
  let fstats_fget getter (_name, units, row) =
    let open Option.Monad_infix in
    row_stats row
    >>| getter
    >>= Float.iround_nearest
    >>| Profiler_units.format_int units
    |> Option.value ~default:""
  in
  Textutils.Ascii_table.(
    [ Column.create ~align:Align.left  "name"  fst3
    ; Column.create ~align:Align.right "count" fstats_count
    ; Column.create ~align:Align.right "sum"   (fstats_fget Fstats.total)
    ; Column.create ~align:Align.right "mean"  (fstats_fget Fstats.mean)
    ; Column.create ~align:Align.right "min"   (fstats_fget Fstats.min)
    ; Column.create ~align:Align.right "max"   (fstats_fget Fstats.max)
    ; Column.create ~align:Align.right "stdev" (fstats_fget Fstats.stdev)
    ]
  )

(* Used for benchmarks, tests etc. *)
let disable_print = ref false

(* Printing configuration *)
let print_enabled =
  let env = "PRINT_ENABLED" in
  let v =
    match Check_environment.get_var env with
    | Some "true" -> true
    | Some "false" -> false
    | Some v ->
      Core.Std.printf "Unknown value for %s for %s, use true or false\n%!" v env;
      true
    | None -> true
  in
  ref v

let print_interval =
  let env = "PRINT_INTERVAL" in
  let v =
    match Check_environment.get_var env with
    | Some v -> Int.of_string v
    | None -> 1
  in
  ref v

let online_print () =
  if not !disable_print then begin
    Core.Std.printf !"%{Time}\n%!" (Time.now ());
    let table =
      Map.fold_right
        !all_rows
        ~init:[]
        ~f:(fun ~key:name ~data:(row_fn, units) acc ->
          let row = row_fn () in
          if row_is_empty row
          then acc
          else (name, units, row) :: acc
        )
    in
    if not (List.is_empty table)
    then begin
      Textutils.Ascii_table.output
        ~oc:Out_channel.stdout
        ~limit_width_to:150
        columns
        table;
      Out_channel.flush Out_channel.stdout
    end
  end

let maybe_print =
  let last_print = ref (Common.now_no_calibrate ()) in
  fun () ->
    if !print_enabled then begin
      let now = Common.now_no_calibrate () in
      let diff =
        Time_ns.diff now !last_print
        |> Time_ns.Span.to_int_sec
      in
      if debug then
        Core.Std.printf "print_interval = %d, diff = %d\n"
          !print_interval diff;
      if diff >= !print_interval then begin
        last_print := now;
        online_print ()
      end
    end

let add_print_to_slow_tasks =
  let once = ref false in
  fun () ->
    if not !once then begin
      Common.add_slow_task Common.Online_profiler maybe_print;
      once := true
    end

let () =
  at_exit online_print

let add_row  : string -> (unit -> row) -> Profiler_units.t -> unit =
  add_print_to_slow_tasks ();
  fun name fn units ->
    all_rows := Map.add !all_rows ~key:name ~data:(fn, units)

let safe_to_delay () =
  Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:1

module Profiler = struct
  let is_enabled = true

  let safe_to_delay = safe_to_delay
  let dump_stats = online_print

  let configure
    ?don't_require_core_profiler_env
    ?offline_profiler_data_file:_
    ?online_print_time_interval_secs
    ?online_print_by_default
    () =
    Option.iter don't_require_core_profiler_env ~f:(fun () ->
      Check_environment.don't_require_core_profiler_env ());
    Option.iter online_print_by_default ~f:(fun bool ->
      print_enabled := bool);
    Option.iter online_print_time_interval_secs ~f:(fun secs ->
      print_interval := secs)
  ;;
end

module Timer = struct
  module Single = struct
    type t =
      { name : string
      ; mutable count : int
      }

    let create name () =
      Check_environment.check_safety_exn ();
      let t = { name; count = 0 } in
      add_row name (fun () -> Count_only t.count) Profiler_units.Int;
      t

    let record t =
      t.count <- t.count + 1;
      Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:3
  end
  module Raw_group = struct
    type t =
      { name : string
      (* [session] is initialised to 0 *)
      ; mutable session : int
      }

    let create ~name = { name; session = 0 }

    let reset group =
      Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:2;
      group.session <- group.session + 1
  end
  module Group_probe = struct
    type t =
      { name : string
      ; group : Raw_group.t
      ; sources : (t * Fstats.t) array
      (* [session] is initialised to -1 *)
      ; mutable session : int
      (* [last_time] is initalised to the epoch; the value won't be used since session
         won't match *)
      ; mutable last_time : Time_ns.t
      }

    let record t =
      let n = Common.now Common.Online_profiler ~reluctance:4 () in
      let gsession = t.group.session in
      for i = 0 to (Array.length t.sources - 1) do
        let (src, stats) = t.sources.(i) in
        if src.session = gsession then
          Time_ns.diff n src.last_time
          |> Time_ns.Span.to_int_ns
          |> float
          |> Fstats.update_in_place stats
      done;
      t.last_time <- n;
      t.session <- gsession


    let create group ~sources ~name =
      Check_environment.check_safety_exn ();
      let probe =
        { name
        ; group
        ; sources = Array.map sources ~f:(fun src -> (src, Fstats.create ()))
        ; last_time = Time_ns.epoch
        ; session = -1
        }
      in
      Array.iter probe.sources ~f:(fun (src, stats) ->
        let row_name = group.name ^ ":" ^ src.name ^ "," ^ name in
        add_row row_name (fun () -> Stats (Fstats.copy stats)) Profiler_units.Nanoseconds
      );
      probe
  end

  type t =
    | Single of Single.t
    | Group_probe of Group_probe.t
  type probe = t

  let create ~name =
    Single (Single.create name ())

  let record = function
    | Single t -> Single.record t
    | Group_probe t -> Group_probe.record t

  module Group = struct
    include Raw_group

    let add_probe t ?(sources=[||]) ~name () =
      let sources =
        Array.map sources ~f:(fun (src : probe) ->
          match src with
          | Single _ -> failwith "Probe sources must come from the same group"
          | Group_probe src ->
            if src.group <> t
            then failwith "Probe sources must come from the same group"
            else src);
      in
      Group_probe  (Group_probe.create t ~sources ~name)
  end
end

BENCH_MODULE "Timer" = struct
  let timer = Timer.create ~name:"bench_timer"

  let group = Timer.Group.create ~name:"bench_timer_group"
  let group_probe0 = Timer.Group.add_probe group ~name:"bench_timer_group_probe0" ()
  let group_probe1 =
    Timer.Group.add_probe group ~name:"bench_timer_group_probe1" ()
      ~sources:[|group_probe0|]
  let group_probe2 =
    Timer.Group.add_probe group ~name:"bench_timer_group_probe2" ()
      ~sources:[|group_probe0; group_probe1|]

  let () =
    Timer.record group_probe0;
    Timer.record group_probe1;
    Timer.record group_probe2

  BENCH "at" = Timer.record timer
  BENCH "group_probe_at (0 sources)" = Timer.record group_probe0
  BENCH "group_probe_at (1 sources)" = Timer.record group_probe1
  BENCH "group_probe_at (2 sources)" = Timer.record group_probe2

  let group2 = Timer.Group.create ~name:"bench_timer_group2"

  BENCH "group_reset" = Timer.Group.reset group2

  let () = disable_print := true
end


module Probe = struct
  (* A probe doesn't need to know its name, so we can save an indirection. *)
  module Single = struct
    type t = Fstats.t

    let create name units =
      Check_environment.check_safety_exn ();
      let t = Fstats.create () in
      add_row name (fun () -> Stats (Fstats.copy t)) units;
      t

    let record t value =
      Fstats.update_in_place t (float value);
      Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:3
  end

  module Raw_group = struct
    type t =
      { name : string
      ; units : Profiler_units.t
        (* [session] is initialised to 0 *)
      ; mutable session : int
      }

    let create ~name ~units = { name; units; session = 0 }

    let reset group =
      Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:2;
      group.session <- group.session + 1
  end

  module Group_probe = struct
    type t =
      { name : string
      ; group : Raw_group.t
      ; sources : (t * Fstats.t) array
      (* See [Timer.Group.Probe.t] *)
      ; mutable session : int
      ; mutable last_value : int
      }

    let create group ~sources ~name =
      Check_environment.check_safety_exn ();
      let probe =
        { name
        ; group
        ; sources = Array.map sources ~f:(fun src -> (src, Fstats.create ()))
        ; last_value = 0
        ; session = -1
        }
      in
      Array.iter probe.sources ~f:(fun (src, stats) ->
        let row_name = group.name ^ ":" ^ src.name ^ "," ^ name in
        add_row row_name (fun () -> Stats (Fstats.copy stats)) group.units
      );
      probe

    let record t value =
      let gsession = t.group.session in
      (* Using Array.iter would cause allocation of a closure *)
      for i = 0 to (Array.length t.sources - 1) do
        let (src, stats) = t.sources.(i) in
        if src.session = gsession then
          Fstats.update_in_place stats (float (value - src.last_value))
      done;
      t.last_value <- value;
      t.session <- gsession;
      Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:4
  end

  type t =
    | Single of Single.t
    | Group_probe of Group_probe.t

  type probe = t

  let create ~name ~units =
    Single (Single.create name units)

  let record t value =
    match t with
    | Single t -> Single.record t value
    | Group_probe t -> Group_probe.record t value

  module Group = struct
    include Raw_group

    let add_probe t ?(sources=[||]) ~name () =
      let sources =
        Array.map sources ~f:(fun (src : probe) ->
          match src with
          | Single _ ->
            failwith "Probe sources must come from the same group"
          | Group_probe src ->
            if src.group <> t
            then failwith "Probe sources must come from the same group"
            else src
        )
      in
      Group_probe (Group_probe.create t ~sources ~name)
  end
end

BENCH_MODULE "Probe" = struct
  let probe = Probe.create ~name:"bench_probe" ~units:Profiler_units.Seconds

  let group = Probe.Group.create ~name:"bench_probe_group" ~units:Profiler_units.Words
  let group_probe0 = Probe.Group.add_probe group ~name:"bench_probe_group_probe0" ()
  let group_probe1 =
    Probe.Group.add_probe group ~name:"bench_probe_group_probe1"
      ~sources:[|group_probe0|] ()
  let group_probe2 =
    Probe.Group.add_probe group ~name:"bench_probe_group_probe2"
      ~sources:[|group_probe0; group_probe1|] ()

  let () =
    Probe.record group_probe0 2;
    Probe.record group_probe1 3;
    Probe.record group_probe2 4

  BENCH "at" = Probe.record probe 10

  BENCH "group_probe_at (0 sources)" = Probe.record group_probe0 5
  BENCH "group_probe_at (1 sources)" = Probe.record group_probe1 6
  BENCH "group_probe_at (2 sources)" = Probe.record group_probe2 7

  let group2 = Probe.Group.create ~name:"bench_probe_group2" ~units:Profiler_units.Int

  BENCH "group_reset" = Probe.Group.reset group2

  let () = disable_print := true
end


module Delta_timer = struct
  type state = Time_ns.t
  type t =
    { name : string
    ; stats : Fstats.t
    ; mutable state : state
    }

  let create ~name =
    let t =
      { name
      ; stats = Fstats.create ()
      ; state = Time_ns.epoch
      }
    in
    add_row name (fun () -> Stats (Fstats.copy t.stats)) Profiler_units.Nanoseconds;
    t

  let stateless_start _ = Common.now Common.Online_profiler ~reluctance:4 ()
  let stateless_stop t state =
    Time_ns.diff (Common.now Common.Online_profiler ~reluctance:2 ()) state
    |> Time_ns.Span.to_int_ns
    |> float
    |> Fstats.update_in_place t.stats

  let start t = t.state <- stateless_start t
  let stop t = stateless_stop t t.state

  let wrap_sync t f x =
    let state = stateless_start t in
    let r =
      try
        f x
      with ex ->
        stateless_stop t state;
        Exn.reraise ex "Core_profiler Delta_timer.wrap_sync"
    in
    stateless_stop t state;
    r

  let wrap_sync2 t f x y =
    let state = stateless_start t in
    let r =
      try
        f x y
      with ex ->
        stateless_stop t state;
        Exn.reraise ex "Core_profiler Delta_timer.wrap_sync2"
    in
    stateless_stop t state;
    r

  let wrap_sync3 t f x y z =
    let state = stateless_start t in
    let r =
      try
        f x y z
      with ex ->
        stateless_stop t state;
        Exn.reraise ex "Core_profiler Delta_timer.wrap_sync3"
    in
    stateless_stop t state;
    r

  let wrap_sync4 t f x y z w =
    let state = stateless_start t in
    let r =
      try
        f x y z w
      with ex ->
        stateless_stop t state;
        Exn.reraise ex "Core_profiler Delta_timer.wrap_sync4"
    in
    stateless_stop t state;
    r

  (* let wrap_async t f x =
   *   let open Async.Std in
   *   let state = start_async t in
   *   try_with ~run:`Now (fun () -> f x) >>= fun res ->
   *   stop_async t state;
   *   match res with
   *   | Ok x -> return x
   *   | Error ex -> Exn.reraise ex "Core_profiler Delta_timer.wrap_async" *)
end

BENCH_MODULE "Delta_timer" = struct
  let delta = Delta_timer.create ~name:"unittest"
  let started = Delta_timer.stateless_start delta

  BENCH "stateless_start" = Delta_timer.stateless_start delta
  BENCH "stateless_stop" = Delta_timer.stateless_stop delta started
  BENCH "start" = Delta_timer.start delta
  BENCH "stop" = Delta_timer.stop delta

  let () = disable_print := true
end

BENCH_MODULE "Delta_timer.wrap_sync" = struct
  let nop () = ()

  let wrapped_nop =
    let delta = Delta_timer.create ~name:"nop" in
    Delta_timer.wrap_sync delta nop

  let count_256 () =
    for _i = 1 to 256 do
      ()
    done

  let wrapped_count_256 =
    let delta = Delta_timer.create ~name:"count_256" in
    Delta_timer.wrap_sync delta count_256

  BENCH "nop" = nop ()
  BENCH "wrapped_nop" = wrapped_nop ()
  BENCH "count_256" = count_256 ()
  BENCH "wrapped_count_256" = wrapped_count_256 ()

  let () = disable_print := true
end


module Delta_probe = struct
  type state = int
  type t =
    { name : string
    ; stats : Fstats.t
    ; mutable state : state
    }

  let create ~name ~units =
    let t =
      { name
      ; stats = Fstats.create ()
      ; state = 0
      }
    in
    add_row name (fun () -> Stats (Fstats.copy t.stats)) units;
    t

  let stateless_start _ value = value
  let stateless_stop  t state value =
    Fstats.update_in_place t.stats (float (value - state));
    Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:3

  let start t v = t.state <- stateless_start t v
  let stop t v = stateless_stop t t.state v
end

BENCH_MODULE "Delta_probe" = struct
  let delta = Delta_probe.create ~name:"unittest" ~units:Profiler_units.Int
  let started = Delta_probe.stateless_start delta 123

  BENCH "start" = Delta_probe.start delta 123
  BENCH "stop" = Delta_probe.stop delta 456
  BENCH "start_async" = Delta_probe.stateless_start delta 123
  BENCH "stop_async" = Delta_probe.stateless_stop delta started 456

  let () = disable_print := true
end
