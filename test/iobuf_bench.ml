open Core.Std
open Core_bench.Std

let chunk_size = 10_000_000

let current_chunk = Iobuf.create ~len:0

let allocate_new_chunk len =
  let new_memory = Iobuf.create ~len in
  Iobuf.set_bounds_and_buffer ~src:new_memory ~dst:current_chunk

let ensure_free len =
  assert (len <= chunk_size);
  if Iobuf.length current_chunk < len then allocate_new_chunk chunk_size

(*
let write_timer_at id time =
  let len = Protocol.Timer_at.buffer_length in
  ensure_free len;
  let written = Protocol.Timer_at.write ~id ~time current_chunk in
  assert (written = len);
  Iobuf.advance current_chunk len

let write_probe_at id time value =
  let len = Protocol.Probe_at.buffer_length in
  ensure_free len;
  let written = Protocol.Probe_at.write ~id ~time ~value current_chunk in
  assert (written = len);
  Iobuf.advance current_chunk len
*)

module Poke = Iobuf.Unsafe.Poke
module Fill = Iobuf.Unsafe.Fill

let write_timer_manual id time =
  let len = 16 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.uint16_le current_chunk ~pos:0 len;
  Poke.uint16_le current_chunk ~pos:2 4;   (* message type *)
  Poke.uint32_le current_chunk ~pos:4 (Core_profiler.Probe_id.to_int_exn id);
  Poke.int64_le  current_chunk ~pos:8 (Time_ns.to_int_ns_since_epoch time);
  Iobuf.advance current_chunk len

let write_timer_manual_fill id time =
  let len = 16 in
  let current_chunk = current_chunk in
  ensure_free len;
  Fill.uint16_le current_chunk len;
  Fill.uint16_le current_chunk 4;   (* message type *)
  Fill.uint32_le current_chunk (Core_profiler.Probe_id.to_int_exn id);
  Fill.int64_le  current_chunk (Time_ns.to_int_ns_since_epoch time)

let write_timer_manual_no_length id time =
  let len = 16 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.uint32_le current_chunk ~pos:0 4;   (* message type *)
  Poke.uint32_le current_chunk ~pos:4 (Core_profiler.Probe_id.to_int_exn id);
  Poke.int64_le  current_chunk ~pos:8 (Time_ns.to_int_ns_since_epoch time);
  Iobuf.advance current_chunk len

let write_probe_manual id time value =
  let len = 24 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.uint16_le current_chunk ~pos:0 len;
  Poke.uint16_le current_chunk ~pos:2 4;   (* message type *)
  Poke.uint32_le current_chunk ~pos:4 (Core_profiler.Probe_id.to_int_exn id);
  Poke.int64_le  current_chunk ~pos:8 (Time_ns.to_int_ns_since_epoch time);
  Poke.int64_le  current_chunk ~pos:16 value;
  Iobuf.advance current_chunk len

let write_probe_manual_no_length id time value =
  let len = 24 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.uint32_le current_chunk ~pos:0 4;   (* message type *)
  Poke.uint32_le current_chunk ~pos:4 (Core_profiler.Probe_id.to_int_exn id);
  Poke.int64_le  current_chunk ~pos:8 (Time_ns.to_int_ns_since_epoch time);
  Poke.int64_le  current_chunk ~pos:16 value;
  Iobuf.advance current_chunk len

let write_timer_manual_64s id time =
  let len = 32 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.int64_le current_chunk ~pos:0 len;
  Poke.int64_le current_chunk ~pos:8 4;   (* message type *)
  Poke.int64_le current_chunk ~pos:16 (Core_profiler.Probe_id.to_int_exn id);
  Poke.int64_le current_chunk ~pos:24 (Time_ns.to_int_ns_since_epoch time);
  Iobuf.advance current_chunk len

let write_probe_manual_64s id time value =
  let len = 40 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.int64_le current_chunk ~pos:0 len;
  Poke.int64_le current_chunk ~pos:8 4;   (* message type *)
  Poke.int64_le current_chunk ~pos:16 (Core_profiler.Probe_id.to_int_exn id);
  Poke.int64_le current_chunk ~pos:24 (Time_ns.to_int_ns_since_epoch time);
  Poke.int64_le current_chunk ~pos:32 value;
  Iobuf.advance current_chunk len

let pack_header_epoch_int = 1405004982655426271
let pack_header_epoch = Time_ns.of_int_ns_since_epoch pack_header_epoch_int

let pack_header_unsafe id time =
  let id = Core_profiler.Probe_id.to_int_exn id in
  let time =
    Time_ns.diff time pack_header_epoch
    |> Time_ns.Span.to_int_ns
  in
  time lor (id lsl 57)

let pack_header_unsafe_intmath id time =
  let id = Core_profiler.Probe_id.to_int_exn id in
  let time = Time_ns.to_int_ns_since_epoch time - pack_header_epoch_int in
  time lor (id lsl 57)

let pack_header id time =
  let id = Core_profiler.Probe_id.to_int_exn id in
  let time =
    Time_ns.diff time pack_header_epoch
    |> Time_ns.Span.to_int_ns
  in
  assert (time >= 0);
  assert (time < 1 lsl 57 - 1);
  assert (id < 32);
  time lor (id lsl 57)

type unpacked_header =
  | Setup_message
  | Short_message of Core_profiler.Probe_id.t * Time_ns.t
with sexp

let unpack_header buf =
  let module Peek = Iobuf.Unsafe.Peek in
  assert (Iobuf.length buf >= 8);

  let msb = Peek.uint8 buf ~pos:7 in
  if msb land 64 <> 0
  then
    Setup_message
  else
    let header = Peek.int64_le buf ~pos:0 in
    let id =
      header lsr 57
      |> Core_profiler.Probe_id.of_int_exn
    in
    let time =
      header land (1 lsl 57 - 1)
      |> Time_ns.Span.of_int_ns
      |> Time_ns.add pack_header_epoch
    in

    (* if debug then printf !"unpacked %i %{Common.Id} %{Time_ns}\n" header id time; *)
    Short_message (id, time)

let pack_header2_epoch = Time_ns.now ()
let pack_header2_time_bits = 55

let pack_header2 id time =
  let id = Core_profiler.Probe_id.to_int_exn id in
  let time =
    Time_ns.diff time pack_header2_epoch
    |> Time_ns.Span.to_int_ns
  in
  assert (time >= 0);
  assert (time < 1 lsl pack_header2_time_bits - 1);
  assert (id < 1 lsl (63 - pack_header2_time_bits));
  time lor (id lsl pack_header2_time_bits)

let pack_header2_unsafe id time =
  let id = Core_profiler.Probe_id.to_int_exn id in
  let time =
    Time_ns.diff time pack_header2_epoch
    |> Time_ns.Span.to_int_ns
  in
  time lor (id lsl pack_header2_time_bits)

let unpack_header2 buf =
  let module Peek = Iobuf.Unsafe.Peek in
  assert (Iobuf.length buf >= 8);

  let header = Peek.int64_le buf ~pos:0 in
  let id =
    header lsr pack_header2_time_bits
    |> Core_profiler.Probe_id.of_int_exn
  in
  let time =
    header land (1 lsl pack_header2_time_bits - 1)
    |> Time_ns.Span.of_int_ns
    |> Time_ns.add pack_header2_epoch
  in
  (id, time)

let write_timer_packed_header id time =
  let len = 8 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.int64_le current_chunk ~pos:0 (pack_header id time);
  Iobuf.advance current_chunk len

let write_timer_packed_header_unsafe id time =
  let len = 8 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.int64_le current_chunk ~pos:0 (pack_header_unsafe id time);
  Iobuf.advance current_chunk len

let write_timer_packed_header_unsafe_intmath id time =
  let len = 8 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.int64_le current_chunk ~pos:0 (pack_header_unsafe_intmath id time);
  Iobuf.advance current_chunk len

let write_timer_packed_header_fill id time =
  let len = 8 in
  let current_chunk = current_chunk in
  ensure_free len;
  Fill.int64_le current_chunk (pack_header id time)

let write_probe_packed_header id time value =
  let len = 16 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.int64_le current_chunk ~pos:0 (pack_header id time);
  Poke.int64_le current_chunk ~pos:8 value;
  Iobuf.advance current_chunk len

let write_timer_packed_header2 id time =
  let len = 8 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.int64_le current_chunk ~pos:0 (pack_header2 id time);
  Iobuf.advance current_chunk len

let write_timer_packed_header_unsafe2 id time =
  let len = 8 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.int64_le current_chunk ~pos:0 (pack_header2_unsafe id time);
  Iobuf.advance current_chunk len

let write_probe_packed_header2 id time value =
  let len = 16 in
  let current_chunk = current_chunk in
  ensure_free len;
  Poke.int64_le current_chunk ~pos:0 (pack_header2 id time);
  Poke.int64_le current_chunk ~pos:8 value;
  Iobuf.advance current_chunk len

let id = Core_profiler.Probe_id.of_int_exn 20
let time = Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_sec 123822.)
let value = 8904095842

let test_packing () =
  List.iter [0; 1; 4; 6; 10; 20; 25; 30; 31] ~f:(fun id ->
    let id = Core_profiler.Probe_id.of_int_exn id in
    let packed_a = pack_header id time in
    let packed_b = pack_header_unsafe id time in
    assert (packed_a = packed_b);

    Iobuf.reset current_chunk;
    write_timer_packed_header id time;
    Iobuf.flip_lo current_chunk;

    let unpacked = unpack_header current_chunk in
    (* unpacked |> sexp_of_unpacked_header |> Sexp.to_string |> print_endline; *)
    assert (unpacked = Short_message (id, time))
  )

let test_packing2 () =
  List.iter [0; 1; 30; 61; 102; 128; 255] ~f:(fun id ->
    let id = Core_profiler.Probe_id.of_int_exn id in
    let packed_a = pack_header2 id time in
    let packed_b = pack_header2_unsafe id time in
    assert (packed_a = packed_b);

    Iobuf.reset current_chunk;
    write_timer_packed_header2 id time;
    Iobuf.flip_lo current_chunk;

    let unpacked = unpack_header2 current_chunk in
    (* unpacked |> sexp_of_unpacked_header |> Sexp.to_string |> print_endline; *)
    assert (unpacked = (id, time))
  )

let command = Bench.make_command
(*  [ Bench.Test.create ~name:"Timer_at   protogen"
      (fun () -> write_timer_at id time) *)
  [ Bench.Test.create ~name:"Timer_at   manual Iobuf, protogen protocol"
      (fun () -> write_timer_manual id time)
  ; Bench.Test.create ~name:"Timer_at   manual Iobuf (Fill instead of Poke), protogen protocol"
      (fun () -> write_timer_manual_fill id time)
  ; Bench.Test.create ~name:"Timer_at   manual Iobuf, protogen protocol without length"
      (fun () -> write_timer_manual_no_length id time)
  ; Bench.Test.create ~name:"Timer_at   manual Iobuf, protogen protocol, all 64bit ints"
      (fun () -> write_timer_manual_64s id time)
  ; Bench.Test.create ~name:"Timer_at   densely packed header"
      (fun () -> write_timer_packed_header id time)
  ; Bench.Test.create ~name:"Timer_at   densely packed header (Fill, not Poke)"
      (fun () -> write_timer_packed_header_fill id time)
  ; Bench.Test.create ~name:"Timer_at   densely packed header (unsafe packing)"
      (fun () -> write_timer_packed_header_unsafe id time)
  ; Bench.Test.create ~name:"Timer_at   densely packed header (unsafe packing; int (not Time_ns) math)"
      (fun () -> write_timer_packed_header_unsafe_intmath id time)
  ; Bench.Test.create ~name:"Timer_at   densely packed header v2"
      (fun () -> write_timer_packed_header2 id time)
  ; Bench.Test.create ~name:"Timer_at   densely packed header v2 (unsafe)"
      (fun () -> write_timer_packed_header_unsafe2 id time)
(*  ; Bench.Test.create ~name:"Probe_at protogen"
      (fun () -> write_probe_at id time value) *)
  ; Bench.Test.create ~name:"Probe_at manual Iobuf, protogen protocol"
      (fun () -> write_probe_manual id time value)
  ; Bench.Test.create ~name:"Probe_at manual Iobuf, protogen protocol without length"
      (fun () -> write_probe_manual_no_length id time value)
  ; Bench.Test.create ~name:"Probe_at manual Iobuf, protogen protocol, all 64bit ints"
      (fun () -> write_probe_manual_64s id time value)
  ; Bench.Test.create ~name:"Probe_at densely packed header"
      (fun () -> write_probe_packed_header id time value)
  ; Bench.Test.create ~name:"Probe_at densely packed header v2"
      (fun () -> write_probe_packed_header2 id time value)
  ]

let () = test_packing ()
let () = test_packing2 ()
let () = Command.run command
