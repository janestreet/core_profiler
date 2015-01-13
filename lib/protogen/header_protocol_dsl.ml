open Core.Std
open Protogen.Compiler_lib
open Optional_offset
open Mandatory_length

let wrap_int2
      ~module_name
      ~round_trip_value
      ?(of_int_fn="of_int_exn")
      ?(to_int_fn="to_int_exn")
      kind =
  wrap
    ~of_wire_fn:(sprintf "%s.%s" module_name of_int_fn)
    ~to_wire_fn:(sprintf "%s.%s" module_name to_int_fn)
    ~typ:(sprintf "%s.t" module_name)
    ~rand_fn:(sprintf "(function `Min -> %s \
                        | `Max -> %s \
                        | `Random -> %s)"
              round_trip_value round_trip_value round_trip_value)
    kind

let id_kind =
  wrap_int ~module_name:"Probe_id" uint16

let epoch_kind =
  wrap_int2
    ~of_int_fn:"of_int"
    ~to_int_fn:"to_int"
    ~module_name:"Profiler_epoch"
    ~round_trip_value:"Profiler_epoch.of_int 0"
    int63

let spec_field =
  wrap
    ~of_wire_fn:"Probe_type.of_char"
    ~to_wire_fn:"Probe_type.to_char"
    ~typ:"Probe_type.t"
    ~rand_fn:"(function `Min -> Probe_type.Timer
               | `Max -> Probe_type.Timer
               | `Random -> Probe_type.Timer)"
    char

let spec =
  [ message ~length:69 "new_single"
      [ generate_length_and_subtract "message_length" uint8 0
      ; field "message_type" ~constant:'N' char
      ; field "id" id_kind
      ; field "spec" spec_field
      ; field "name" (string 64)
      ]

  ; message ~length:69 "new_group"
      [ generate_length_and_subtract "message_length" uint8 0
      ; field "message_type" ~constant:'P' char
      ; field "id" id_kind
      ; field "spec" spec_field
      ; field "name" (string 64)
      ]

  ; message ~length:72 "new_group_point"
      [ generate_length_and_subtract "message_length" uint8 0
      ; field "message_type" ~constant:'O' char
      ; field "group_id" id_kind
      ; field "id" id_kind
      ; field "name" (string 64)
      ; group "sources" ~length:uint16 [ field "id" id_kind ]
      ]

  ; message ~length:2 "end_of_header"
      [ generate_length_and_subtract "message_length" uint8 0
      ; field "message_type" ~constant:'Z' char
      ]

  ; message ~length:10 "epoch"
      [ generate_length_and_subtract "message_length" uint8 0
      ; field "message_type" ~constant:'E' char
      ; field "epoch" epoch_kind
      ]
  ]

let () =
  Exn.handle_uncaught ~exit:true (fun () ->
    compile_and_output
      ~output_file:"header_protocol.ml"
      ~test_file:"tests_header_protocol.sexp"
      ~add_to_length:0
      ~endianness:`little_endian
      ~backend:`iobuf
      ~string_padding:' '
      ~allow_dependency_on_async:false
      spec
  )
