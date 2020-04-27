(** This file is automatically generated by a target in the build system.
    Do not modify it by hand.  *)

open! Core

let module_name = __MODULE__
let padding = ' '
let _ = padding

type ('ty, -'rw) t = ('rw, Iobuf.no_seek) Iobuf.t constraint 'rw = [> read ]
type ('ty, 'rw) message = ('ty, 'rw) t
type ('ty, 'rw) t_no_exn = ('ty, 'rw) t

module R = struct
  type 'message t =
    | Need_more_data
    | Ok of 'message * int
    | Junk of exn * (int, exn) Result.t
  [@@deriving sexp_of]
end

module Message_type_and_errors = struct
  type _ t =
    | New_single : [ `New_single ] t
    | New_group : [ `New_group ] t
    | New_group_point : [ `New_group_point ] t
    | End_of_header : [ `End_of_header ] t
    | Epoch : [ `Epoch ] t
    | Need_more_data : [ `Error ] t
    | Invalid_message_type_or_subtype : [ `Error ] t
    | Message_length_too_short : [ `Error ] t
  [@@deriving sexp_of]

  type packed = T : _ t -> packed [@@deriving sexp_of] [@@unboxed]

  let to_wire_exn : type ty. ty t -> char = function
    | Epoch -> 'E'
    | New_single -> 'N'
    | New_group_point -> 'O'
    | New_group -> 'P'
    | End_of_header -> 'Z'
    | Need_more_data -> invalid_arg "to_wire_exn: received Need_more_data"
    | Invalid_message_type_or_subtype ->
      invalid_arg "to_wire_exn: received Invalid_message_type_or_subtype"
    | Message_length_too_short ->
      invalid_arg "to_wire_exn: received Message_length_too_short"
  ;;

  let of_wire = function
    | 'E' -> T Epoch
    | 'N' -> T New_single
    | 'O' -> T New_group_point
    | 'P' -> T New_group
    | 'Z' -> T End_of_header
    | _ -> T Invalid_message_type_or_subtype
  ;;

  let to_index_exn : type ty. ty t -> int = function
    | New_single -> 0
    | New_group -> 1
    | New_group_point -> 2
    | End_of_header -> 3
    | Epoch -> 4
    | Need_more_data -> invalid_arg "to_index_exn: received Need_more_data"
    | Invalid_message_type_or_subtype ->
      invalid_arg "to_index_exn: received Invalid_message_type_or_subtype"
    | Message_length_too_short ->
      invalid_arg "to_index_exn: received Message_length_too_short"
  ;;

  let of_index_exn = function
    | 0 -> T New_single
    | 1 -> T New_group
    | 2 -> T New_group_point
    | 3 -> T End_of_header
    | 4 -> T Epoch
    | _ -> invalid_arg "of_index_exn: invalid index"
  ;;

  let max_index = 4
  let all_of_packed = List.init (max_index + 1) ~f:of_index_exn

  module Packed = struct
    module T = struct
      type 'ty message_type_and_errors = 'ty t [@@deriving sexp_of]

      type t = packed = T : _ message_type_and_errors -> t
      [@@deriving sexp_of] [@@unboxed]

      let to_index_exn (T t) = to_index_exn t
      let compare t1 t2 = Int.compare (to_index_exn t1) (to_index_exn t2)
      let hash t = Int.hash (to_index_exn t)
      let hash_fold_t state t = Int.hash_fold_t state (to_index_exn t)
      let t_of_sexp _ = failwith "unimplemented"
      let all = all_of_packed
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)
  end
end

let get_message_type buf =
  let len = Iobuf.length buf in
  let pos = 0 in
  if len < 2
  then Message_type_and_errors.(T Need_more_data)
  else (
    let message_num_bytes = 0 + Iobuf.Unsafe.Peek.uint8 buf ~pos in
    if len < message_num_bytes
    then Message_type_and_errors.(T Need_more_data)
    else (
      match Iobuf.Unsafe.Peek.char buf ~pos:(pos + 1) with
      | 'E' ->
        if message_num_bytes < 10
        then T Message_length_too_short
        else Message_type_and_errors.(T Epoch)
      | 'N' ->
        if message_num_bytes < 69
        then T Message_length_too_short
        else Message_type_and_errors.(T New_single)
      | 'O' ->
        if message_num_bytes < 72
        || message_num_bytes
           <
           let pos = 0 in
           72 + (2 * Iobuf.Unsafe.Peek.uint16_le buf ~pos:(pos + 70))
        then T Message_length_too_short
        else Message_type_and_errors.(T New_group_point)
      | 'P' ->
        if message_num_bytes < 69
        then T Message_length_too_short
        else Message_type_and_errors.(T New_group)
      | 'Z' ->
        if message_num_bytes < 2
        then T Message_length_too_short
        else Message_type_and_errors.(T End_of_header)
      | other ->
        ignore other;
        Message_type_and_errors.(T Invalid_message_type_or_subtype)))
;;

let of_iobuf buf ~trusted:_ = Iobuf.no_seek buf

let of_iobuf_exn buf ty =
  let (Message_type_and_errors.T mt) = get_message_type buf in
  if Message_type_and_errors.to_index_exn mt = Message_type_and_errors.to_index_exn ty
  then of_iobuf buf ~trusted:ty
  else
    failwiths
      ~here:[%here]
      "unexpected message type"
      mt
      [%sexp_of: _ Message_type_and_errors.t]
;;

module New_single = struct
  type phantom = [ `New_single ]
  type nonrec -'rw t = (phantom, 'rw) t

  let message_type = 'N'
  let buffer_length = 69
  let of_iobuf_exn buf = of_iobuf_exn buf Message_type_and_errors.New_single

  let write ~id ~spec ~name buf =
    let pos = 0 in
    assert (Iobuf.length buf >= 69);
    Iobuf.Unsafe.Poke.char buf ~pos:(pos + 1) message_type;
    Iobuf.Unsafe.Poke.uint16_le_trunc buf ~pos:(pos + 2) (Probe_id.to_int_exn id);
    Iobuf.Unsafe.Poke.char buf ~pos:(pos + 4) (Probe_type.to_char spec);
    Iobuf.Unsafe.Poke.tail_padded_fixed_string ~padding buf ~len:64 ~pos:(pos + 5) name;
    let total_bytes_packed = 69 in
    Iobuf.Unsafe.Poke.uint8_trunc buf ~pos (total_bytes_packed - 0);
    total_bytes_packed
  ;;

  let create ~id ~spec ~name =
    let iobuf = Iobuf.create ~len:69 in
    let size = write ~id ~spec ~name iobuf in
    assert (Iobuf.length iobuf = size);
    iobuf
  ;;

  let get_message_length buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.uint8 buf ~pos
  ;;

  let get_message_type buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.char buf ~pos:(pos + 1)
  ;;

  let get_id buf =
    let pos = 0 in
    Probe_id.of_int_exn (Iobuf.Unsafe.Peek.uint16_le buf ~pos:(pos + 2))
  ;;

  let get_spec buf =
    let pos = 0 in
    Probe_type.of_char (Iobuf.Unsafe.Peek.char buf ~pos:(pos + 4))
  ;;

  let name_max_len = 64

  let get_name buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.tail_padded_fixed_string ~padding buf ~len:64 ~pos:(pos + 5)
  ;;

  let get_name_zero buf f =
    let buf = Iobuf.read_only buf in
    let pos = 5 in
    let len = ref 64 in
    while
      !len > 0 && Char.( = ) padding (Iobuf.Unsafe.Peek.char buf ~pos:(pos + !len - 1))
    do
      decr len
    done;
    f buf ~safe_pos:pos ~safe_len:!len
  ;;

  let get_name_zero_padded buf f =
    let buf = Iobuf.read_only buf in
    let pos = 5 in
    f buf ~safe_pos:pos ~safe_len:64
  ;;

  let set_id buf field =
    let pos = 0 in
    Iobuf.Poke.uint16_le_trunc buf ~pos:(pos + 2) (Probe_id.to_int_exn field)
  ;;

  let set_spec buf field =
    let pos = 0 in
    Iobuf.Poke.char buf ~pos:(pos + 4) (Probe_type.to_char field)
  ;;

  let set_name buf field =
    let pos = 0 in
    Iobuf.Poke.tail_padded_fixed_string ~padding buf ~len:64 ~pos:(pos + 5) field
  ;;

  let set_name_zero buf f a =
    let lo = Iobuf.Lo_bound.window buf in
    let hi = Iobuf.Hi_bound.window buf in
    Iobuf.advance buf 5;
    Iobuf.resize buf ~len:64;
    f a buf;
    while not (Iobuf.is_empty buf) do
      Iobuf.Unsafe.Fill.char buf padding
    done;
    Iobuf.Lo_bound.restore lo buf;
    Iobuf.Hi_bound.restore hi buf
  ;;

  let to_sub_iobuf t = Iobuf.sub_shared t ~len:(get_message_length t + 0)

  module Unpacked = struct
    type t =
      { message_length : int
      ; message_type : char
      ; id : Probe_id.t
      ; spec : Probe_type.t
      ; name : string
      }
    [@@deriving sexp]

    let num_bytes t = t.message_length + 0

    let write (t : t) iobuf =
      let res = write iobuf ~id:t.id ~spec:t.spec ~name:t.name in
      res
    ;;
  end

  let to_unpacked buf : Unpacked.t =
    { message_length = get_message_length buf
    ; message_type = get_message_type buf
    ; id = get_id buf
    ; spec = get_spec buf
    ; name = get_name buf
    }
  ;;

  let sexp_of_t _ t = Unpacked.sexp_of_t (to_unpacked t)

  let of_unpacked (unpacked : Unpacked.t) =
    let t = Iobuf.create ~len:69 in
    ignore (Unpacked.write unpacked t : int);
    Iobuf.of_string (Iobuf.to_string t)
  ;;

  let t_of_sexp _ sexp = of_unpacked (Unpacked.t_of_sexp sexp)
end

module New_group = struct
  type phantom = [ `New_group ]
  type nonrec -'rw t = (phantom, 'rw) t

  let message_type = 'P'
  let buffer_length = 69
  let of_iobuf_exn buf = of_iobuf_exn buf Message_type_and_errors.New_group

  let write ~id ~spec ~name buf =
    let pos = 0 in
    assert (Iobuf.length buf >= 69);
    Iobuf.Unsafe.Poke.char buf ~pos:(pos + 1) message_type;
    Iobuf.Unsafe.Poke.uint16_le_trunc buf ~pos:(pos + 2) (Probe_id.to_int_exn id);
    Iobuf.Unsafe.Poke.char buf ~pos:(pos + 4) (Probe_type.to_char spec);
    Iobuf.Unsafe.Poke.tail_padded_fixed_string ~padding buf ~len:64 ~pos:(pos + 5) name;
    let total_bytes_packed = 69 in
    Iobuf.Unsafe.Poke.uint8_trunc buf ~pos (total_bytes_packed - 0);
    total_bytes_packed
  ;;

  let create ~id ~spec ~name =
    let iobuf = Iobuf.create ~len:69 in
    let size = write ~id ~spec ~name iobuf in
    assert (Iobuf.length iobuf = size);
    iobuf
  ;;

  let get_message_length buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.uint8 buf ~pos
  ;;

  let get_message_type buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.char buf ~pos:(pos + 1)
  ;;

  let get_id buf =
    let pos = 0 in
    Probe_id.of_int_exn (Iobuf.Unsafe.Peek.uint16_le buf ~pos:(pos + 2))
  ;;

  let get_spec buf =
    let pos = 0 in
    Probe_type.of_char (Iobuf.Unsafe.Peek.char buf ~pos:(pos + 4))
  ;;

  let name_max_len = 64

  let get_name buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.tail_padded_fixed_string ~padding buf ~len:64 ~pos:(pos + 5)
  ;;

  let get_name_zero buf f =
    let buf = Iobuf.read_only buf in
    let pos = 5 in
    let len = ref 64 in
    while
      !len > 0 && Char.( = ) padding (Iobuf.Unsafe.Peek.char buf ~pos:(pos + !len - 1))
    do
      decr len
    done;
    f buf ~safe_pos:pos ~safe_len:!len
  ;;

  let get_name_zero_padded buf f =
    let buf = Iobuf.read_only buf in
    let pos = 5 in
    f buf ~safe_pos:pos ~safe_len:64
  ;;

  let set_id buf field =
    let pos = 0 in
    Iobuf.Poke.uint16_le_trunc buf ~pos:(pos + 2) (Probe_id.to_int_exn field)
  ;;

  let set_spec buf field =
    let pos = 0 in
    Iobuf.Poke.char buf ~pos:(pos + 4) (Probe_type.to_char field)
  ;;

  let set_name buf field =
    let pos = 0 in
    Iobuf.Poke.tail_padded_fixed_string ~padding buf ~len:64 ~pos:(pos + 5) field
  ;;

  let set_name_zero buf f a =
    let lo = Iobuf.Lo_bound.window buf in
    let hi = Iobuf.Hi_bound.window buf in
    Iobuf.advance buf 5;
    Iobuf.resize buf ~len:64;
    f a buf;
    while not (Iobuf.is_empty buf) do
      Iobuf.Unsafe.Fill.char buf padding
    done;
    Iobuf.Lo_bound.restore lo buf;
    Iobuf.Hi_bound.restore hi buf
  ;;

  let to_sub_iobuf t = Iobuf.sub_shared t ~len:(get_message_length t + 0)

  module Unpacked = struct
    type t =
      { message_length : int
      ; message_type : char
      ; id : Probe_id.t
      ; spec : Probe_type.t
      ; name : string
      }
    [@@deriving sexp]

    let num_bytes t = t.message_length + 0

    let write (t : t) iobuf =
      let res = write iobuf ~id:t.id ~spec:t.spec ~name:t.name in
      res
    ;;
  end

  let to_unpacked buf : Unpacked.t =
    { message_length = get_message_length buf
    ; message_type = get_message_type buf
    ; id = get_id buf
    ; spec = get_spec buf
    ; name = get_name buf
    }
  ;;

  let sexp_of_t _ t = Unpacked.sexp_of_t (to_unpacked t)

  let of_unpacked (unpacked : Unpacked.t) =
    let t = Iobuf.create ~len:69 in
    ignore (Unpacked.write unpacked t : int);
    Iobuf.of_string (Iobuf.to_string t)
  ;;

  let t_of_sexp _ sexp = of_unpacked (Unpacked.t_of_sexp sexp)
end

module New_group_point = struct
  type phantom = [ `New_group_point ]
  type nonrec -'rw t = (phantom, 'rw) t

  let message_type = 'O'
  let buffer_length ~sources_count = 72 + (2 * sources_count)
  let of_iobuf_exn buf = of_iobuf_exn buf Message_type_and_errors.New_group_point

  let write ~group_id ~id ~name ~sources_count buf =
    let pos = 0 in
    assert (Iobuf.length buf >= 72 + (2 * sources_count));
    Iobuf.Unsafe.Poke.char buf ~pos:(pos + 1) message_type;
    Iobuf.Unsafe.Poke.uint16_le_trunc buf ~pos:(pos + 2) (Probe_id.to_int_exn group_id);
    Iobuf.Unsafe.Poke.uint16_le_trunc buf ~pos:(pos + 4) (Probe_id.to_int_exn id);
    Iobuf.Unsafe.Poke.tail_padded_fixed_string ~padding buf ~len:64 ~pos:(pos + 6) name;
    Iobuf.Unsafe.Poke.uint16_le_trunc buf ~pos:(pos + 70) sources_count;
    let pos_after_sources = pos + 72 + (sources_count * 2) in
    let total_bytes_packed = pos_after_sources - pos in
    Iobuf.Unsafe.Poke.uint8_trunc buf ~pos (total_bytes_packed - 0);
    total_bytes_packed
  ;;

  let create ~group_id ~id ~name ~sources_count =
    let iobuf = Iobuf.create ~len:(72 + (2 * sources_count)) in
    let size = write ~group_id ~id ~name ~sources_count iobuf in
    assert (Iobuf.length iobuf = size);
    iobuf
  ;;

  let get_message_length buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.uint8 buf ~pos
  ;;

  let get_message_type buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.char buf ~pos:(pos + 1)
  ;;

  let get_group_id buf =
    let pos = 0 in
    Probe_id.of_int_exn (Iobuf.Unsafe.Peek.uint16_le buf ~pos:(pos + 2))
  ;;

  let get_id buf =
    let pos = 0 in
    Probe_id.of_int_exn (Iobuf.Unsafe.Peek.uint16_le buf ~pos:(pos + 4))
  ;;

  let name_max_len = 64

  let get_name buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.tail_padded_fixed_string ~padding buf ~len:64 ~pos:(pos + 6)
  ;;

  let get_name_zero buf f =
    let buf = Iobuf.read_only buf in
    let pos = 6 in
    let len = ref 64 in
    while
      !len > 0 && Char.( = ) padding (Iobuf.Unsafe.Peek.char buf ~pos:(pos + !len - 1))
    do
      decr len
    done;
    f buf ~safe_pos:pos ~safe_len:!len
  ;;

  let get_name_zero_padded buf f =
    let buf = Iobuf.read_only buf in
    let pos = 6 in
    f buf ~safe_pos:pos ~safe_len:64
  ;;

  let get_sources_count buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.uint16_le buf ~pos:(pos + 70)
  ;;

  let get_sources_source_id buf ~count ~index =
    if index < 0 || index >= count then invalid_arg "index out of bounds";
    let pos = 0 in
    let pos_of_source_id_using_count_and_index = pos + 72 + (count * 0) + (index * 2) in
    Probe_id.of_int_exn
      (Iobuf.Unsafe.Peek.uint16_le buf ~pos:pos_of_source_id_using_count_and_index)
  ;;

  let set_group_id buf field =
    let pos = 0 in
    Iobuf.Poke.uint16_le_trunc buf ~pos:(pos + 2) (Probe_id.to_int_exn field)
  ;;

  let set_id buf field =
    let pos = 0 in
    Iobuf.Poke.uint16_le_trunc buf ~pos:(pos + 4) (Probe_id.to_int_exn field)
  ;;

  let set_name buf field =
    let pos = 0 in
    Iobuf.Poke.tail_padded_fixed_string ~padding buf ~len:64 ~pos:(pos + 6) field
  ;;

  let set_name_zero buf f a =
    let lo = Iobuf.Lo_bound.window buf in
    let hi = Iobuf.Hi_bound.window buf in
    Iobuf.advance buf 6;
    Iobuf.resize buf ~len:64;
    f a buf;
    while not (Iobuf.is_empty buf) do
      Iobuf.Unsafe.Fill.char buf padding
    done;
    Iobuf.Lo_bound.restore lo buf;
    Iobuf.Hi_bound.restore hi buf
  ;;

  let set_sources_source_id buf ~count ~index field =
    if index < 0 || index >= count then invalid_arg "index out of bounds";
    let pos = 0 in
    let pos_of_source_id_using_count_and_index = pos + 72 + (count * 0) + (index * 2) in
    Iobuf.Poke.uint16_le_trunc
      buf
      ~pos:pos_of_source_id_using_count_and_index
      (Probe_id.to_int_exn field)
  ;;

  let write_sources buf ~count ~index ~source_id =
    set_sources_source_id buf ~count ~index source_id
  ;;

  let to_sub_iobuf t = Iobuf.sub_shared t ~len:(get_message_length t + 0)

  module Unpacked = struct
    type t_sources = { source_id : Probe_id.t } [@@deriving sexp]

    type t =
      { message_length : int
      ; message_type : char
      ; group_id : Probe_id.t
      ; id : Probe_id.t
      ; name : string
      ; sources_grp : t_sources array
      }
    [@@deriving sexp]

    let num_bytes t = t.message_length + 0

    let write (t : t) iobuf =
      let res =
        write
          iobuf
          ~group_id:t.group_id
          ~id:t.id
          ~name:t.name
          ~sources_count:(Array.length t.sources_grp)
      in
      Array.iteri t.sources_grp ~f:(fun i (record : t_sources) ->
        write_sources
          iobuf
          ~count:(Array.length t.sources_grp)
          ~index:i
          ~source_id:record.source_id);
      res
    ;;
  end

  let to_unpacked buf : Unpacked.t =
    { message_length = get_message_length buf
    ; message_type = get_message_type buf
    ; group_id = get_group_id buf
    ; id = get_id buf
    ; name = get_name buf
    ; sources_grp =
        (let sources_count = get_sources_count buf in
         Array.init sources_count ~f:(fun i ->
           { Unpacked.source_id =
               get_sources_source_id buf ~count:sources_count ~index:i
           }))
    }
  ;;

  let sexp_of_t _ t = Unpacked.sexp_of_t (to_unpacked t)

  let of_unpacked (unpacked : Unpacked.t) =
    let t = Iobuf.create ~len:(72 + (2 * Array.length unpacked.sources_grp)) in
    ignore (Unpacked.write unpacked t : int);
    Iobuf.of_string (Iobuf.to_string t)
  ;;

  let t_of_sexp _ sexp = of_unpacked (Unpacked.t_of_sexp sexp)
end

module End_of_header = struct
  type phantom = [ `End_of_header ]
  type nonrec -'rw t = (phantom, 'rw) t

  let message_type = 'Z'
  let buffer_length = 2
  let of_iobuf_exn buf = of_iobuf_exn buf Message_type_and_errors.End_of_header

  let write buf =
    let pos = 0 in
    assert (Iobuf.length buf >= 2);
    Iobuf.Unsafe.Poke.char buf ~pos:(pos + 1) message_type;
    let total_bytes_packed = 2 in
    Iobuf.Unsafe.Poke.uint8_trunc buf ~pos (total_bytes_packed - 0);
    total_bytes_packed
  ;;

  let create () =
    let iobuf = Iobuf.create ~len:2 in
    let size = write iobuf in
    assert (Iobuf.length iobuf = size);
    iobuf
  ;;

  let get_message_length buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.uint8 buf ~pos
  ;;

  let get_message_type buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.char buf ~pos:(pos + 1)
  ;;

  let to_sub_iobuf t = Iobuf.sub_shared t ~len:(get_message_length t + 0)

  module Unpacked = struct
    type t =
      { message_length : int
      ; message_type : char
      }
    [@@deriving sexp]

    let num_bytes t = t.message_length + 0

    let write (_ : t) iobuf =
      let res = write iobuf in
      res
    ;;
  end

  let to_unpacked buf : Unpacked.t =
    { message_length = get_message_length buf; message_type = get_message_type buf }
  ;;

  let sexp_of_t _ t = Unpacked.sexp_of_t (to_unpacked t)

  let of_unpacked (unpacked : Unpacked.t) =
    let t = Iobuf.create ~len:2 in
    ignore (Unpacked.write unpacked t : int);
    Iobuf.of_string (Iobuf.to_string t)
  ;;

  let t_of_sexp _ sexp = of_unpacked (Unpacked.t_of_sexp sexp)
end

module Epoch = struct
  type phantom = [ `Epoch ]
  type nonrec -'rw t = (phantom, 'rw) t

  let message_type = 'E'
  let buffer_length = 10
  let of_iobuf_exn buf = of_iobuf_exn buf Message_type_and_errors.Epoch

  let write ~epoch buf =
    let pos = 0 in
    assert (Iobuf.length buf >= 10);
    Iobuf.Unsafe.Poke.char buf ~pos:(pos + 1) message_type;
    Iobuf.Unsafe.Poke.int64_le buf ~pos:(pos + 2) (Profiler_epoch.to_int epoch);
    let total_bytes_packed = 10 in
    Iobuf.Unsafe.Poke.uint8_trunc buf ~pos (total_bytes_packed - 0);
    total_bytes_packed
  ;;

  let create ~epoch =
    let iobuf = Iobuf.create ~len:10 in
    let size = write ~epoch iobuf in
    assert (Iobuf.length iobuf = size);
    iobuf
  ;;

  let get_message_length buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.uint8 buf ~pos
  ;;

  let get_message_type buf =
    let pos = 0 in
    Iobuf.Unsafe.Peek.char buf ~pos:(pos + 1)
  ;;

  let get_epoch buf =
    let pos = 0 in
    Profiler_epoch.of_int (Iobuf.Unsafe.Peek.int64_le_exn buf ~pos:(pos + 2))
  ;;

  let set_epoch buf field =
    let pos = 0 in
    Iobuf.Poke.int64_le buf ~pos:(pos + 2) (Profiler_epoch.to_int field)
  ;;

  let to_sub_iobuf t = Iobuf.sub_shared t ~len:(get_message_length t + 0)

  module Unpacked = struct
    type t =
      { message_length : int
      ; message_type : char
      ; epoch : Profiler_epoch.t
      }
    [@@deriving sexp]

    let num_bytes t = t.message_length + 0

    let write (t : t) iobuf =
      let res = write iobuf ~epoch:t.epoch in
      res
    ;;
  end

  let to_unpacked buf : Unpacked.t =
    { message_length = get_message_length buf
    ; message_type = get_message_type buf
    ; epoch = get_epoch buf
    }
  ;;

  let sexp_of_t _ t = Unpacked.sexp_of_t (to_unpacked t)

  let of_unpacked (unpacked : Unpacked.t) =
    let t = Iobuf.create ~len:10 in
    ignore (Unpacked.write unpacked t : int);
    Iobuf.of_string (Iobuf.to_string t)
  ;;

  let t_of_sexp _ sexp = of_unpacked (Unpacked.t_of_sexp sexp)
end

module Unpacked = struct
  type t =
    | New_single of New_single.Unpacked.t
    | New_group of New_group.Unpacked.t
    | New_group_point of New_group_point.Unpacked.t
    | End_of_header of End_of_header.Unpacked.t
    | Epoch of Epoch.Unpacked.t
  [@@deriving sexp]

  let num_bytes = function
    | New_single m -> New_single.Unpacked.num_bytes m
    | New_group m -> New_group.Unpacked.num_bytes m
    | New_group_point m -> New_group_point.Unpacked.num_bytes m
    | End_of_header m -> End_of_header.Unpacked.num_bytes m
    | Epoch m -> Epoch.Unpacked.num_bytes m
  ;;

  let write t iobuf =
    match t with
    | New_single msg -> New_single.Unpacked.write msg iobuf
    | New_group msg -> New_group.Unpacked.write msg iobuf
    | New_group_point msg -> New_group_point.Unpacked.write msg iobuf
    | End_of_header msg -> End_of_header.Unpacked.write msg iobuf
    | Epoch msg -> Epoch.Unpacked.write msg iobuf
  ;;
end

let num_bytes_needed_for_message_length = 1

let num_bytes_in_message buf =
  let pos = 0 in
  if Iobuf.length buf < 1 then failwith "Not enough data to read a message length!";
  0 + Iobuf.Unsafe.Peek.uint8 buf ~pos
;;

let skip_message buf = Iobuf.advance buf (num_bytes_in_message buf)

let buffer_contains_full_message buf =
  let len = Iobuf.length buf in
  len >= num_bytes_needed_for_message_length && len >= num_bytes_in_message buf
;;

let of_unpacked (u : Unpacked.t) =
  match u with
  | New_single msg -> New_single.of_unpacked msg
  | New_group msg -> New_group.of_unpacked msg
  | New_group_point msg -> New_group_point.of_unpacked msg
  | End_of_header msg -> End_of_header.of_unpacked msg
  | Epoch msg -> Epoch.of_unpacked msg
;;

let to_unpacked buf =
  let (Message_type_and_errors.T mt) = get_message_type buf in
  let m = of_iobuf buf ~trusted:mt in
  match mt with
  | Message_type_and_errors.Need_more_data -> R.Need_more_data
  | Message_type_and_errors.Invalid_message_type_or_subtype ->
    let len_or_error = Result.try_with (fun () -> num_bytes_in_message buf) in
    R.Junk (Failure "Invalid_message_type_or_subtype", len_or_error)
  | Message_type_and_errors.Message_length_too_short ->
    let len_or_error = Result.try_with (fun () -> num_bytes_in_message buf) in
    R.Junk (Failure "Message_length_too_short", len_or_error)
  | Message_type_and_errors.New_single ->
    R.Ok (Unpacked.New_single (New_single.to_unpacked m), num_bytes_in_message buf)
  | Message_type_and_errors.New_group ->
    R.Ok (Unpacked.New_group (New_group.to_unpacked m), num_bytes_in_message buf)
  | Message_type_and_errors.New_group_point ->
    R.Ok
      (Unpacked.New_group_point (New_group_point.to_unpacked m), num_bytes_in_message buf)
  | Message_type_and_errors.End_of_header ->
    R.Ok (Unpacked.End_of_header (End_of_header.to_unpacked m), num_bytes_in_message buf)
  | Message_type_and_errors.Epoch ->
    R.Ok (Unpacked.Epoch (Epoch.to_unpacked m), num_bytes_in_message buf)
;;

let to_unpacked_exn buf =
  match to_unpacked buf with
  | Ok (msg, _len) -> msg
  | (Need_more_data | Junk _) as failure ->
    let (Message_type_and_errors.T message_type) = get_message_type buf in
    raise_s
      [%message
        "[to_unpacked_exn] failure"
          (message_type : _ Message_type_and_errors.t)
          (failure : _ R.t)]
;;

let sexp_of_t _ _ t =
  match to_unpacked t with
  | (R.Need_more_data | R.Junk _) as e ->
    failwiths ~here:[%here] "invalid message" e [%sexp_of: Nothing.t R.t]
  | R.Ok (t, _) -> Unpacked.sexp_of_t t
;;

let sexp_of_t_no_exn _ _ t =
  let sexp_of_error error =
    let len =
      try num_bytes_in_message t with
      | _ -> Iobuf.length t
    in
    let len = Int.min (Int.min (Iobuf.length t) len) 1000 in
    let data = Iobuf.to_string t ~len in
    [%sexp ("invalid message" : string), { error : Error.t; data : string }]
  in
  match to_unpacked t with
  | exception exn -> sexp_of_error (Error.of_exn exn)
  | R.Need_more_data -> sexp_of_error (Error.of_string "Need_more_data")
  | R.Junk (exn, _) -> sexp_of_error (Error.of_exn exn)
  | R.Ok (t, _) -> Unpacked.sexp_of_t t
;;

let backing_iobuf t = t
