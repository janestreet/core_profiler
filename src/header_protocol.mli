(** This file is automatically generated by a target in the build system.
    Do not modify it by hand.  *)

open! Core

type (-'hierarchy, -'rw) t constraint 'rw = [> read ]
type ('hierarchy, 'rw) message = ('hierarchy, 'rw) t

val sexp_of_t : _ -> _ -> (_, _) t -> Sexp.t

type ('hierarchy, 'rw) t_no_exn = ('hierarchy, 'rw) t

val sexp_of_t_no_exn : _ -> _ -> (_, _) t_no_exn -> Sexp.t
val to_iobuf : (_, 'rw) t -> ('rw, Iobuf.no_seek) Iobuf.t

module R : sig
  type 'message t =
    | Need_more_data
    | Ok of 'message * int
    | Junk of exn * (int, exn) Result.t
  [@@deriving sexp_of]
end

module Message_type_and_errors : sig
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

  (** [all_of_packed] does not include the error cases. *)
  type packed = T : _ t -> packed [@@deriving sexp_of, enumerate] [@@unboxed]

  module Packed : sig
    (** The hash and compare functions throw on the error cases of
        [message_type_and_errors]. [all] does not include the error cases. *)
    type 'ty message_type_and_errors = 'ty t

    type t = packed = T : _ message_type_and_errors -> t
    [@@deriving sexp_of, enumerate] [@@unboxed]

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end

  val to_wire_exn : _ t -> char
  val of_wire : char -> packed

  (** {to,of}_index_exn provide dense packed integers starting from 0, suitable
      for indexing into an array. *)
  val to_index_exn : _ t -> int

  val of_index_exn : int -> packed
  val max_index : int
end

val get_message_type : ([> read ], _) Iobuf.t -> Message_type_and_errors.packed

(** [of_iobuf] must be fed a message type that comes from a call to
    [get_message_type] on the same window, otherwise it may cause segfaults
    or nonsensical reads. *)
val of_iobuf : ('rw, _) Iobuf.t -> trusted:'ty Message_type_and_errors.t -> ('ty, 'rw) t

val of_iobuf_exn : ('rw, _) Iobuf.t -> 'ty Message_type_and_errors.t -> ('ty, 'rw) t

module New_single : sig
  type phantom = [ `New_single ]
  type nonrec -'rw t = (phantom, 'rw) t constraint 'rw = [> read ] [@@deriving sexp]

  val message_type : char
  val buffer_length : int
  val of_iobuf_exn : ('rw, _) Iobuf.t -> 'rw t

  val write
    :  id:Probe_id.t
    -> spec:Probe_type.t
    -> name:string
    -> (read_write, _) Iobuf.t
    -> int

  val create
    :  id:Probe_id.t
    -> spec:Probe_type.t
    -> name:string
    -> (read_write, Iobuf.seek) Iobuf.t

  val get_message_length : _ t -> int
  val get_message_type : _ t -> char
  val get_id : _ t -> Probe_id.t
  val get_spec : _ t -> Probe_type.t
  val name_max_len : int
  val get_name : _ t -> string

  val get_name_zero
    :  _ t
    -> ((read, Iobuf.no_seek) Iobuf.t -> safe_pos:int -> safe_len:int -> 'a)
    -> 'a

  val get_name_zero_padded
    :  _ t
    -> ((read, Iobuf.no_seek) Iobuf.t -> safe_pos:int -> safe_len:int -> 'a)
    -> 'a

  val set_id : (read_write, _) Iobuf.t -> Probe_id.t -> unit
  val set_spec : (read_write, _) Iobuf.t -> Probe_type.t -> unit
  val set_name : (read_write, _) Iobuf.t -> string -> unit

  (** [set_name_zero buf f a] calls [f] on [buf], with the window adjusted
      to where [name] is. Even though [f] is given a seekable buffer, it
      must move nothing except the lower bound of the window past the data it wrote. *)
  val set_name_zero
    :  (read_write, Iobuf.seek) Iobuf.t
    -> ('a -> (read_write, Iobuf.seek) Iobuf.t -> unit)
    -> 'a
    -> unit

  val to_sub_iobuf : 'rw t -> ('rw, Iobuf.seek) Iobuf.t

  module Unpacked : sig
    type t =
      { message_length : int
      ; message_type : char
      ; id : Probe_id.t
      ; spec : Probe_type.t
      ; name : string
      }
    [@@deriving sexp]

    val num_bytes : t -> int
    val write : t -> (read_write, _) Iobuf.t -> int
  end

  val to_unpacked : 'rw t -> Unpacked.t
  val of_unpacked : Unpacked.t -> 'rw t
end

module New_group : sig
  type phantom = [ `New_group ]
  type nonrec -'rw t = (phantom, 'rw) t constraint 'rw = [> read ] [@@deriving sexp]

  val message_type : char
  val buffer_length : int
  val of_iobuf_exn : ('rw, _) Iobuf.t -> 'rw t

  val write
    :  id:Probe_id.t
    -> spec:Probe_type.t
    -> name:string
    -> (read_write, _) Iobuf.t
    -> int

  val create
    :  id:Probe_id.t
    -> spec:Probe_type.t
    -> name:string
    -> (read_write, Iobuf.seek) Iobuf.t

  val get_message_length : _ t -> int
  val get_message_type : _ t -> char
  val get_id : _ t -> Probe_id.t
  val get_spec : _ t -> Probe_type.t
  val name_max_len : int
  val get_name : _ t -> string

  val get_name_zero
    :  _ t
    -> ((read, Iobuf.no_seek) Iobuf.t -> safe_pos:int -> safe_len:int -> 'a)
    -> 'a

  val get_name_zero_padded
    :  _ t
    -> ((read, Iobuf.no_seek) Iobuf.t -> safe_pos:int -> safe_len:int -> 'a)
    -> 'a

  val set_id : (read_write, _) Iobuf.t -> Probe_id.t -> unit
  val set_spec : (read_write, _) Iobuf.t -> Probe_type.t -> unit
  val set_name : (read_write, _) Iobuf.t -> string -> unit

  (** [set_name_zero buf f a] calls [f] on [buf], with the window adjusted
      to where [name] is. Even though [f] is given a seekable buffer, it
      must move nothing except the lower bound of the window past the data it wrote. *)
  val set_name_zero
    :  (read_write, Iobuf.seek) Iobuf.t
    -> ('a -> (read_write, Iobuf.seek) Iobuf.t -> unit)
    -> 'a
    -> unit

  val to_sub_iobuf : 'rw t -> ('rw, Iobuf.seek) Iobuf.t

  module Unpacked : sig
    type t =
      { message_length : int
      ; message_type : char
      ; id : Probe_id.t
      ; spec : Probe_type.t
      ; name : string
      }
    [@@deriving sexp]

    val num_bytes : t -> int
    val write : t -> (read_write, _) Iobuf.t -> int
  end

  val to_unpacked : 'rw t -> Unpacked.t
  val of_unpacked : Unpacked.t -> 'rw t
end

module New_group_point : sig
  type phantom = [ `New_group_point ]
  type nonrec -'rw t = (phantom, 'rw) t constraint 'rw = [> read ] [@@deriving sexp]

  val message_type : char
  val buffer_length : sources_count:int -> int
  val of_iobuf_exn : ('rw, _) Iobuf.t -> 'rw t

  val write
    :  group_id:Probe_id.t
    -> id:Probe_id.t
    -> name:string
    -> sources_count:int
    -> (read_write, _) Iobuf.t
    -> int

  val create
    :  group_id:Probe_id.t
    -> id:Probe_id.t
    -> name:string
    -> sources_count:int
    -> (read_write, Iobuf.seek) Iobuf.t

  val get_message_length : _ t -> int
  val get_message_type : _ t -> char
  val get_group_id : _ t -> Probe_id.t
  val get_id : _ t -> Probe_id.t
  val name_max_len : int
  val get_name : _ t -> string

  val get_name_zero
    :  _ t
    -> ((read, Iobuf.no_seek) Iobuf.t -> safe_pos:int -> safe_len:int -> 'a)
    -> 'a

  val get_name_zero_padded
    :  _ t
    -> ((read, Iobuf.no_seek) Iobuf.t -> safe_pos:int -> safe_len:int -> 'a)
    -> 'a

  val get_sources_count : _ t -> int

  (** Beware: [count] is trusted. If it is wrong, this function could read the wrong data or segfault. *)
  val get_sources_source_id : 'rw t -> count:int -> index:int -> Probe_id.t

  val set_group_id : (read_write, _) Iobuf.t -> Probe_id.t -> unit
  val set_id : (read_write, _) Iobuf.t -> Probe_id.t -> unit
  val set_name : (read_write, _) Iobuf.t -> string -> unit

  (** [set_name_zero buf f a] calls [f] on [buf], with the window adjusted
      to where [name] is. Even though [f] is given a seekable buffer, it
      must move nothing except the lower bound of the window past the data it wrote. *)
  val set_name_zero
    :  (read_write, Iobuf.seek) Iobuf.t
    -> ('a -> (read_write, Iobuf.seek) Iobuf.t -> unit)
    -> 'a
    -> unit

  (** Beware: [count] is trusted. If it is wrong, this function could read the wrong data. *)
  val set_sources_source_id
    :  (read_write, _) Iobuf.t
    -> count:int
    -> index:int
    -> Probe_id.t
    -> unit

  (** Beware: [count] is trusted. If it is wrong, this function could read the wrong data. *)
  val write_sources
    :  (read_write, _) Iobuf.t
    -> count:int
    -> index:int
    -> source_id:Probe_id.t
    -> unit

  val to_sub_iobuf : 'rw t -> ('rw, Iobuf.seek) Iobuf.t

  module Unpacked : sig
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

    val num_bytes : t -> int
    val write : t -> (read_write, _) Iobuf.t -> int
  end

  val to_unpacked : 'rw t -> Unpacked.t
  val of_unpacked : Unpacked.t -> 'rw t
end

module End_of_header : sig
  type phantom = [ `End_of_header ]
  type nonrec -'rw t = (phantom, 'rw) t constraint 'rw = [> read ] [@@deriving sexp]

  val message_type : char
  val buffer_length : int
  val of_iobuf_exn : ('rw, _) Iobuf.t -> 'rw t
  val write : (read_write, _) Iobuf.t -> int
  val create : unit -> (read_write, Iobuf.seek) Iobuf.t
  val get_message_length : _ t -> int
  val get_message_type : _ t -> char
  val to_sub_iobuf : 'rw t -> ('rw, Iobuf.seek) Iobuf.t

  module Unpacked : sig
    type t =
      { message_length : int
      ; message_type : char
      }
    [@@deriving sexp]

    val num_bytes : t -> int
    val write : t -> (read_write, _) Iobuf.t -> int
  end

  val to_unpacked : 'rw t -> Unpacked.t
  val of_unpacked : Unpacked.t -> 'rw t
end

module Epoch : sig
  type phantom = [ `Epoch ]
  type nonrec -'rw t = (phantom, 'rw) t constraint 'rw = [> read ] [@@deriving sexp]

  val message_type : char
  val buffer_length : int
  val of_iobuf_exn : ('rw, _) Iobuf.t -> 'rw t
  val write : epoch:Profiler_epoch.t -> (read_write, _) Iobuf.t -> int
  val create : epoch:Profiler_epoch.t -> (read_write, Iobuf.seek) Iobuf.t
  val get_message_length : _ t -> int
  val get_message_type : _ t -> char
  val get_epoch : _ t -> Profiler_epoch.t
  val set_epoch : (read_write, _) Iobuf.t -> Profiler_epoch.t -> unit
  val to_sub_iobuf : 'rw t -> ('rw, Iobuf.seek) Iobuf.t

  module Unpacked : sig
    type t =
      { message_length : int
      ; message_type : char
      ; epoch : Profiler_epoch.t
      }
    [@@deriving sexp]

    val num_bytes : t -> int
    val write : t -> (read_write, _) Iobuf.t -> int
  end

  val to_unpacked : 'rw t -> Unpacked.t
  val of_unpacked : Unpacked.t -> 'rw t
end

module Unpacked : sig
  type t =
    | New_single of New_single.Unpacked.t
    | New_group of New_group.Unpacked.t
    | New_group_point of New_group_point.Unpacked.t
    | End_of_header of End_of_header.Unpacked.t
    | Epoch of Epoch.Unpacked.t
  [@@deriving sexp]

  val num_bytes : t -> int
  val write : t -> (read_write, _) Iobuf.t -> int
end

val num_bytes_needed_for_message_length : int

(** Assuming the iobuf starts at a message, returns its length or raise if
    the window doesn't contain [num_bytes_needed_for_message_length] bytes. *)
val num_bytes_in_message : ([> read ], _) Iobuf.t -> int

(** Equivalent to [Iobuf.advance buf (num_bytes_in_message buf)] *)
val skip_message : ([> read ], Iobuf.seek) Iobuf.t -> unit

val buffer_contains_full_message : ([> read ], _) Iobuf.t -> bool
val of_unpacked : Unpacked.t -> (_, _) Iobuf.t
val to_unpacked : ([> read ], _) Iobuf.t -> Unpacked.t R.t
val to_unpacked_exn : ([> read ], _) Iobuf.t -> Unpacked.t
