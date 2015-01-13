(* This file is automatically generated by a target in the build system.
   Do not modify it by hand.  *)

open Core.Std
type (-'hierarchy, -'rw) message constraint 'rw = [> read ]
val sexp_of_message : _ -> _ -> (_, _) message -> Sexp.t
module R : sig
  type 'message t =
   | Need_more_data
   | Ok of 'message * int
   | Junk of exn * (int, exn) Result.t
   with sexp_of
end

module New_single : sig
  

  type t_unpacked = {
    message_length : int;
    message_type : char;
    id : Probe_id.t;
    spec : Probe_type.t;
    name : string;
  } with sexp
  type -'rw t = ([ `New_single ], 'rw) message with sexp

  val message_type : char

  val buffer_length : int
  val write :
    id:Probe_id.t -> 
    spec:Probe_type.t -> 
    name:string -> 
    (read_write, _) Iobuf.t ->
    int
  val create :
    id:Probe_id.t -> 
    spec:Probe_type.t -> 
    name:string -> 
    (read_write, Iobuf.seek) Iobuf.t
  val get_message_length : _ t -> int
  val get_message_type : _ t -> char
  val get_id : _ t -> Probe_id.t
  val get_spec : _ t -> Probe_type.t
  val get_name : _ t -> string
  val set_id : (read_write, _) Iobuf.t -> Probe_id.t -> unit
  val set_spec : (read_write, _) Iobuf.t -> Probe_type.t -> unit
  val set_name : (read_write, _) Iobuf.t -> string -> unit
  val to_sub_iobuf : 'rw t -> ('rw, Iobuf.seek) Iobuf.t
  val to_unpacked : 'rw t -> t_unpacked
  val of_unpacked : (read_write, _) Iobuf.t -> t_unpacked -> int
end

module New_group : sig
  

  type t_unpacked = {
    message_length : int;
    message_type : char;
    id : Probe_id.t;
    spec : Probe_type.t;
    name : string;
  } with sexp
  type -'rw t = ([ `New_group ], 'rw) message with sexp

  val message_type : char

  val buffer_length : int
  val write :
    id:Probe_id.t -> 
    spec:Probe_type.t -> 
    name:string -> 
    (read_write, _) Iobuf.t ->
    int
  val create :
    id:Probe_id.t -> 
    spec:Probe_type.t -> 
    name:string -> 
    (read_write, Iobuf.seek) Iobuf.t
  val get_message_length : _ t -> int
  val get_message_type : _ t -> char
  val get_id : _ t -> Probe_id.t
  val get_spec : _ t -> Probe_type.t
  val get_name : _ t -> string
  val set_id : (read_write, _) Iobuf.t -> Probe_id.t -> unit
  val set_spec : (read_write, _) Iobuf.t -> Probe_type.t -> unit
  val set_name : (read_write, _) Iobuf.t -> string -> unit
  val to_sub_iobuf : 'rw t -> ('rw, Iobuf.seek) Iobuf.t
  val to_unpacked : 'rw t -> t_unpacked
  val of_unpacked : (read_write, _) Iobuf.t -> t_unpacked -> int
end

module New_group_point : sig
  

  type t_unpacked = {
    message_length : int;
    message_type : char;
    group_id : Probe_id.t;
    id : Probe_id.t;
    name : string;
    sources_id : Probe_id.t array;
  } with sexp
  type -'rw t = ([ `New_group_point ], 'rw) message with sexp

  val message_type : char

  val buffer_length : 
    sources_count:int -> int
  val write :
    group_id:Probe_id.t -> 
    id:Probe_id.t -> 
    name:string -> 
    sources_count:int -> 
    (read_write, _) Iobuf.t ->
    int
  val create :
    group_id:Probe_id.t -> 
    id:Probe_id.t -> 
    name:string -> 
    sources_count:int -> 
    (read_write, Iobuf.seek) Iobuf.t
  val get_message_length : _ t -> int
  val get_message_type : _ t -> char
  val get_group_id : _ t -> Probe_id.t
  val get_id : _ t -> Probe_id.t
  val get_name : _ t -> string
  val get_sources_count : _ t -> int
  (* Beware: [count] is trusted. If is it wrong, this function could read the wrong data or segfault. *)
  val get_sources_id : 'rw t -> count:int -> index:int -> Probe_id.t
  val set_group_id : (read_write, _) Iobuf.t -> Probe_id.t -> unit
  val set_id : (read_write, _) Iobuf.t -> Probe_id.t -> unit
  val set_name : (read_write, _) Iobuf.t -> string -> unit
  (* Beware: [count] is trusted. If is it wrong, this function could read the wrong data. *)
  val set_sources_id : (read_write, _) Iobuf.t -> count:int -> index:int -> Probe_id.t -> unit
  val to_sub_iobuf : 'rw t -> ('rw, Iobuf.seek) Iobuf.t
  val to_unpacked : 'rw t -> t_unpacked
  val of_unpacked : (read_write, _) Iobuf.t -> t_unpacked -> int
end

module End_of_header : sig
  

  type t_unpacked = {
    message_length : int;
    message_type : char;
  } with sexp
  type -'rw t = ([ `End_of_header ], 'rw) message with sexp

  val message_type : char

  val buffer_length : int
  val write :
    (read_write, _) Iobuf.t ->
    int
  val create :
    (read_write, Iobuf.seek) Iobuf.t
  val get_message_length : _ t -> int
  val get_message_type : _ t -> char
  val to_sub_iobuf : 'rw t -> ('rw, Iobuf.seek) Iobuf.t
  val to_unpacked : 'rw t -> t_unpacked
  val of_unpacked : (read_write, _) Iobuf.t -> t_unpacked -> int
end

module Epoch : sig
  

  type t_unpacked = {
    message_length : int;
    message_type : char;
    epoch : Profiler_epoch.t;
  } with sexp
  type -'rw t = ([ `Epoch ], 'rw) message with sexp

  val message_type : char

  val buffer_length : int
  val write :
    epoch:Profiler_epoch.t -> 
    (read_write, _) Iobuf.t ->
    int
  val create :
    epoch:Profiler_epoch.t -> 
    (read_write, Iobuf.seek) Iobuf.t
  val get_message_length : _ t -> int
  val get_message_type : _ t -> char
  val get_epoch : _ t -> Profiler_epoch.t
  val set_epoch : (read_write, _) Iobuf.t -> Profiler_epoch.t -> unit
  val to_sub_iobuf : 'rw t -> ('rw, Iobuf.seek) Iobuf.t
  val to_unpacked : 'rw t -> t_unpacked
  val of_unpacked : (read_write, _) Iobuf.t -> t_unpacked -> int
end

type t_unpacked =
  | New_single_unpacked of New_single.t_unpacked
  | New_group_unpacked of New_group.t_unpacked
  | New_group_point_unpacked of New_group_point.t_unpacked
  | End_of_header_unpacked of End_of_header.t_unpacked
  | Epoch_unpacked of Epoch.t_unpacked
with sexp

module Message_type_and_errors : sig
  type _ t =
    | New_single : [ `New_single ] t
    | New_group : [ `New_group ] t
    | New_group_point : [ `New_group_point ] t
    | End_of_header : [ `End_of_header ] t
    | Epoch : [ `Epoch ] t
    | Need_more_data : [ `Need_more_data ] t
    | Invalid_message_type_or_subtype : [ `Invalid_message_type_or_subtype ] t
  val sexp_of_t : _ -> _ t -> Sexp.t
  
  type packed = T : _ t -> packed with sexp_of
end

val get_message_type : ([> read ], _) Iobuf.t -> Message_type_and_errors.packed
val get_message : ('rw, _) Iobuf.t -> 'ty Message_type_and_errors.t -> ('ty, 'rw) message

val skip_message : ([> read ], Iobuf.seek) Iobuf.t -> unit
val consuming_dispatch :
  ('rw, Iobuf.seek) Iobuf.t -> 
  on_error:([ `Need_more_data | `Invalid_message_type_or_subtype ] -> 'a) ->
  on_epoch:('rw Epoch.t -> 'a) ->
  on_new_single:('rw New_single.t -> 'a) ->
  on_new_group_point:('rw New_group_point.t -> 'a) ->
  on_new_group:('rw New_group.t -> 'a) ->
  on_end_of_header:('rw End_of_header.t -> 'a) ->
  'a

val consuming_dispatch_with_arg :
  ('rw, Iobuf.seek) Iobuf.t -> 'v0 -> 
  on_error:('v0 -> [ `Need_more_data | `Invalid_message_type_or_subtype ] -> 'a) ->
  on_epoch:('rw Epoch.t -> 'v0 -> 'a) ->
  on_new_single:('rw New_single.t -> 'v0 -> 'a) ->
  on_new_group_point:('rw New_group_point.t -> 'v0 -> 'a) ->
  on_new_group:('rw New_group.t -> 'v0 -> 'a) ->
  on_end_of_header:('rw End_of_header.t -> 'v0 -> 'a) ->
  'a

val consuming_dispatch_with_arg2 :
  ('rw, Iobuf.seek) Iobuf.t -> 'v0 -> 'v1 -> 
  on_error:('v0 -> 'v1 -> [ `Need_more_data | `Invalid_message_type_or_subtype ] -> 'a) ->
  on_epoch:('rw Epoch.t -> 'v0 -> 'v1 -> 'a) ->
  on_new_single:('rw New_single.t -> 'v0 -> 'v1 -> 'a) ->
  on_new_group_point:('rw New_group_point.t -> 'v0 -> 'v1 -> 'a) ->
  on_new_group:('rw New_group.t -> 'v0 -> 'v1 -> 'a) ->
  on_end_of_header:('rw End_of_header.t -> 'v0 -> 'v1 -> 'a) ->
  'a

val of_unpacked : (read_write, Iobuf.seek) Iobuf.t -> t_unpacked -> int
val to_unpacked :
  ([> read ], Iobuf.seek) Iobuf.t
  -> t_unpacked R.t