(** Units for the measurements made by [Core_profiler], used to get better output
    formatting.  This has no performance implications. *)

open Core.Std

type t =
  | Words
  | Seconds
  | Nanoseconds
  | Int
[@@deriving sexp, compare]

val to_string  : t -> string
val of_string  : string -> t

val format_int : t -> int -> string
