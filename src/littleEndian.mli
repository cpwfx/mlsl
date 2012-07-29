(* File: littleEndian.mli *)

val write_byte : out_channel -> int -> unit

val write_int : out_channel -> int -> unit

val write_int64 : out_channel -> int64 -> unit

val write_short : out_channel -> int -> unit
