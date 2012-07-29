(* File: littleEndian.ml *)

let write_byte = output_byte

let write_int chan value =
	output_byte chan value;
	output_byte chan (value asr 8);
	output_byte chan (value asr 16);
	output_byte chan (value asr 24)

let write_int64 chan value =
	output_byte chan (Int64.to_int value);
	output_byte chan (Int64.to_int (Int64.shift_right value 8));
	output_byte chan (Int64.to_int (Int64.shift_right value 16));
	output_byte chan (Int64.to_int (Int64.shift_right value 24));
	output_byte chan (Int64.to_int (Int64.shift_right value 32));
	output_byte chan (Int64.to_int (Int64.shift_right value 40));
	output_byte chan (Int64.to_int (Int64.shift_right value 48));
	output_byte chan (Int64.to_int (Int64.shift_right value 56))

let write_short chan value =
	output_byte chan value;
	output_byte chan (value asr 8)
