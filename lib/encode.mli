open Ovideo
open Types

module Bitstream(W : Bits.Writer) : sig
  
  val mba_table : Tables.Mba.t Table.code array
  val coef_table : Tables.Coef.t Table.code array array
  val mvd_table : Tables.Mvd.t Table.code array
  val cbp_table : Tables.Cbp.t Table.code array

  val write_start_code : W.t -> int -> unit
  val write_picture_start_code : W.t -> unit
  val write_picture_header : W.t -> Picture_header.t -> unit
  val write_gob_header : W.t -> Gob_header.t -> unit
  val write_mba : W.t -> int -> unit
  val write_coef : W.t -> int -> int -> unit
  val write_mvd : W.t -> int -> unit
  val write_mtype : W.t -> Mtype.t -> unit
end

module Quant : sig
end

module Make(W : Bits.Writer)(Me : Motion.Estimator) : sig

  module State : sig
    type t = 
      {
        mutable bits : W.t;
        mutable picture_header : Picture_header.t;
        mutable gob_header : Gob_header.t;
        mutable ref : Frame.U8.t;
        mutable cur : Frame.U8.t;
        mutable inp : Frame.U8.t;

        (* macrblock coding parameters *)
        quant : int with_prev;
        mvx : int with_prev;
        mvy : int with_prev;
        mba : int with_prev;
        mutable mtype : Mtype.t;
        mutable cbp : int;
        
        (* various internal block results *)
        block_cur : Frame.SInt.Plane.t array;
        block_dct : Frame.SInt.Plane.t array;
        block_qnt : Frame.SInt.Plane.t array;
        block_iqnt : Frame.SInt.Plane.t array;
        block_idct : Frame.SInt.Plane.t array;
      }
      val init : W.t -> Source_format.t -> t
  end

  val rle : bool -> Frame.SInt.Plane.t -> (int * int) list
  val write_mb_header : State.t -> unit
  val write_block_vlc : W.t -> bool -> (int * int) list -> unit

  val encode_intra_mb : State.t -> unit
  val encode_intra_gob : State.t -> unit
  val encode_intra_picture : State.t -> unit

end

