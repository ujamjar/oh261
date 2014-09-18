open Ovideo
open Types

module Bitstream(W : Bits.Writer) = struct

  open Table

  let write_start_code bits gobno = 
    W.put bits 16 1;
    W.put bits 4 gobno

  let write_picture_start_code bits = write_start_code bits 0

  let put_bool bits b = W.put bits 1 (if b then 1 else 0)

  let write_picture_header bits pic = 
    let open Picture_header in
    write_picture_start_code bits;
    W.put bits 5 pic.temporal_reference;
    put_bool bits pic.split_screen_indicator;
    put_bool bits pic.document_camera_indicator;
    put_bool bits pic.freeze_picture_release;
    put_bool bits (pic.source_format = Source_format.Cif);
    put_bool bits pic.hi_res;
    put_bool bits pic.spare;
    put_bool bits false (* pei *)

  let write_gob_header bits gob = 
    let open Gob_header in
    write_start_code bits gob.group_number;
    W.put bits 5 gob.gob_quant;
    put_bool bits false

  (* create an array indexed by mba *)
  let mba_table = 
    let open Tables.Mba in
    Array.of_list (List.sort (fun a b -> compare a.data.mba b.data.mba) codes)

  (* 2d array indexed by run and level *)
  let coef_table = 
    let open Tables.Coef in
    (* exclude eob and escape codes *)
    let codes = List.filter (fun c -> not c.data.escape && not c.data.eob) codes in
    (* sort by run *)
    Array.init 27 
      (fun run ->
        (* sort by level *)
        let codes = List.filter (fun c -> c.data.run = run) codes in
        Array.of_list (List.sort 
              (fun a b -> compare a.data.level b.data.level) codes))

  let mvd_table =
    let open Tables.Mvd in
    Array.of_list (List.sort (fun a b -> compare a.data.mvd b.data.mvd) codes)

  let cbp_table = 
    let open Tables.Cbp in
    Array.of_list (List.sort (fun a b -> compare a.data.cbp b.data.cbp) codes)

  (* 0: stuffing code, 1..33: mba *)
  let write_mba bits a =
    let mba = mba_table.(a) in
    W.put bits mba.length mba.code

  let write_coef bits run level = 
    let sign = level < 0 in
    let level = abs level in
    if run <= 26 && level <= Array.length coef_table.(run) then
      let coef = coef_table.(run).(level-1) in
      W.put bits coef.length coef.code;
      W.put bits 1 (if sign then 0 else 1)
    else
      (* escape code *)
      W.put bits 6 1;
      W.put bits 6 run;
      W.put bits 8 (level land 255)

  let write_mvd bits mvd = 
    let mvd = mvd_table.(mvd+16) in
    W.put bits mvd.length mvd.code

  module M = Map.Make(struct
    type t = Mtype.t
    let compare = compare
  end)

  let mtype_map =
    List.fold_left (fun m e -> M.add e.data e m) M.empty Tables.Mtype.codes

  let write_mtype bits d =
    try
      let m = M.find d mtype_map in
      W.put bits m.length m.code
    with _ -> 
      failwith "invalid mtype"

  let write_cbp bits cbp = 
    let cbp = cbp_table.(cbp-1) in
    W.put bits cbp.length cbp.code

end

module Quant = struct

  (* XXX not proper quantizers! XXX *)

  let intra quant d q i = 
    for y=0 to 7 do
      for x=0 to 7 do
        if x=0 && y=0 then begin
          q.{y,x} <- d.{y,x} / 32;
          i.{y,x} <- q.{y,x} * 8
        end else begin
          q.{y,x} <- d.{y,x} / (quant*8);
          if i.{y,x} <> 0 then
            i.{y,x} <- Decode.Quant.inv quant q.{y,x} 
          else
            i.{y,x} <- 0
        end
      done
    done

  let inter quant d q i = 
    for y=0 to 7 do
      for x=0 to 7 do
        q.{y,x} <- d.{y,x} / (quant*8);
        if i.{y,x} <> 0 then
          i.{y,x} <- Decode.Quant.inv quant q.{y,x} 
        else
          i.{y,x} <- 0
      done
    done

end

module Make(W : Bits.Writer)(Me : Motion.Estimator) = struct

  module B = Bitstream(W)

  module Si = Frame.SInt
  module U8 = Frame.U8
  module Sip = Si.Plane
  module U8p = U8.Plane

  module State = struct
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
        block_cur : Sip.t array;
        block_dct : Sip.t array;
        block_qnt : Sip.t array;
        block_iqnt : Sip.t array;
        block_idct : Sip.t array;
      }
    let init bits source_format = 
      let open Picture_header in
      let block () = Array.init 6 (fun _ -> Sip.make 8 8) in
      let prev () = { prev=0; cur=0 } in
      {
        bits = bits; 
        picture_header = { empty with source_format=source_format };
        gob_header = Gob_header.empty;
        ref = alloc_frame empty.source_format;
        cur = alloc_frame empty.source_format;
        inp = alloc_frame empty.source_format;
        
        quant = prev ();
        mvx = prev ();
        mvy = prev ();
        mba = prev ();
        mtype = Tables.Mtype.empty;
        cbp = 0;

        block_cur = block ();
        block_dct = block ();
        block_qnt = block ();
        block_iqnt = block ();
        block_idct = block ();
      }
  end
  open State

  let rle intra block = 
    let incr (x,y) = if x=7 then (0,y+1) else (x+1,y) in
    let rec f run l (x, y) = 
      if x=0 && y=8 then 
        List.rev l
      else if block.{y,x} <> 0 then 
        f 0 ((run,block.{y,x})::l) (incr (x,y))
      else 
        f (run+1) l (incr (x,y))
    in
    f 0 [] (if intra then (1,0) else (0,0))

  let write_mb_header s =
    let open Mtype in
    B.write_mba s.bits (s.mba.cur - s.mba.prev);
    B.write_mtype s.bits s.mtype;
    begin if s.mtype.quant then W.put s.bits 5 s.quant.cur end;
    if s.mtype.mvd then begin
      B.write_mvd s.bits s.mvx.cur; (* XXX differential *)
      B.write_mvd s.bits s.mvy.cur  (* XXX differential *)
    end;
    begin if s.mtype.cbp then B.write_cbp s.bits s.cbp end

  let rec write_block_vlc bits first = function
    | [] -> W.put bits 2 2 (* EOB *)
    | (r,l)::t -> begin
      begin if first && r=0 && (l=1 || l=(-1)) then
        if l<0 then W.put bits 2 3
        else W.put bits 2 2
      else
        B.write_coef bits r l
      end;
      write_block_vlc bits false t
    end

  let get_block s i = 
    let open Frame.U8 in
    match i with 
    | 0 -> s.inp.y, s.cur.y, s.ref.y, 16, 0, 0
    | 1 -> s.inp.y, s.cur.y, s.ref.y, 16, 8, 0
    | 2 -> s.inp.y, s.cur.y, s.ref.y, 16, 0, 8
    | 3 -> s.inp.y, s.cur.y, s.ref.y, 16, 8, 8
    | 4 -> s.inp.u, s.cur.u, s.ref.u,  8, 0, 0
    | _ -> s.inp.v, s.cur.v, s.ref.v,  8, 0, 0

  let encode_intra_mb s = 
    let open Mtype in
    let x, y = mb_to_pos s.gob_header.Gob_header.group_number s.mba.cur in
    (* header *)
    s.mtype.intra <- true; 
    s.mtype.fil <- false;
    s.mtype.quant <- s.quant.prev <> s.quant.cur;
    s.mtype.mvd <- false; 
    s.mtype.cbp <- false; 
    s.mtype.coef <- true;
    write_mb_header s;
    (* encode *)
    for i=0 to 5 do
      let inp, cur, _, bs, ox, oy = get_block s i in
      Sip.blit ~x:(x*bs+ox) ~y:(y*bs+oy) ~w:8 ~h:8 ~dx:0 ~dy:0 inp s.block_cur.(i);
      (* dct and quant *)
      Dct.Chen.fdct s.block_cur.(i) s.block_dct.(i);
      Quant.intra 8 s.block_idct.(i) s.block_qnt.(i) s.block_iqnt.(i);
      Dct.Chen.idct s.block_iqnt.(i) s.block_idct.(i);
      (* vlc *)
      let rle = rle true s.block_qnt.(i) in
      write_block_vlc s.bits false rle;
      (* reconstruct *)
      U8p.blit ~x:0 ~y:0 ~w:8 ~h:8 ~dx:(x*bs+ox) ~dy:(y*bs+oy) s.block_cur.(i) cur;
    done

  let encode_inter_mb s mbno = 
    ()

  let encode_intra_gob s = 
    B.write_gob_header s.bits s.gob_header;
    s.quant.cur <- s.gob_header.Gob_header.gob_quant;
    s.quant.prev <- s.gob_header.Gob_header.gob_quant;
    s.mba.prev <- 0;
    for i=1 to 33 do
      s.mba.cur <- i;
      encode_intra_mb s;
      s.mba.prev <- i;
    done

  let encode_intra_picture s = 
    let open Picture_header in
    let open Gob_header in
    let open Source_format in
    (* write picture header *)
    let lim, step = if s.picture_header.source_format = Qcif then 5,2 else 12,1 in
    B.write_picture_header s.bits s.picture_header;
    (* encode gobs *)
    let rec gobs i = 
      if i > lim then ()
      else begin
        s.gob_header.group_number <- i;
        s.gob_header.gob_quant <- 8;
        encode_intra_gob s;
        gobs (i+step)
      end
    in
    gobs 1;
    (* increment picture no. *)
    s.picture_header.temporal_reference <- s.picture_header.temporal_reference + 1

  let encode_inter_picture s = ()

end

