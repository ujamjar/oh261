open Printf
open Ovideo
open Types

#let_default log_level=0

let log_chan = ref stderr
let log s = (output_string !log_chan s; flush !log_chan)

module Bitstream(R : Bits.Reader) = struct

  (* 15 '0's followed by a '1' *)
  let find_start_code bits = 
    let rec f nzeros = 
      let b = R.get bits 1 in
      if b=0 then f (nzeros+1)
      else if nzeros >= 15 then ()
      else f 0
    in
    f 0

  (* start code followed by 4 '0' bits *)
  let rec find_picture_start_code bits = 
    find_start_code bits;
    if R.show bits 4 = 0 then 
      R.advance bits 4
    else begin
      R.advance bits 4;
      find_picture_start_code bits
    end

  (* padding. '1' followed by 8 bits. *)
  let rec read_spare bits = 
    if R.get bits 1 = 1 then begin
      ignore (R.get bits 8);
      read_spare bits
    end

  let get_bool bits = R.get bits 1 = 1

  let read_picture_header bits = 
    let temporal_reference = R.get bits 5 in
    let split_screen_indicator = get_bool bits in
    let document_camera_indicator = get_bool bits in
    let freeze_picture_release = get_bool bits in
    let source_format = if get_bool bits then Source_format.Cif else Source_format.Qcif in
    let hi_res = get_bool bits in
    let spare = get_bool bits in
    let picture_header = 
      Picture_header.({
        temporal_reference; split_screen_indicator; document_camera_indicator;
        freeze_picture_release; source_format; hi_res; spare;
      })
    in
    read_spare bits;
    picture_header

  let read_gob_header bits = 
    find_start_code bits;
    let group_number = R.get bits 4 in
    let gob_quant = R.get bits 5 in
    let gob_header = Gob_header.({ group_number; gob_quant; }) in
    read_spare bits;
    gob_header
 
  (* create the codeword lookup tables *)
  let mba   = "mba", Table.lookup Tables.Mba.codes
  let mtype = "mtype", Table.lookup Tables.Mtype.codes
  let cbp   = "cbp", Table.lookup Tables.Cbp.codes
  let mvd   = "mvd", Table.lookup Tables.Mvd.codes
  let coef  = "coef", Table.lookup Tables.Coef.codes

  exception No_valid_codeword of string * int

  let lookup_code bits (name,(table,max_bits)) = 
    let code = R.show bits max_bits in
    match table.(code) with
    | None -> raise (No_valid_codeword (name, code))
    | Some(l,c) -> begin
#if log_level>2 
      log (sprintf "%5s: %i %s\n" name l (Types.string_of_bits l (code lsr (max_bits - l))));
#endif
      R.advance bits l;
      c
    end

#if log_level>2 
  let lookup_code_coef bits table = 
    let open Tables.Coef in
    let coef = lookup_code bits table in
    log (sprintf "eob=%b escape=%b run=%i level=%i\n" 
      coef.eob coef.escape coef.run coef.level);
    coef
#else
  let lookup_code_coef = lookup_code
#endif

  let intra_dc_coef bits = 
    let dc = R.get bits 8 in
    let dc = if dc = 255 then 128 else dc in
#if log_level>2 
    log (sprintf "intra_dc: %i\n" dc);
#endif
    (false, 0, dc)

  let inter_first_coef bits = 
    R.advance bits 1;
    let sign = if R.get bits 1 = 0 then 1 else -1 in 
#if log_level>2
    log (sprintf "inter_first: %i\n" sign);
#endif
    (false, 0, sign)

  let lookup_coef bits first intra =
    let get_coef() =
      let open Tables.Coef in
      let coef = lookup_code_coef bits coef in
      if coef.eob then
        (true, 0, 0)
      else 
        if coef.escape then
          let run = R.get bits 6 in
          let level = R.get bits 8 in
          let level = if (level land (1 lsl 7)) <> 0 then level lor ((-1) lsl 8) else level in
          (false, run, level)
        else
          let sign = if R.get bits 1 = 0 then 1 else -1 in
          (coef.eob, coef.run, coef.level * sign)
    in
    if first then
      if intra then intra_dc_coef bits
      else
        if R.show bits 1 = 1 then inter_first_coef bits
        else get_coef()
    else
      get_coef()

end

module Quant = struct

  let inv q l = 
    if (q land 1) = 1 then 
      if l > 0 then q * ((2 * l) + 1)
      else q * ((2 * l) - 1)
    else 
      if l > 0 then (q * ((2 * l) + 1)) - 1
      else (q * ((2 * l) - 1)) + 1

end

module Recon = struct

  let clip x = max 0 (min 255 x)

  let copy ~x ~y ~ref ~cur = begin
    let open Frame.U8 in
    Plane.blit ~x ~y ~w:16 ~h:16 ~dx:x ~dy:y ref.y cur.y;
    let x,y = x/2, y/2 in
    Plane.blit ~x ~y ~w:8 ~h:8 ~dx:x ~dy:y ref.u cur.u;
    Plane.blit ~x ~y ~w:8 ~h:8 ~dx:x ~dy:y ref.v cur.v
  end

  let intra ~x ~y ~pred ~cur = 
    for j=0 to 7 do
      for i=0 to 7 do
        cur.{y+j,x+i} <- clip pred.{j,i}
      done
    done

  let skip ~x ~y ~mvx ~mvy ~ref ~cur = 
    for j=0 to 7 do
      for i=0 to 7 do
        cur.{y+j,x+i} <- ref.{y+j+mvy,x+i+mvx}
      done
    done

  let mc ~x ~y ~mvx ~mvy ~ref ~pred ~cur = 
    for j=0 to 7 do
      for i=0 to 7 do
        cur.{y+j,x+i} <- clip (pred.{j,i} + ref.{y+j+mvy,x+i+mvx})
      done
    done

  let fil = 
    let fil1 = Frame.SInt.Plane.make ~w:8 ~h:8 in
    let fil2 = Frame.SInt.Plane.make ~w:8 ~h:8 in
    (fun ~x ~y ~mvx ~mvy ~ref ~pred ~cur ->
      for j=0 to 7 do
        for i=0 to 7 do
          fil1.{j,i} <- ref.{y+j+mvy,x+i+mvx}
        done
      done;
      (* horz filter *)
      for j=0 to 7 do
        fil2.{j,0} <- 4 * fil1.{j,0};
        for i=1 to 6 do
          fil2.{j,i} <- fil1.{j,i-1} + (2 * fil1.{j,i}) + fil1.{j,i+1}
        done;
        fil2.{j,7} <- 4 * fil1.{j,7}
      done;
      (* vert filter *)
      for i=0 to 7 do
        fil1.{0,i} <- 4 * fil2.{0,i};
        for j=1 to 6 do
          fil1.{j,i} <- fil2.{j-1,i} + (2 * fil2.{j,i}) + fil2.{j+1,i}
        done;
        fil1.{7,i} <- 4 * fil2.{7,i}
      done;
      (* round and reconstruct *)
      for j=0 to 7 do
        for i=0 to 7 do
          let r = (fil1.{j,i} + 8) / 16 in
          cur.{y+j,x+i} <- clip (r + pred.{j,i})
        done;
      done)

end

module Make(R : Bits.Reader) = struct

  module B = Bitstream(R)

  module State = struct
    type t = 
      {
        mutable bits : R.t;
        mutable picture_header : Picture_header.t;
        mutable gob_header : Gob_header.t;
        mutable ref : Frame.U8.t;
        mutable cur : Frame.U8.t;

        (* macroblock info *)
        mutable mba : int;
        mutable mbadiff : int;
        mutable mvx : int;
        mutable mvy : int;
        mutable mvdx : int;
        mutable mvdy : int;
        mutable cbp : int;
        mutable quant : int;
        mutable mtype : Mtype.t;
        mutable block_iqnt : Frame.SInt.Plane.t;
        mutable block_idct : Frame.SInt.Plane.t;
      }
    let init bits = 
      let open Picture_header in
      {
        bits = bits;
        picture_header = empty;
        gob_header = Gob_header.empty;
        ref = alloc_frame empty.source_format;
        cur = alloc_frame empty.source_format;
        mba = 0;
        mbadiff = 0;
        mvx = 0;
        mvy = 0;
        mvdx = 0;
        mvdy = 0;
        cbp = 0;
        quant = 0;
        mtype = Tables.Mtype.empty;
        block_iqnt = Frame.SInt.Plane.make ~w:8 ~h:8;
        block_idct = Frame.SInt.Plane.make ~w:8 ~h:8;
      }
  end

  open State

  let read_mb_header s = 
    let open Mtype in
    s.mbadiff <- (B.lookup_code s.bits B.mba).Tables.Mba.mba;
    s.mtype <- B.lookup_code s.bits B.mtype;
    s.quant <- if s.mtype.quant then R.get s.bits 5 else s.quant;
    s.mvdx <- if s.mtype.mvd then (B.lookup_code s.bits B.mvd).Tables.Mvd.mvd else 0;
    s.mvdy <- if s.mtype.mvd then (B.lookup_code s.bits B.mvd).Tables.Mvd.mvd else 0;
    s.cbp <- 
      if s.mtype.cbp then (B.lookup_code s.bits B.cbp).Tables.Cbp.cbp 
      else if s.mtype.intra then 63
      else 0

  let get_mv prev_mv delta_mv = 
    let delta delta_mv = 
      if delta_mv < 0 then 32 - delta_mv
      else -32 + delta_mv
    in
    let mv = prev_mv + delta_mv in
    if mv < -15 || mv > 15 then delta delta_mv + prev_mv
    else mv

  let get_block s i = 
    let open Frame.U8 in
    match i with 
    | 0 -> s.cur.y, s.ref.y, 16, 0, 0, s.mvx  , s.mvy
    | 1 -> s.cur.y, s.ref.y, 16, 8, 0, s.mvx  , s.mvy
    | 2 -> s.cur.y, s.ref.y, 16, 0, 8, s.mvx  , s.mvy
    | 3 -> s.cur.y, s.ref.y, 16, 8, 8, s.mvx  , s.mvy
    | 4 -> s.cur.u, s.ref.u,  8, 0, 0, s.mvx/2, s.mvy/2
    | _ -> s.cur.v, s.ref.v,  8, 0, 0, s.mvx/2, s.mvy/2

  let rec decode_coefs s first pos =
    let open Mtype in
    let eob, run, level = B.lookup_coef s.bits first s.mtype.intra in
    if not eob then begin
      (* skip run coefs *)
      let pos = pos + run in
      assert (pos < 64);
      begin
        (* inverse zigzag / quantise coef *)
        if pos = 0 && s.mtype.intra then s.block_iqnt.{0,0} <- level * 8
        else 
          let zx,zy = Zigzag.inv2d.(pos) in
          s.block_iqnt.{zy,zx} <- Quant.inv s.quant level
      end;
      decode_coefs s false (pos+1)
    end

  (* copy skipped macroblocks *)
  let rec copy_skipped_mbs s last = 
    s.mba <- s.mba + 1;
    if s.mba >= last then ()
    else
      let x,y = mb_to_pos s.gob_header.Gob_header.group_number s.mba in
      Recon.copy ~x:(x*16) ~y:(y*16) ~ref:s.ref ~cur:s.cur;
      copy_skipped_mbs s last

  let compute_mvs s = 
    let open Mtype in
    let left_of_gob = s.mba=1 || s.mba=12 || s.mba=23 in
    if left_of_gob || s.mbadiff <> 1 || not s.mtype.mvd then begin
      s.mvx <- get_mv 0 s.mvdx;
      s.mvy <- get_mv 0 s.mvdy
    end else begin
      s.mvx <- get_mv s.mvx s.mvdx;
      s.mvy <- get_mv s.mvy s.mvdy
    end

  let update_mv_pred s = 
    let open Mtype in
    if s.mtype.intra || not s.mtype.mvd then begin
      s.mvx <- 0;
      s.mvy <- 0
    end

  let decode_block s i x y = 
    let open Mtype in
    let cur, ref, bs, ox, oy, mvx, mvy = get_block s i in
    let x, y = (x*bs) + ox, (y*bs) + oy in
    if s.cbp land (1 lsl (5-i)) = 0 then
      Recon.skip ~x ~y ~mvx ~mvy ~cur ~ref
    else begin
      Frame.SInt.Plane.clear s.block_iqnt 0;
      decode_coefs s true 0;
      Dct.Chen.idct s.block_iqnt s.block_idct;
      if s.mtype.intra then 
        Recon.intra ~x ~y ~pred:s.block_idct ~cur
      else if s.mtype.fil then
        Recon.fil ~x ~y ~mvx ~mvy ~pred:s.block_idct ~ref ~cur
      else
        Recon.mc ~x ~y ~mvx ~mvy ~pred:s.block_idct ~ref ~cur
    end

  let decode_blocks s x y = 
    let rec b i = 
      if i < 6 then begin
        decode_block s i x y;
        b (i+1)
      end
    in b 0

#if log_level>2
  let log_si_plane p = 
    let open Frame.SInt.Plane in
    for y=0 to height p - 1 do
      for x=0 to width p - 1 do
        log (sprintf "%-6i " p.{y,x})
      done;
      log ("\n")
    done
  let log_u8_plane p = 
    let open Frame.SInt.Plane in
    for y=0 to height p - 1 do
      for x=0 to width p - 1 do
        log (sprintf "%-3i " p.{y,x})
      done;
      log ("\n")
    done
#endif

#if log_level>1
  let log_mb_header s = 
    let x, y = mb_to_pos s.gob_header.Gob_header.group_number (s.mba+s.mbadiff) in
    let () = 
      log ("-------------------------------------------------------\n");
      log (sprintf "mb addr=[%i=%i+%i] [%i,%i] @ %i\n" 
        (s.mba+s.mbadiff) s.mba s.mbadiff
        x y (R.pos s.bits));
      log (sprintf "mtype=%s\n" (Types.Mtype.Show_t.show s.mtype));
      log (sprintf "quant=%i cbp=%i mv=[%i,%i] mvd=[%i,%i]\n" 
        s.quant s.cbp s.mvx s.mvy s.mvdx s.mvdy);
      log ("-------------------------------------------------------\n")
    in ()
#endif
#if log_level>2 
  let log_mb_data s = 
    let () = 
      log_si_plane s.block_iqnt;
      log ("-------------------------------------------------------\n");
      log_si_plane s.block_idct;
      log ("-------------------------------------------------------\n")
    in
    ()
#endif

  let decode_mb s = 
    (* decode the mb header *)
    read_mb_header s;
#if log_level>1
    log_mb_header s;
#endif
    (* copy skipped mbs *)
    copy_skipped_mbs s (s.mba + s.mbadiff);
    (* get mb position *)
    let x, y = mb_to_pos s.gob_header.Gob_header.group_number s.mba in
    compute_mvs s;
    (* decode each block *)
    decode_blocks s x y;
#if log_level>2
    log_mb_data s;
#endif
    (* update the mv predictors *)
    update_mv_pred s

  let config_frame s = 
    let open Picture_header in
    match s.cur.Frame.U8.width, s.picture_header.source_format with
    | 176, Source_format.Cif 
    | 352, Source_format.Qcif -> begin
      s.ref <- alloc_frame s.picture_header.source_format;
      s.cur <- alloc_frame s.picture_header.source_format
    end
    | _ -> ()

  let swap s = 
    let cur = s.cur in
    s.cur <- s.ref;
    s.ref <- cur 
    
  let decode_picture s = 
    let open Gob_header in
    B.find_picture_start_code s.bits;
    s.picture_header <- B.read_picture_header s.bits;
#if log_level>0
    log ("*******************************************************\n");
    log (sprintf "picture_header @ %i\n%s\n" 
      (R.pos s.bits)
      (Picture_header.Show_t.show s.picture_header));
    log ("*******************************************************\n");
#endif
    config_frame s;
    s.gob_header <- { group_number=0; gob_quant=8 }; (* default header *)
    swap s;
    let complete_gob () = 
      if s.gob_header.group_number <> 0 then 
        copy_skipped_mbs s 34
    in
    let rec loop () = 
      let code = R.show s.bits 20 in
      if code = 16 then begin
        (* PSC, done *)
        complete_gob () (* complete previous gob *)
      end else if (code lsr 4) = 1 then begin
        (* GSC *)
        complete_gob (); (* complete previous gob *)
        s.gob_header <- B.read_gob_header s.bits;
#if log_level>0
        log ("+++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
        log (sprintf "gob_header @ %i\n%s\n" 
          (R.pos s.bits)
          (Gob_header.Show_t.show s.gob_header));
        log ("+++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
#endif
        s.quant <- s.gob_header.gob_quant;
        s.mba <- 0;
        loop();
      end else if (code lsr 9) = 15 then begin
        (* stuffing *)
        R.advance s.bits 11; 
        loop();
      end else begin
        (* macroblock *)
        decode_mb s; loop ()
      end
    in
    loop()

end


