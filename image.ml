(* 画像処理ライブラリ *)

(* cmap: colormap: i -> (R,G,B) *)
type cmap = (d1,char*char*char) arr

let reds : cmap = iota 256 |> map (fun i -> Char.(chr i, chr 0, chr 0))

let cmap1 : cmap = iota 256 |>
    map (fun i -> Char.(chr (min 255 (2*i)), chr (min 255 (i)), chr (min 255 (2*i))))


let display_as_ppm : string -> cmap -> (d2,int) arr -> unit =
 fun fname cmap arr2 -> (* fname:ファイル名 cmap:そのままの意味 arr2:拡大後の配列  *)
  let max_color_val = 255 in (* 輝度の最大値 *)
  let cmap_bytes = map (fun (r,g,b) -> (* cmap:(d1,char*char*char) -> (d1,bytes) arrへの変換？ *)
    Bytes.init 3 (function 0 -> r | 1 -> g | 2 -> b | _ -> assert false))
    cmap in
  let cmap_len1 = rho cmap in (* cmapのサイズ(bounds) *)
  let (arr2_min,arr2_max) = (* arr2の最大値arr2_maxと最小値arr2_min *)
    arr2 |> to_rows |> map (map pair_dup >> reduce minmax) |> reduce minmax in
  let arr2_range = arr2_max - arr2_min in (* arr2の値の範囲 *)
  let ch = open_out_bin fname in (* ファイルを開いて書き出し開始 *)
  let (upr,upc) = rho arr2 in (* arr2のbounds *)
  Printf.fprintf ch "P6 %d %d %d\n" (upc+1) (upr+1) max_color_val;
  let f_cmap x = (* ((x - arr2_min) * cmap_len1) / arr2_range)はどういう計算？ *)
    (if arr2_range = 0 then 0 else ((x - arr2_min) * cmap_len1) / arr2_range) |>
    get cmap_bytes in (* f_cmap x: cmap_bytesの(if arr2_ran~~arr2_range)番目の要素を得る *)
  let _ = arr2 |> to_rows |> iter (map f_cmap >> iter (output_bytes ch)) in (* arr2にf_cmapを適用(map)して得られた(R,G,B)書き込み *)
  close_out ch (* ファイルを閉じて書き出し終了 *)

let img1 = Arr ((199,199), fun (i,j) -> i + j)

let img2 = Arr ((199,199), fun (i,j) -> i*i + j*j)

let img3 = Arr ((199,199), fun (i,j) ->
  (i-100)*i + (j-50)*j |> float_of_int |> sqrt |> truncate)

let _ = display_as_ppm "/tmp/a1.ppm" cmap1 img3

(*
This function uses scanf to parse the header, and low-level
really_input to read the body of the image.
In OCaml, mixing the low-level and the formatted input is very
error-prone. The formatted input reads by 1024-byte chunks.
Therefore, we create our own formatted buffer (and hope
there is no look-ahead).
*)

let read_pgm : string -> (d2,int) arr =
  let open Bigarray in
  let allocate cin =                    (* read header, allocate image *)
    let alloc ncols nrows =
      Printf.printf "Reading image %dx%d...\n" nrows ncols;
      Array2.create int8_unsigned c_layout nrows ncols in
    Scanf.bscanf (Scanf.Scanning.from_function (fun () -> input_char cin))
      "P5 %u %u %u%c" @@
      fun ncols nrows maxgray term ->
        if not (term = ' ' || term = '\n' || term = '\t') then
          failwith ("Could not find the single terminating whitespace " ^
                    "at the end of the header");
        if maxgray > 255 || maxgray <= 0 then
          Printf.ksprintf failwith "maxgray %d is odd or unsupported"
                      maxgray;
        alloc ncols nrows
  in
  let read_data cin img =
    let nrows = Array2.dim1 img and ncols = Array2.dim2 img in
    let buf = Bytes.make ncols '\000' in (* a scanline *)
    for i = 0 to nrows - 1 do
      really_input cin buf 0 ncols;
      let scanline = Array2.slice_left img i in
      for j = 0 to ncols - 1 do
        scanline.{j} <- Char.code (Bytes.get buf j)
      done
    done;
    Arr ((nrows-1,ncols-1), fun (i,j) -> img.{i,j})
  in
  fun fname ->
    let cin = open_in_bin fname in
    let r = try read_data cin (allocate cin) with e -> close_in cin; raise e in
    close_in cin; r

let takao () = read_pgm "takaosan.pgm"

(*
let takao_small = takao |> scale_nn 0.25

let _ = display_as_ppm "/tmp/a1.ppm" reds takao_small
*)
;;
