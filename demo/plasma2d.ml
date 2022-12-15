(* 2次元 plasma fractals *)

(* 宿題
3つの手法でのfractalをやってみよう
 ー 中点: 隅の点野平均
 ー 中点: edge中点の平均
 ー square-diamond

具体的に過程を記述

別方法: 三角、6角、4角(正方形ではない)、diamondなどgridを考えて、
fractal法で細かくする。最後に画像に変換する

秘訣: 拡大(upscaling) + 乱数 (奇点)

拡大＋縮小？ (特に、0.7倍のような宿題)
*)

(*
#use "image.ml";;
#use "cmaps.ml";;
*)

(* 先ず、scaleを考えまちょう
 *)
let scale_twice : (d2, 'a) arr -> (d2, 'a) arr
    = function Arr ((upbr,upbc), xf) ->
    Arr ((2*upbr+1,2*upbc+1), fun (i,j) -> xf (i/2,j/2))
(*
元の画像のpixelがx、拡大された画像のpixelがyとされて、
y[2i,2j], y[2i+1,2j], y[2i,2j+1], y[2i+1,2j+1]のそれぞれは、x[i,j]と相当する
 *)

let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
  scale_twice |> pr_arr2
(*
 |4 4 8 8|
 |4 4 8 8|
 |8 8 16 16|
 |8 8 16 16|
*)

let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
  scale_twice |> scale_twice |> pr_arr2

let fimul : float -> int -> int = fun f ->
  float_of_int >> ( *. ) f >> int_of_float

(*
縮小
一番の近くpixel (nearest-neighbor)
scale_twiceと違い、pixelとpixelの間に拡大・縮小
 *)
let scale_nn : float -> (d2, 'a) arr -> (d2, 'a) arr
 = fun scale -> function Arr ((upbr,upbc),xf) ->
   let inv = 1. /. scale in
   Arr ((fimul scale upbr, fimul scale upbc),
        fun (i,j) -> xf (fimul inv i, fimul inv j))


(* [-rscale/2 .. rscale/2]以内の乱数 *)
let rand () =
  let rscale = 6 in
  Random.int rscale - rscale / 2

let noise : (d2,int) arr -> (d2,int) arr = fun (Arr (d,_)) ->
   Arr (d, fun (i,j) -> if i land 1 = 0 && j land 1 = 0 then 0 else rand ())

(*
素朴の拡大(最近の隣やつ) ＋ 乱数
 *)
let expand_nn : (d2,int) arr -> (d2,int) arr = fun m ->
  let m2 = map (( * ) 2) m |> scale_twice in
  zip_with (+) m2 (noise m2) |> materialize2 0

let m0 = of_array [| 4;4;4;4|] |> rho2 (2,2)
let _ = m0 |> pr_arr2
let _ = m0 |> expand_nn |> pr_arr2
(*
 |8 5 8 6|
 |9 10 8 7|
 |8 7 8 7|
 |10 7 8 10|
*)

(* Church数 *)
let rec ntimes : int -> ('a -> 'a) -> 'a -> 'a = fun n f z ->
  if n = 0 then z else ntimes (n-1) f (f z)

let m = m0 |> ntimes 7 expand_nn
let _ = display_as_ppm "/tmp/a1.ppm" daytime m
(* ややまずい: 正方形っぽい *)

(* bilinear + 乱数 *)

let scale_twice_bilinear : (d2,int) arr -> (d2,int) arr = fun m ->
  let (nr1,nc1) = rho m in
  Arr ((2*nr1,2*nc1),
   fun (i,j) ->
   let i1 = i/2 and j1 = j/2 in
   let m00 = get m (i1,j1) in
   match (i land 1, j land 1) with
   | (0,0) -> m00
   | (1,0) -> (m00 + get m (i1+1,j1)) / 2
   | (0,1) -> (m00 + get m (i1,j1+1)) / 2
   | (1,1) -> (m00 + get m (i1,j1+1) + get m (i1+1,j1) +
               get m (i1+1,j1+1)) / 4
 )
(*
(以内点の)共有に気を付けて
 *)
let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
  scale_twice_bilinear |> pr_arr2

(*
 |4 6 8|
 |6 9 12|
 |8 12 16|
*)

let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
  scale_twice_bilinear |> scale_twice_bilinear |> pr_arr2
(*
 |4 5 6 7 8|
 |5 6 7 8 10|
 |6 7 9 10 12|
 |7 8 10 12 14|
 |8 10 12 14 16|
*)

(* 2倍拡大 ＋ ノイズ *)
let expander : ((d2,int) arr -> (d2,int) arr) ->  (* 拡大する関数 *)
               float ->              (* 乱数 scaling: fractal次元 *)
               (d2,int) arr -> (d2,int) arr =
  fun scaler noisef ->
  map (fimul noisef) >> scaler >> fun m2 ->
  zip_with (+) m2 (noise m2) |> materialize2 0


let m = m0 |> ntimes 8 (expander scale_twice_bilinear 2.)
let _ = display_as_ppm "/tmp/a1.ppm" daytime m
let _ = display_as_ppm "/tmp/a1.ppm" dusk m
let _ = display_as_ppm "/tmp/a1.ppm" night m

let m = m0 |> ntimes 8 (expander scale_twice_bilinear 1.2)
let _ = display_as_ppm "/tmp/a1.ppm" daytime m
let _ = display_as_ppm "/tmp/a1.ppm" night m

let m = m0 |> ntimes 8 (expander scale_twice_bilinear 2.1)
let _ = display_as_ppm "/tmp/a1.ppm" daytime m
let _ = display_as_ppm "/tmp/a1.ppm" night m
(* 自分で ffいじてみて *)

let _ = iota 13 |> map (float_of_int >> ( *. ) 0.1 >> (+.) 1.0) |>
   iter (fun i -> Printf.printf "%g\n" i)

let _ = iota 13 |> map (float_of_int >> ( *. ) 0.1 >> (+.) 1.0) |>
   iter (fun i ->
    m0 |> ntimes 8 (expander scale_twice_bilinear i) |>
    display_as_ppm (Printf.sprintf "/tmp/a%g.ppm" i) daytime)


(*
expand_bilinear3 同じように
*)

(* bilinear, 別の手法 *)

let shift1 : d1 -> (d1, 'a) arr -> (d1, 'a) arr
    = fun n -> function Arr (upb, xf) ->
        (* xf(i-n)はよく評価できるため 0<= i-n <= upb成り立つべき *)
        Arr(upb, fun i ->
          let j = i-n in
          (if j < 0 then 0 else if j > upb then upb else j) |> xf)

let _ = iota 5 |> shift1 0 |> pr_arr1
let _ = iota 5 |> shift1 1 |> pr_arr1
let _ = iota 5 |> shift1 (-1) |> pr_arr1


let pair_add : d2 -> d2 -> d2 = fun (x1,y1) (x2,y2) -> (x1+x2,y1+y2)
let pair_div : d2 -> int -> d2 = fun (x,y) n -> (x/n, y/n)
let pair_mul : d2 -> int -> d2 = fun (x,y) n -> (x*n, y*n)
let pair_sub : d2 -> d2 -> d2 = fun (x1,y1) (x2,y2) -> (x1-x2,y1-y2)
let pair_pos : d2 -> bool = fun (x,y) -> x > 0 && y > 0
let pair_clip : d2 -> d2 -> d2 = fun (x,y) (maxx,maxy) ->
  ((if x < 0 then 0 else if x > maxx then maxx else x),
   (if y < 0 then 0 else if y > maxy then maxy else y))

let shift2 : d2 -> (d2, 'a) arr -> (d2, 'a) arr
    = fun n -> function Arr (upb, xf) ->
        (* xf(i-n)はよく評価できるため 0<= i-n <= upb成り立つべき *)
        Arr(upb, fun i ->
          let j = pair_sub i n in
          pair_clip j upb |> xf)

(* 別の手法

let to_matrix : (d1,(d1,'a)arr) arr -> (d2,'a) arr
    = function Arr (r, xf) ->
      Arr ((r, xf 0 |> rho), fun (i,j) -> get (xf i) j)

let shift2 : d2 -> (d2, 'a) arr -> (d2, 'a) arr
    = fun (nr,nc) -> function Arr ((r,c),_) as m ->
        to_rows m |> shift1 nr |> to_matrix |> transpose |>
        to_rows   |> shift1 nc |> to_matrix |> transpose
*)

let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
        shift2 (0,0) |> pr_arr2
let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
        shift2 (0,1) |> pr_arr2
let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
        shift2 (1,0) |> pr_arr2
let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
        shift2 (1,1) |> pr_arr2
let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
        shift2 (-1,-1) |> pr_arr2


let mapi : ('bounds -> 'a -> 'b) -> ('bounds,'a) arr -> ('bounds,'b) arr
    = fun f -> function Arr (upb,xf) ->
      Arr (upb, fun i -> f i (xf i) )

let reduce2 : ('a -> 'a -> 'a) -> (d2, 'a) arr -> 'a
    = fun f m -> to_rows m |> map (reduce f) |> reduce f

(*
https://en.wikipedia.org/wiki/Convolution
https://en.wikipedia.org/wiki/Kernel_(image_processing)
http://www.songho.ca/dsp/convolution/convolution2d_example.html
*)

let convolve2 : d2 ->                   (* patternの中央 *)
                (d2,int) arr ->         (* pattern *)
                (d2,int) arr -> (d2,int) arr
    = fun pcenter pattern m ->
      pattern |>
      mapi (fun idx n -> map (( * ) n) m |> shift2 (pair_sub pcenter idx)) |>
      reduce2 (zip_with (+))

let mat1 = of_array [|0;0;0;0; 0;1;1;1; 0;1;0;0; 0;0;1;0|] |> rho2 (4,4)
let mat2 = of_array [|1;1;1; 1;9;1; 1;1;1|]               |> rho2 (3,3)
let _ = mat1 |> pr_arr2
(*
mat1 = 0 0 0 0   mat2 = 1 1 1
       0 1 1 1          1 9 1
       0 1 0 0          1 1 1
       0 0 1 0
       glider
*)
let _ = convolve2 (1,1) mat2 mat1 |> pr_arr2
(*
 |1  2  3  3|
 |2 11 12 11|
 |2 12  5  4|
 |1  3 11  2|
*)

let scale_twice_bilinear : (d2,int) arr -> (d2,int) arr = fun m ->
  let mvert = convolve2 (0,0) (of_array [|1;1|] |> rho2 (2,1)) m |>
             map (fun x -> x / 2) in
  let mhorz = convolve2 (0,0) (of_array [|1;1|] |> rho2 (1,2)) m |>
             map (fun x -> x / 2) in
  let mdiag = convolve2 (0,0) (of_array [|1;1; 1;1|]  |> rho2 (2,2)) m |>
              map (fun x -> x / 4) in
  Arr (pair_mul (rho m) 2,
   fun (i,j) ->
   let idx = (i/2,j/2) in
   match (i land 1, j land 1) with
   | (0,0) -> get m idx
   | (1,0) -> get mvert idx
   | (0,1) -> get mhorz idx
   | (1,1) -> get mdiag idx
 )


let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
        convolve2 (0,0) (of_array [|1;1|] |> rho2 (2,1))
       |> pr_arr2
(*
 |12 24|
 |16 32|
*)

let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
        convolve2 (0,0) (of_array [|1;1|] |> rho2 (1,2))
       |> pr_arr2
(*
 |12 16|
 |24 32|
*)

let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
  scale_twice_bilinear |> pr_arr2

(*
 |4 6 8|
 |6 9 12|
 |8 12 16|
*)

let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
  scale_twice_bilinear |> scale_twice_bilinear |> pr_arr2
(*
 |4 5 6 7 8|
 |5 6 7 8 10|
 |6 7 9 10 12|
 |7 8 10 12 14|
 |8 10 12 14 16|
*)

(*
square-diamond
 *)

let scale_twice_sd : (d2,int) arr -> (d2,int) arr = fun m ->
  let diamond_pat = of_array [|1;2;1; 1;2;1|] |> rho2 (2,3) in
  let mvert = convolve2 (0,1) diamond_pat m |>
             map (fun x -> x / 8) in
  let mhorz = convolve2 (1,0) (transpose diamond_pat) m |>
             map (fun x -> x / 8) in
  let mdiag = convolve2 (0,0) (of_array [|1;1; 1;1|] |> rho2 (2,2)) m |>
              map (fun x -> x / 4) in
  Arr (pair_mul (rho m) 2,
   fun (i,j) ->
   let idx = (i/2,j/2) in
   match (i land 1, j land 1) with
   | (0,0) -> get m idx
   | (1,0) -> get mvert idx
   | (0,1) -> get mhorz idx
   | (1,1) -> get mdiag idx
 )

let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
  scale_twice_sd |> pr_arr2
(*
 |4 7 8|
 |7 9 10|
 |8 10 16|
*)

let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
  scale_twice_sd |> scale_twice_sd |> pr_arr2
(*
 |4 6 7 8 8|
 |6 6 7 8 8|
 |7 7 9 9 10|
 |8 8 9 11 12|
 |8 8 10 12 16|
*)

let m = m0 |> ntimes 8 (expander scale_twice_sd 2.)
let _ = display_as_ppm "/tmp/a1.ppm" daytime m
let _ = display_as_ppm "/tmp/a1.ppm" night m

let m = m0 |> ntimes 8 (expander scale_twice_sd 2.1)
let _ = display_as_ppm "/tmp/a1.ppm" daytime m
let _ = display_as_ppm "/tmp/a1.ppm" night m

(*
https://en.wikipedia.org/wiki/Bicubic_interpolation
*)

let bicubic_kernel = of_array [|-1;9;9;-1|] (* 1/16 *)

let outer_product : (d1,int) arr -> (d1,int) arr -> (d2,int) arr =
  fun (Arr (nr,xf)) (Arr (nc,yf)) ->
    Arr ((nr,nc), fun (i,j) -> xf i * yf j)

let bicubic_bikernel = outer_product bicubic_kernel bicubic_kernel
let _ = bicubic_bikernel |> pr_arr2


let scale_twice_bc : (d2,int) arr -> (d2,int) arr = fun m ->
  let mvert = convolve2 (1,0) (bicubic_kernel |> rho2 (4,1)) m |>
             map (fun x -> x / 16) in
  let mhorz = convolve2 (0,1) (bicubic_kernel |> rho2 (1,4)) m |>
             map (fun x -> x / 16) in
  let mdiag = convolve2 (1,1) bicubic_bikernel m |>
              map (fun x -> x / 256) in
  Arr (pair_mul (rho m) 2,
   fun (i,j) ->
   let idx = (i/2,j/2) in
   match (i land 1, j land 1) with
   | (0,0) -> get m idx
   | (1,0) -> get mvert idx
   | (0,1) -> get mhorz idx
   | (1,1) -> get mdiag idx
 )

let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
  scale_twice_bc |> pr_arr2
(*
|4 6 8|
 |6 9 12|
 |8 12 16|
*)

let _ = of_array [| 4;8;8;16|] |> rho2 (2,2) |>
  scale_twice_bc |> scale_twice_sd |> pr_arr2
(*
 |4 5 6 7 8|
 |5 6 7 8 9|
 |6 7 9 10 12|
 |7 8 10 12 13|
 |8 9 12 13 16|
*)

let m = m0 |> ntimes 8 (expander scale_twice_bc 2.)
let _ = display_as_ppm "/tmp/a1.ppm" daytime m
let _ = display_as_ppm "/tmp/a1.ppm" night m

let m = m0 |> ntimes 8 (expander scale_twice_bc 1.9)
let _ = display_as_ppm "/tmp/a1.ppm" daytime m
let _ = display_as_ppm "/tmp/a1.ppm" night m

(* 論文のため *)
(*
let m = m0 |> ntimes 8 (expander scale_twice_bc 1.2)
let _ = display_as_ppm "/tmp/a1.ppm" daytime m
let m = m0 |> ntimes 8 (expander scale_twice_bc 2.0)
let _ = display_as_ppm "/tmp/a1.ppm" daytime m
let m = m0 |> ntimes 8 (expander scale_twice_bc 3.0)
let _ = display_as_ppm "/tmp/a1.ppm" daytime m

let m0 = of_array [| 4;0;0;2|] |> rho2 (2,2) in
  m0 |> ntimes 8 (expander scale_twice_bc 2.0)
  |> display_as_ppm "/tmp/a1.ppm" daytime
*)

(*
https://en.wikipedia.org/wiki/Lanczos_resampling
*)

(*
3x 拡大も同じようにできる
*)

(*
(* Laplacian pyramid *)
let expand_gp : (d2,int) arr -> (d2,int) arr = fun m ->
  let m2 = map (fimul 1.2) m |> scale_twice in
  let gp = Arr (rho m2, fun _ -> rand ()) |> materialize2 0 in
  let gp' = Arr (rho m2,
   fun (i,j) -> if i land 1 = 1 && j land 1 = 1 then
     let others = get gp (i-1,j-1) + get gp (i-1,j) + get gp (i,j-1) in
     - others else get gp (i,j)) in
  zip_with (+) gp' m2


let _ = of_array [|4|] |> rho2 (1,1) |> expand_gp |> expand_gp |> pr_arr2

let mg = of_array [|4|] |> rho2 (1,1) |>
         expand_gp |> expand_gp |>
         expand_gp |> expand_gp |>
         expand_gp |> expand_gp |>
         expand_gp |> expand_gp
let _ = display_as_ppm "/tmp/a1.ppm" daytime mg

(* 2倍の拡大: 近所＋乱数
 *)


let m0' = of_array [| 4;5;4; 5; 7; 5; 4; 5; 4|] |> rho2 (3,3)
let m2 = m0' |>
   expand1 |> materialize2 0 |>
   expand1 |> materialize2 0 |>
   expand1 |> materialize2 0 |>
   expand1 |> materialize2 0 |>
   expand1 |> materialize2 0 |>
   expand1 |> materialize2 0 |>
   expand1 |> materialize2 0 |>
   expand1 |> materialize2 0

let m0' = of_array [| 4;4;4; 4; 4; 4; 4; 4; 4|] |> rho2 (3,3)
let m2 = m0' |>
   expand1 |> materialize2 0 |>
   expand |> materialize2 0 |>
   expand1 |> materialize2 0 |>
   expand |> materialize2 0 |>
   expand1 |> materialize2 0 |>
   expand |> materialize2 0 |>
   expand1 |> materialize2 0 |>
   expand |> materialize2 0
let _ = display_as_ppm "/tmp/a1.ppm" daytime m2

let expand3 : (d2,int) arr -> (d2,int) arr = fun m ->
  let m = map (( * ) 3) m in
  let (nr1,nc1) = rho m in             (* 2^n *)
  Arr ((3*nr1,3*nc1),
   fun (i,j) ->
   let i1 = i/3 and j1 = j/3 in
   let m00 = get m (i1,j1) in
   match (i mod 3, j mod 3) with
   | (0,0) -> m00
   | (1,0) -> (m00*2 + (get m (i1+1,j1))) / 3 + rand ()
   | (2,0) -> (m00 + (get m (i1+1,j1))*2) / 3 + rand ()
   | (0,1) -> (m00*2 + (get m (i1,j1+1))) / 3 + rand ()
   | (0,2) -> (m00 + (get m (i1,j1+1))*2) / 3 + rand ()
   | (1,1) -> (m00*4 + (get m (i1+1,j1))*3 + (get m (i1,j1+1))*3 +
               (get m (i1+1,j1+1))*2) / 12 + rand ()
   | (1,2) -> (m00*3 + (get m (i1+1,j1))*2 + (get m (i1,j1+1))*4 +
               (get m (i1+1,j1+1))*3) / 12 + rand ()
   | (2,1) -> (m00*3 + (get m (i1+1,j1))*4 + (get m (i1,j1+1))*2 +
               (get m (i1+1,j1+1))*3) / 12 + rand ()
   | (2,2) -> (m00*2 + (get m (i1+1,j1))*3 + (get m (i1,j1+1))*3 +
               (get m (i1+1,j1+1))*4) / 12 + rand ()
 )

(*
3倍拡大
 *)


let m0_2 = of_array [| 4;4;4; 4; 4; 4; 4; 4; 4|] |> rho2 (3,3)
let m2 = m0_2 |>
   expand1 |> materialize2 0 |> scale_nn 0.7 |>
   expand |> materialize2 0 |> scale_nn 0.7 |>
   expand1 |> materialize2 0 |> scale_nn 0.7 |>
   expand |> materialize2 0 |> scale_nn 0.7 |>
   expand1 |> materialize2 0 |> scale_nn 0.7 |>
   expand |> materialize2 0 |> scale_nn 0.7 |>
   expand1 |> materialize2 0 |> scale_nn 0.7 |>
   expand |> materialize2 0
let _ = display_as_ppm "/tmp/a1.ppm" daytime m2

*)

;;

