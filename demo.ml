open Printf;;

let verbose = ref false
let print_string s = if !verbose then print_string s else ()
let print_int n = if !verbose then print_int n else ()
let print_newline () = if !verbose then print_newline () else ()
;;

#warnings "-8";;
#use "intro2021.ml";;
#use "image.ml";;
#use "cmaps.ml";;

verbose := true;;

let fimul : float -> int -> int = fun f ->
  float_of_int >> ( *. ) f >> int_of_float

let scale_twice : (d2, 'a) arr -> (d2, 'a) arr
    = function Arr ((upbr,upbc), xf) ->
    Arr ((2*upbr+1,2*upbc+1), fun (i,j) -> xf (i/2,j/2))

(*
Shrink
nearest pixel (nearest-neighbor)
Unlike scale_twice, enlarge/shrink between pixels
 *)
let scale_nn : float -> (d2, 'a) arr -> (d2, 'a) arr
 = fun scale -> function Arr ((upbr,upbc),xf) ->
   let inv = 1. /. scale in
   Arr ((fimul scale upbr, fimul scale upbc),
        fun (i,j) -> xf (fimul inv i, fimul inv j))


(* random number within [-rscale/2 .. rscale/2] *)
let rand () =
  let rscale = 6 in
  Random.int rscale - rscale / 2

let noise : (d2,int) arr -> (d2,int) arr = fun (Arr (d,_)) ->
   Arr (d, fun (i,j) -> if i land 1 = 0 && j land 1 = 0 then 0 else rand ())

(*
Naive Expansion (Recent Neighbor) + Random Number
 *)
let expand_nn : (d2,int) arr -> (d2,int) arr = fun m ->
  let m2 = map (( * ) 2) m |> scale_twice in
  zip_with (+) m2 (noise m2) |> materialize2 0

let m0 = of_array [| 4;4;4;4|] |> rho2 (2,2)
(* let _ = m0 |> pr_arr2 *)

(* Church numbers *)
let rec ntimes : int -> ('a -> 'a) -> 'a -> 'a = fun n f z ->
  if n = 0 then z else ntimes (n-1) f (f z)

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


(* 2x zoom + noise *)
let expander : ((d2,int) arr -> (d2,int) arr) ->  (* expander *)
               float ->              (* scaling number : fractal dimension *)
               (d2,int) arr -> (d2,int) arr =
  fun scaler noisef ->
  map (fimul noisef) >> scaler >> fun m2 ->
  zip_with (+) m2 (noise m2) |> materialize2 0

let nr_frame = ref 0
let reset () = nr_frame := 0

let frame prefix m =
    incr nr_frame;
    display_as_ppm (sprintf "%s.%03d.ppm" prefix !nr_frame) daytime m

let konst a _ = a

let _ = iota 13 |> map (float_of_int >> ( *. ) 0.1 >> (+.) 1.0) |>
   iter (fun i ->
    reset ();
    printf "%g\n%!" i;
    m0 |> ntimes 11 (expander scale_twice_bilinear i >> hook konst (frame @@ sprintf "b%02g" (i *. 10.))) |> konst ()
  )
