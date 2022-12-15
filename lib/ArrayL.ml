
type ('i,'a) arr = Arr of 'i * ('i -> 'a)
(**
[Arr (upb,indexf)]
[upb] - maximum index
*)

(** vector dimension *)
type d1 = int

let pr_arr1gen : (bool -> unit) -> ('a -> unit) -> (d1,'a) arr -> unit =
  fun pr_border pr_el  -> function Arr (upb,xf) ->
    pr_border false;
    pr_el @@ xf 0;
    for i=1 to upb do
      print_string " ";
      pr_el @@ xf i
    done;
    pr_border true


let pr_arr1 : (d1,int) arr -> unit = fun arr ->
  pr_arr1gen (fun b -> print_string "|"; if b then print_newline ())
             print_int arr


let of_array : 'a array -> (d1,'a) arr = fun arr ->
  let n = Array.length arr in
  assert (n>0);
  Arr(n-1, Array.get arr)

let rev : (d1,'a) arr -> (d1,'a) arr =
  function Arr (upb,a) ->
  Arr(upb,fun i -> a (upb-i))

(** composition from left to right *)
let (>>) f g = fun x -> f x |> g

let map : ('a -> 'b) -> ('bounds,'a) arr -> ('bounds,'b) arr
    = fun f -> function Arr (upbs,xf) ->
      Arr(upbs, xf >> f)

let iota : int -> (d1,int) arr = fun n ->
  assert (n>0);
  Arr(n-1,Fun.id)

let hook : ('x -> 'a -> 'b) -> ('x -> 'a) -> 'x -> 'b =
  fun f g x -> f x (g x)

let fork : ('a->'b->'c) -> ('x->'a) -> ('x->'b) -> ('x->'c) =
  fun h f g x -> h (f x) (g x)

let add2 : (d1,int) arr -> (d1,int) arr -> (int,int) arr
    = fun (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fork (+) xf yf)

let zip_with : ('a -> 'b -> 'c) ->
           ('bounds,'a) arr -> ('bounds,'b) arr -> ('bounds,'c) arr
    = fun f (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fork f xf yf)

let reduce : ('a -> 'a -> 'a) -> (d1, 'a) arr -> 'a
  = fun f -> function Arr (upb, xf) ->
    let rec loop acc i =
      if i > upb then acc else loop (f acc (xf i)) (i+1)
    in loop (xf 0) 1

let length : (d1,'a) arr -> int = function Arr(upb,_) -> upb + 1

let mean : (d1,float) arr -> float =
  fork (/.) (reduce (+.)) (length >> float_of_int)

let sqrf : float -> float = fun x -> x *. x

let assure : string -> ('a -> bool) -> 'a -> 'a =
  fun str pred x ->
    if pred x then x else failwith str

let standard_dev : (d1,float) arr -> float =
  hook
   (fun x xbar -> map (Fun.flip (-.) xbar) x |>
     map sqrf |> reduce (+.) |>
    (Fun.flip (/.)
      (((length x |> assure "too short" (Fun.flip(>) 1)) -1) |>
      float_of_int)) |>
    sqrt)
  mean

let dt : ('a -> 'b -> 'c) -> ('c -> 'd) -> ('a -> 'b -> 'd)
  = fun f g -> fun x y -> f x y |> g

let dot_gen : ('a -> 'b -> 'c) -> ('c -> 'c -> 'c) -> ((d1,'a) arr -> (d1,'b) arr -> 'c)
  = fun f g -> dt (zip_with f) (reduce g)

let dist = fun a b -> dot_gen (fun x y -> x -. y |> sqrf) (+.) a b |> sqrt
let dot = dot_gen ( * ) (+)

let distinf : (d1,int) arr -> (d1,int) arr -> int =
  dot_gen (dt (-) abs) max

let dtf : ('a -> 'b -> 'c) -> ('c -> 'd) ->
  (('t -> 'a) -> ('t -> 'b) -> ('t -> 'd)) =
  fun f g -> dt (fork f) ((>>) g)

type d2 = int * int

let rho2 : d2 -> (d1,'a) arr -> (d2,'a) arr
 = fun (nr,nc) (Arr (upb,xf)) ->
   assert (nr*nc = upb+1);
   Arr((nr-1,nc-1), fun (i,j) -> i*nc + j |> xf)

let pr_arr2 : (d2,int) arr -> unit = function Arr ((nr1,nc1),xf) ->
  Arr (nr1,fun i -> Arr (nc1, fun j -> xf (i,j))) |>
  pr_arr1gen (fun _ -> ()) pr_arr1

let transpose : ('d1*'d2,'a) arr -> ('d2*'d1,'a) arr
    = function Arr ((nr1,nc1),xf) ->
      Arr ((nc1,nr1), fun (i,j) -> xf (j,i))

let addm : (d2,int) arr -> (d2,int) arr -> (d2,int) arr =
  fun (Arr (boundsx,xf)) (Arr (boundsy,yf)) ->
    assert (boundsx = boundsy);
    Arr (boundsx, fork (+) xf yf)

let row : (d2,'a) arr -> int -> (d1,'a) arr
    = function Arr ((nr1,nc1),xf) -> fun i ->
      Arr (nc1,fun j -> xf (i,j))

let to_rows : (d2,'a) arr -> (d1,(d1,'a)arr) arr
    = function (Arr ((nr1,nc1),_)) as arr ->
      Arr (nr1, row arr)

let matmul : (d2,int) arr -> (d2,int) arr -> (d2,int) arr
    = function Arr ((nr1,nc1), xf1) as m1 ->
      function Arr ((nr2,nc2), xf2) as m2 ->
      assert (nc1 = nr2);
      Arr ((nr1,nc2),
        fork dot
             (fst >> row m1)
             (snd >> row (transpose m2)))

let matmul_gen : ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) ->
  (d2,'a) arr -> (d2,'a) arr -> (d2,'a) arr
    = fun f g ->
      function Arr ((nr1,nc1), _) as m1 ->
      function Arr ((nr2,nc2), _) as m2 ->
      assert (nc1 = nr2);
      Arr ((nr1,nc2), fun (i,j) ->
        dot_gen f g (row m1 i) (row (transpose m2) j))

let append : (d1,'a) arr -> (d1,'a) arr -> (d1,'a) arr
    = function Arr (upb1,xf1) ->
      function Arr (upb2,xf2) ->
        Arr (upb1+upb2+1, fun i -> if i <= upb1 then xf1 i else xf2 (i-upb1-1))

let intercalate : 'a -> (d1,'a) arr -> (d1,'a) arr
    = fun x -> function Arr (upb1,xf1) ->
      Arr (upb1*2, fun i -> if i mod 2 = 0 then xf1 (i / 2) else x)

let iter : ('a -> unit) -> ('s,'a) arr -> unit
    = fun f -> function Arr (upb,xf) ->
      for i = 0 to upb do xf i |> f done

let pr_arr1' : (d1,int) arr -> unit = fun arr ->
  let arr' = arr |> map string_of_int |> intercalate " " in
  append (append (of_array [|"|"|]) arr') (of_array [|"|\n"|]) |>
  iter print_string

let minmax : 'a * 'a -> 'a * 'a -> 'a * 'a =
  fun (x1,y1) (x2,y2) -> (min x1 x2, max y1 y2)

let pair_dup : 'a -> 'a * 'a = fun x -> (x,x)

let rho : ('bounds, 'a) arr -> 'bounds
    = function Arr (b, _) -> b

let get : ('bounds, 'a) arr -> 'bounds -> 'a
    = function Arr (b, xf) -> fun i -> xf i

let materialize2 : (d2,'a) arr -> (d2,'a) arr
 = function Arr ((upr,upc),xf) ->
   let arr = Array.make ((upr+1) * (upc+1)) (xf (0,0)) in
   for i=0 to upr do
     for j=0 to upc do
       arr.(i*(upc+1) + j) <- xf (i,j)
     done
   done;
   Arr ((upr,upc), fun (i,j) -> arr.(i*(upc+1) + j))
