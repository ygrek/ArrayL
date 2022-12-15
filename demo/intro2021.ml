(* An Array Programming Language *)
(*
#use "intro2021.ml";;
*)

(* getting started *)

(*what is an array*)

(*
content and shape
1 2 3 4 5 6

1 2 3
4 5 6

1 2
3 4
5 6

Specifically, what is shape?
1 2 3 4 5 6
  a_0 = 1
  a_1 = 2
  ...
  a_5 = 6
It's a [0..5] -> [1..6] function
Such a function expresses an array in the sense of a mapping
(By the way, in Fortran, calling a function and taking an element from an array have the same notation: A(I))


[0..5] domain of function represents the shape
The range of the function is the content of the array

0 0 0 0 0 0
It is expressed by the [0..5] -> 0 function.
The [0..5] part is the same as before (domain, shape), but the content (range) is different

1 2 3
4 5 6
If
a_00 = 1, a_01 = 2, ...
A function of [0..1] x [0..2] -> [1..6]
Different domain but same range

1 2
3 4
5 6
if?

It is [0..5] -> int
A function like above cannot be expressed directly in OCaml, but can be indirectly expressed as
*)

type ('i,'a) arr = Arr of 'i * ('i -> 'a)

(*
Arr (upb,indexf)
upb: maximum index
    No minimum index specified explicitly (like 0)

   So there is no empty array
 *)

(*
Finally, can scalar be expressed as such?
*)

(* <hide> *)
(* scalar is an array with rank=0 *)

let one : (unit,int) arr = Arr ((),fun _ -> 1)
(* </hide> *)
(*
scalar is a kind of vector (for APL)
But in algebra, scalar and vector are completely different things
We also distinguish between scalar and vector
*)


(* vector: array with rank=1 *)

type d1 = int (* originally a natural number *)

(* Show it *)
(* The following is for now, let's fly *)

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


(* 通常の配列はvectorである *)

let of_array : 'a array -> (d1,'a) arr = fun arr ->
  let n = Array.length arr in
  assert (n>0);
  Arr(n-1, fun i -> arr.(i))

(* 同じ、lambdaなし *)
(* a.(i) === Array.get a i
   Currying
*)
let of_array : 'a array -> (d1,'a) arr = fun arr ->
  let n = Array.length arr in
  assert (n>0);
  Arr(n-1, Array.get arr)


let a1 = of_array [|3;1;4;1;5;9;2|]

let _ = pr_arr1 a1

(* 逆順序で入れ替えて しよう *)

(*hide*)
(* a=[1;2;3;4] b=[4;3;2;1]
   a_0 = 1 = b_3
   a_1 = 2 = b_2
一般化？
 *)
let rev : (d1,'a) arr -> (d1,'a) arr =
  function Arr (upb,a) ->
  Arr(upb,fun i -> a (upb-i))

let _ = pr_arr1 (rev a1)

let _ = pr_arr1 @@ rev a1

(* 左から右への流れ *)
let _ = rev a1 |> pr_arr1
let _ = a1 |> rev |> pr_arr1

let _ = a1 |> rev |> rev |> pr_arr1
(*/hide*)


(*Let's 2*v*)
(*hide*)
(* Consider a mathematical formula: y_i = 2*x_i
    OCaml notation is close to mathematical notation *)
let mul1 : int -> (d1,int) arr -> (d1,int) arr
    = fun n -> function Arr (upb,a) ->
      Arr(upb, fun i -> n * (a i))

(* Composition in mathematics. Do you know it? *)
(* composite from left to right *)
let (>>) f g = fun x -> f x |> g

(* Relation between |> and >>?

x |> f |> g === x |> (f >> g)
compared to composition in mathematics
*)

(* Talking about currying before *)
let mul1 : int -> (d1,int) arr -> (d1,int) arr
    = fun n -> function Arr (upb,a) ->
      Arr(upb, a >> ( * ) n)

let _ = pr_arr1 a1

let _ = a1 |> mul1 2 |> pr_arr1
(*/hide*)



(* 1+v しよう *)
(*hide*)
let add1 : int -> (d1,int) arr -> (d1,int) arr
    = fun n -> function Arr (upb,xf) ->
      Arr(upb, xf >> ( + ) n)
(*/hide*)

(* 一般化 *)
(*hide*)
(* もう一回数学的な式を考えよう
 *)
let map1 : ('a -> 'b) -> (d1,'a) arr -> (d1,'b) arr
    = fun f -> function Arr (upb,a) ->
      Arr(upb, a >> f)
(*/hide*)
(* a >> f means: f transforms the range of a; f applies to the range of a
Processing hints in the array body
*)

(* 2*v+1
    Our Operation Order vs. APL Order
 *)
(*hide*)
let _ = pr_arr1 a1
let _ = a1 |> map1 (( * ) 2) |> map1 ((+) 1) |> pr_arr1

let _ = a1 |> (map1 (( * ) 2) >> map1 ((+) 1)) |> pr_arr1

let _ = a1 |> map1 ( ( * ) 2 >> (+) 1) |> pr_arr1

(*
map1 f >> map1 g ===
map1 (f >> g)
Same meaning, slightly more efficient

The story of symbol
Applying a rule like map1 (even with your eyes closed) always gives the correct result
(semantic preservation).
It is an algebraic transformation
*)
(*/hide*)

let iota : int -> (d1,int) arr = fun n ->
  assert (n>0);
  Arr(n-1,fun i -> i)

(* id is really basic *)
let iota : int -> (d1,int) arr = fun n ->
  assert (n>0);
  Arr(n-1,Fun.id)

let _ = iota 5 |> pr_arr1

(*hide*)
let _ = iota 5 |> rev |> pr_arr1

let _ = iota 5 |> map1 (( * ) 2) |> rev |> pr_arr1
(*/hide*)


(*
In a1, the contents were saved in memory. iota?
Both iota 5 and iota 5000000 have the same amount of memory.

On Unix, /dev/zero, /dev/random, etc.

Although OS and PL (APL) seem to be very different worlds.
Above all, it is important to understand the principles/philosophies.
*)

(* x+y *)
(*hide*)
(* Correspondence: 0 -> a_0 + b_0
                   1 -> a_1 + b_1
                   ...
Generalization? implement it literally
 *)
let add2 : (d1,int) arr -> (d1,int) arr -> (d1,int) arr
    = fun (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fun i -> (xf i + yf i))
(*/hide*)

(* It's fine as it is, but expressions of that form often occur.
In APL it is called fork
APL provides; OCaml, you can define your own
In APL, hook and fork are important, so they are expressed with spaces.
*)

(*famous S combinator*)

(* wikipedia
I returns its argument:

Ix = x
K, when applied to any argument x, yields a one-argument constant function Kx, which, when applied to any argument, returns x:

Kxy = x
S is a substitution operator. It takes three arguments and then returns the first argument applied to the third, which is then applied to the result of the second argument applied to the third. More clearly:

Sxyz = xz(yz)
*)

(* `combinator' rearranges arguments and synthesizes *)
let hook : ('x -> 'a -> 'b) -> ('x -> 'a) -> 'x -> 'b =
  fun f g x -> f x (g x)

(* Do you notice anything when you take a closer look at the type? *)
let fork : ('a->'b->'c) -> ('x->'a) -> ('x->'b) -> ('x->'c) =
  fun h f g x -> h (f x) (g x)
(* a-> b-> c is the function that takes the argument(result?) of (x->a) and (x->b) *)

(*Do you know the word Mapreduce?
    Search by Google and MapReduce
*)

(*What is the relationship between fork and Hook?
Express Hook by fork.
Fork depends on Hook?
h (f x) (g x) = (h (f x)) (g x) = (f >> h) x (g x)
Check the significance of Scombinator
*)
(*
Draw a figure
Hook figure and Fork diagram, interaction (conversion)

let hook : ('x -> 'a -> 'b) -> ('x -> 'a) -> 'x -> 'b =
  fun f -> fork f Fun.id

let hook : ('x -> 'a -> 'b) -> ('x -> 'a) -> 'x -> 'b =
  fun f g x -> fork (fun y x -> y x) f g x

let hook : ('x -> 'a -> 'b) -> ('x -> 'a) -> 'x -> 'b =
  fun f g x -> fork (@@) f g x
(* hook = fork (@@) *)

let fork : ('a->'b->'c) -> ('x->'a) -> ('x->'b) -> ('x->'c) =
  fun h f g x -> hook (fun x -> f x |> h) g x

let fork : ('a->'b->'c) -> ('x->'a) -> ('x->'b) -> ('x->'c) =
  fun h f -> hook (f >> h)
*)

(* Mathematics is the science of patterns *)

(*looks like APL now*)
(*with lambdas*)
let add2 : (d1,int) arr -> (d1,int) arr -> (int,int) arr
    = fun (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fun i -> fork (+) xf yf i)

(* without lambda *)
let add2 : (d1,int) arr -> (d1,int) arr -> (int,int) arr
    = fun (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fork (+) xf yf)

let _ = pr_arr1 a1
let _ = a1 |> add2 (iota 7) |> pr_arr1

(* let's generalize *)
(* First of all, look closely at the type Do you notice anything? *)
let zip_with : ('a -> 'b -> 'c) ->
           (int,'a) arr -> (int,'b) arr -> (int,'c) arr
(* Looking at the type of add2, nothing comes to mind.
   On the other hand, zip is worth generalizing similar to map1
 *)
 (*hide*)
    = fun f (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fork f xf yf)

let _ = a1 |> zip_with (+) (iota 7) |> pr_arr1
(*/hide*)

(* In APL, zip_with is also important, so it is expressed with a space *)

(* Consider binary operations op: 'a -> 'b -> 'c
Arguments are x:'a, y:'b then op x y (if infix (like APL) then x op y)
If arguments are x:'t->'a, y:'t->'b then fork op x y (as in APL: x op y)
Arguments are x:(d1,'a)arr, y:(d1,'b)arr then zip_with op x y (as in APL: x op y)

fork, zip_with represent the application of two arguments. deep relationship. So it is represented in APL with white space

unary case: op: 'a -> 'b
If x:'a then op x
If x:'t->'a then fun t -> x t |> op === x >> op
If x:(d1,'a)arr then map op x

Synthesis and map are similarly closely related. Similarly, represented by whitespace in APL
*)


(* Let's do sum_{i=1}^{5} i^2 (using TeX notation) *)
(*hide*)
(* Generally speaking, sum_{i=0}^{n-1} a_i *)
(*If recursion can be used, recursion will appear for the first time *)
(* Is the join from the right or from the left? *)
(*
  sum_{i=0}^{n-1} a_i
  = a_0 +  sum_{i=1}^{n-1} a_i
  = (a_0 + a_1) + sum_{i=2}^{n-1} a_i
  = (a_0 + a_1 + ... a_{k-1}) + sum_{i=k}^{n-1} a_i
     already added                   yet to be computed
*)
let sum : (d1, int) arr -> int = function Arr (upb, xf) ->
  (* acc: already computed i: then still *)
  let rec loop acc i =
    if i > upb then acc else loop (acc + xf i) (i+1)
  in loop (xf 0) 1
(* in APL, called +/ *)
(*/hide*)

(* extend step by step formula *)
let _ = iota 5 |> map1 succ |> map1 (fun x -> x * x) |>
  pr_arr1

let _ = iota 5 |> map1 succ |> map1 (fun x -> x * x) |>
  sum

let sqr : int -> int = fun x -> x * x


let _ = iota 5 |> map1 succ |> map1 sqr |> sum

let _ = iota 5 |> (map1 succ >> map1 sqr) |> sum

let _ = iota 5 |> map1 (succ >> sqr) |> sum


(* Generalized by joining from the left. in APL it is called / *)
let reduce : ('a -> 'a -> 'a) -> (d1, 'a) arr -> 'a
  = fun f -> function Arr (upb, xf) ->
    let rec loop acc i =
      if i > upb then acc else loop (f acc (xf i)) (i+1)
    in loop (xf 0) 1

(* Imagine:
reduce op a = a_0 op a_1 op a_2 ... op a_(n-1)
 *)
let _ = iota 10 |> map1 succ |> reduce (+)
let _ = iota 10 |> map1 succ |> reduce ( * )  (* factorial *)
let _ = iota 10 |> map1 succ |> reduce min
let _ = iota 10 |> map1 succ |> reduce max

(* Statistics
  APL/J: mean = +/ % #
  var = (- mean) ....
 *)

(*hide*)
let length : (d1,'a) arr -> int = function Arr(upb,_) -> upb + 1



(* iota & length *)


(* Let's see the flow
   Let's draw a flowchart
   type story
*)
let mean : (d1,int) arr -> float =
  fork (fun x y -> float_of_int x /. float_of_int y) (reduce (+)) length

let mean : (d1,float) arr -> float =
  fork (/.) (reduce (+.)) (length >> float_of_int)

(* see Gary Kildall's description *)

(* first
let _ = iota 5 |>  mean
*)

let _ = iota 5 |> map1 float_of_int |> mean

(*/hide*)

(* variance? standard deviation?
   Wikipedia: sd x = sqrt (1/(N-1) Sum (x_i - xbar)^2 )
   iota 1 |> sd?

   assure: string -> ('a -> bool) -> 'a -> 'a
*)

let standard_dev : (d1,float) arr -> float =
  hook
   (fun x xbar -> map1 (Fun.flip (-.) xbar) x |>
     map1 (fun x -> x *. x) |> reduce (+.) |>
    (Fun.flip (/.) ((length x -1) |> float_of_int)) |>
    sqrt)
  mean

let sqrf : float -> float = fun x -> x *. x

let standard_dev : (d1,float) arr -> float =
  hook
   (fun x xbar -> map1 (Fun.flip (-.) xbar) x |>
     map1 sqrf |> reduce (+.) |>
    (Fun.flip (/.) ((length x -1) |> float_of_int)) |>
    sqrt)
  mean

let _ = of_array [| 1.0; 1.0; 1.0 |] |> standard_dev

let _ = of_array [| 1.0; 2.0; 3.0 |] |> standard_dev

(*
let _ = iota 1 |> map1 float_of_int |> standard_dev
*)

let assure : string -> ('a -> bool) -> 'a -> 'a =
  fun str pred x ->
    if pred x then x else failwith str

let standard_dev : (d1,float) arr -> float =
  hook
   (fun x xbar -> map1 (Fun.flip (-.) xbar) x |>
     map1 sqrf |> reduce (+.) |>
    (Fun.flip (/.)
      (((length x |> assure "too short" (Fun.flip(>) 1)) -1) |>
      float_of_int)) |>
    sqrt)
  mean

let _ = of_array [| 1.0; 2.0; 3.0 |] |> standard_dev

(*
let _ = iota 1 |> map1 float_of_int |> standard_dev
*)

let _ = a1 |> pr_arr1
let _ = a1 |> rev |> rev |> pr_arr1

(* next: equivalence
let _ = assert ([|1;2|] = [|1;3|])

a1;;
match a1 with Arr (n,xf) -> xf 2;;

let arr_cmp : (d1,'a) arr -> (d1,'a) arr -> bool

let _ = assert (a1 = (a1 |> rev |> rev))
*)


(* Let's define dot How is it different from APL *)
(*hide*)
let dot : (int,int) arr -> (int,int) arr -> int = fun arr1 arr2 ->
  zip_with ( * ) arr1 arr2 |> sum

let _ = let v = iota 5 in dot v v
let _ = dot (of_array [|0;1|]) (of_array [|5;0|])
(* details, above *)
(*/hide*)

(* more generalized *)
(*hide*)
let dot_gen : ('a -> 'a -> 'a) ->
              ('a -> 'a -> 'a) ->
              (int,'a) arr -> (int,'a) arr -> 'a
  = fun f g arr1 arr2 ->
  zip_with f arr1 arr2 |> reduce g

(*
previous dot = dot_gen ( * ) (+)
*)

(* no lambda? (squeeze) *)

(*in APL, g.f*)
let _ = let v1 = iota 5 and v2 = iota 5 |> map1 succ |> rev in
        dot_gen (fun x y -> x - y |> abs) (+) v1 v2

(* processing like other dot products?
  Distance, say the distance between (2,1) and (3,3), what?
*)
let dist = fun a b -> zip_with (-.) a b |> map1 sqrf
  |> reduce (+.) |> sqrt

let _ = dist (of_array [|2.;1.|]) (of_array [|3.;3.|])

let dist = fun a b ->
  zip_with (fun x y -> x -. y |> sqrf) a b
  |> reduce (+.) |> sqrt

let dist = fun a b ->
  dot_gen (fun x y -> x -. y |> sqrf) (+.) a b |> sqrt

let _ = dist (of_array [|2.;1.|]) (of_array [|3.;3.|])


(*
 * a1 = (a1 |> rev |> rev)
 *)

(*
add/min, add/max (distance from one town to number of towns)
and/or (same test passed, but what test?

  Equivalence and relationship: L1, L2, Linf norm

 distance: zip_with (-) a1 a2 |> map sqr |> reduce (+)
 =         zip_with (fun x y -> x - y |> sqr) a1 a2 |> reduce (+)
Remember the story of fork. I notice the pattern

let's define

dt op1 op2: fun x y -> op1 x y |> op2
*)

let dt : ('a -> 'b -> 'c) -> ('c -> 'd) ->
  ('a -> 'b -> 'd) = fun f g -> fun x y -> f x y |> g

let dot_gen : ('a -> 'b -> 'c) -> ('c -> 'c -> 'c) ->
  ((d1,'a) arr -> (d1,'b) arr -> 'c)
  = fun f g -> dt (zip_with f) (reduce g)

(* in the previous
let _ = assert (a1 = (a1 |> rev |> rev))
why is there a problem? why can't we compare functions

Talk about norm L_1, L_2, L_inf
*)
let distinf : (d1,int) arr -> (d1,int) arr -> int =
  dt (zip_with (dt (-) abs)) (reduce max)

let distinf : (d1,int) arr -> (d1,int) arr -> int =
  dot_gen (dt (-) abs) max

let _ = distinf (iota 5) (iota 5 |> map1 sqr)
let _ = distinf a1 (a1 |> rev |> rev)

(* dt can be used as a function *)
let dtf : ('a -> 'b -> 'c) -> ('c -> 'd) ->
  (('t -> 'a) -> ('t -> 'b) -> ('t -> 'd)) =
  fun f g -> fun x y -> fun t -> f (x t) (y t) |> g

let dtf : ('a -> 'b -> 'c) -> ('c -> 'd) ->
  (('t -> 'a) -> ('t -> 'b) -> ('t -> 'd)) =
  fun f g -> dt (fork f) ((>>) g)

(*
if it is a function
             fun x y -> fun t -> op1 (x t) (y t) |> op2
             = fun x y -> fork op1 x y >> op2
             = dt (fork op1) ((>>) op2)
if arr
dot_gen op1 op1:
             fun x y -> zip_with op1 x y |> reduce op2
             = dt (zip_with op1) (reduce op2)

that's why
distance: zip_with (-) x y |> map sqr |> reduce (+)
 =        zip_with (fun x y -> x - y |> sqr) x y |> reduce (+)
 =        dt (zip_with (dt (-) sqr)) (reduce (+))
*)


(* ------------------------------------------------------------------------
   2 dimensions
*)
type d2 = int * int

(* 2D array example *)
let mm1 = Arr ((1,1), function
  | (0,0) -> 1
  | (0,1) -> 2
  | (1,0) -> 3
  | (1,1) -> 4
 )

let rho2 : d2 -> (d1,'a) arr -> (d2,'a) arr
 (* first, explicit *)
 (*hide*)
 = fun (nr,nc) (Arr (upb,xf)) ->
   assert (nr*nc = upb+1);
   Arr((nr-1,nc-1), fun (i,j) -> i*nc + j |> xf)

(*
definition for i*nc + j?
without lambdas
*)

(*/hide*)

let pr_arr2 : (d2,int) arr -> unit = function Arr ((nr1,nc1),xf) ->
  Arr (nr1,fun i -> Arr (nc1, fun j -> xf (i,j))) |>
  pr_arr1gen (fun _ -> ()) pr_arr1

let _ = mm1 |> pr_arr2

let _ = iota 6 |> rho2 (2,3) |> pr_arr2

(* transpose *)
(*hide*)
let transpose : (d2,'a) arr -> (d2,'a) arr
    = function Arr ((nr1,nc1),xf) ->
      Arr ((nc1,nr1), fun (i,j) -> xf (j,i))

let transpose : ('d1*'d2,'a) arr -> ('d2*'d1,'a) arr
    = function Arr ((nr1,nc1),xf) ->
      Arr ((nc1,nr1), fun (i,j) -> xf (j,i))
(*/hide*)

let m1 = iota 6 |> rho2 (2,3)
let _ = m1 |> pr_arr2

let _ = m1 |> transpose |> pr_arr2

let _ = m1 |> transpose |> transpose |> pr_arr2

(*
let _ = m1 = (m1 |> transpose |> transpose)
*)

(* homework:
diag : (d2,'a) arr -> (d1,'a) arr
*)
let diag : (d2,'a) arr -> (d1,'a) arr
	= function Arr ((r,c), xf) ->
	  Arr (min r c, fun i -> xf (i,i) )

(*
  diag m1 --> |0 4|
  m1 |> transpose |> diag --> |0 4|
*)
let _ = diag m1 |> pr_arr1
let _ = m1 |> transpose |> diag |> pr_arr1

(* 1 + m *)
(*hide*)
let add2 : int -> (d2,int) arr -> (d2,int) arr
    = fun n -> function Arr (upbs,xf) ->
      Arr(upbs, fun (i,j) -> n + xf (i,j))
(* no lambdas *)

let _ = m1 |> pr_arr2
let _ = m1 |> add2 1 |> pr_arr2
(*/hide*)
(* generalization of map *)

(*hide*)
let map2 : ('a -> 'b) -> (d2,'a) arr -> (d2,'b) arr
    = fun f -> function Arr (upbs,xf) ->
      Arr(upbs, fun (i,j) -> xf (i,j) |> f)
(*/hide*)

(* more generalized? *)
(*hide*)
let map2 : ('a -> 'b) -> (d2,'a) arr -> (d2,'b) arr
    = fun f -> function Arr (upbs,xf) ->
      Arr(upbs, fun (i,j) -> xf (i,j) |> f)

let map1 : ('a -> 'b) -> (d1,'a) arr -> (d1,'b) arr
    = fun f -> function Arr (upb,xf) ->
      Arr(upb, fun i -> xf i |> f)

let map2 : ('a -> 'b) -> (d2,'a) arr -> (d2,'b) arr
    = fun f -> function Arr (upbs,xf) ->
      Arr(upbs, fun arg -> xf arg |> f)
let map1 : ('a -> 'b) -> (d1,'a) arr -> (d1,'b) arr
    = fun f -> function Arr (upb,xf) ->
      Arr(upb, fun arg -> xf arg |> f)

let map : ('a -> 'b) -> ('bounds,'a) arr -> ('bounds,'b) arr
    = fun f -> function Arr (upbs,xf) ->
      Arr(upbs, xf >> f)
(* same as map1 *)

let _ = m1 |> map succ |> pr_arr2

let _ = iota 6 |> map succ |> pr_arr1

let _ = one |> map succ |> (function Arr ((), xf) -> xf ())

(* It's a basic function, so don't write it explicitly in APL
   1 + iota 6  --> map ((+) 1) (iota 6)
   1 + m2      --> map ((+) 1) m2
   1 + 2       --> map ((+) 1) 2
 *)
(*/hide*)

(* m1 + m2? *)
let addm : (d2,int) arr -> (d2,int) arr -> (d2,int) arr =
  fun (Arr (boundsx,xf)) (Arr (boundsy,yf)) ->
    assert (boundsx = boundsy);
    Arr (boundsx, fun (i,j) -> xf (i,j) + yf (i,j))

let addm : (d2,int) arr -> (d2,int) arr -> (d2,int) arr =
  fun (Arr (boundsx,xf)) (Arr (boundsy,yf)) ->
    assert (boundsx = boundsy);
    Arr (boundsx, fork (+) xf yf)

(*
addm m1 (iota 6);;
*)

let zip_with : ('a -> 'b -> 'c) ->
           (int,'a) arr -> (int,'b) arr -> (int,'c) arr
    = fun f (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fork f xf yf)


let zip_with : ('a -> 'b -> 'c) ->
 ('bounds,'a) arr -> ('bounds,'b) arr -> ('bounds,'c) arr
  = fun f (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fork f xf yf)

let _ = zip_with (+) m1 (m1 |> map succ) |> pr_arr2


(* max? *)


(* In APL, A f.g B applies to both scalar, vector and matrix
   matrix and matrix multiplication: +.*
   v1 +.* v2
 *)
(*/hide*)

(* m * m *)
(*hide*)
(* Let's think mathematically first *)
(* if A=B*C then a_ij = ? and using dot what happens *)
(* a_ij = B[i;*] \dot C[*;j] = B[i;*] \dot C^T[j;*]
   About the notation of B[i;*]
*)

let row : (d2,'a) arr -> int -> (d1,'a) arr
    = function Arr ((nr1,nc1),xf) -> fun i ->
      Arr (nc1,fun j -> xf (i,j))

let to_rows : (d2,'a) arr -> (d1,(d1,'a)arr) arr
    = function (Arr ((nr1,nc1),_)) as arr ->
      Arr (nr1, row arr)

let to_rows : ('d1*'d2,'a) arr -> ('d1,('d2,'a)arr) arr
    = function Arr ((nr1,nc1),xf)  ->
      Arr (nr1, fun i -> Arr (nc1,fun j -> xf (i,j)))
(* from_rows? problem *)
(* related to curry? *)
(* See previous pr_arr2? Remember? *)

let matmul : (d2,int) arr -> (d2,int) arr -> (d2,int) arr
    = function Arr ((nr1,nc1), xf1) as m1 ->
      function Arr ((nr2,nc2), xf2) as m2 ->
      assert (nc1 = nr2);
      Arr ((nr1,nc2), fun (i,j) -> dot (row m1 i) (row (transpose m2) j))

(* Can you see the fork? *)

let matmul : (d2,int) arr -> (d2,int) arr -> (d2,int) arr
    = function Arr ((nr1,nc1), xf1) as m1 ->
      function Arr ((nr2,nc2), xf2) as m2 ->
      assert (nc1 = nr2);
      Arr ((nr1,nc2),
        fork dot
             (fst >> row m1)
             (snd >> row (transpose m2)))

(*
let fst : 'a * 'b -> 'a = fun (x,y) -> x

fst (1,2);;
snd (1,2);;
*)

let _ = matmul m1 (transpose m1) |> pr_arr2
(*
|5 14|
|14 50|
*)

(*/hide*)


(* In APL, A f.g B applies to both scalar, vector and matrix
    matrix and matrix multiplication: +.*
*)
(*hide*)
let matmul_gen : ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) ->
  (d2,'a) arr -> (d2,'a) arr -> (d2,'a) arr
    = fun f g ->
      function Arr ((nr1,nc1), _) as m1 ->
      function Arr ((nr2,nc2), _) as m2 ->
      assert (nc1 = nr2);
      Arr ((nr1,nc2), fun (i,j) ->
        dot_gen f g (row m1 i) (row (transpose m2) j))
(*/hide*)

(* application: 1 - minimum distance for all
d(0,1) = 10, d(1,2)=20, d(2,3)=15, d(0,3)=5
*)

(* pr_arr1, more generally (no loop): cons, snoc (append), intercalate *)
(*hide*)
let append : (d1,'a) arr -> (d1,'a) arr -> (d1,'a) arr
    = function Arr (upb1,xf1) ->
      function Arr (upb2,xf2) ->
        Arr (upb1+upb2+1, fun i -> if i <= upb1 then xf1 i else xf2 (i-upb1-1))

let _ = append (iota 3) (iota 4) |> pr_arr1

(* insert the value between each element *)
let intercalate : 'a -> (d1,'a) arr -> (d1,'a) arr
    = fun x -> function Arr (upb1,xf1) ->
      Arr (upb1*2, fun i -> if i mod 2 = 0 then xf1 (i / 2) else x)

let _ = iota 1 |> intercalate 10 |> pr_arr1
let _ = iota 2 |> intercalate 10 |> pr_arr1
let _ = iota 3 |> intercalate 10 |> pr_arr1
let _ = iota 4 |> intercalate 10 |> pr_arr1

(* Function like map but that does side effects *)
let iter : ('a -> unit) -> ('s,'a) arr -> unit
    = fun f -> function Arr (upb,xf) ->
      for i = 0 to upb do xf i |> f done

(* Now we can implement pr_arr1 with just operators. *)
let pr_arr1' : (d1,int) arr -> unit = fun arr ->
  let arr' = arr |> map string_of_int |> intercalate " " in
  append (append (of_array [|"|"|]) arr') (of_array [|"|\n"|]) |>
  iter print_string

(*
   append (append (of_array [|"|"|]) arr') (of_array [|"|\n"|]) |>
is ugly
let's be more beautiful

of_array [| of_array [|"|"|]; arr'; of_array [|"|\n"|] |]
  |> reduce append
APL-like
*)

let _ = iota 1 |> pr_arr1'
let _ = iota 2 |> pr_arr1'
let _ = iota 3 |> pr_arr1'



(* min/max *)
(* largest element of matrix *)
let _ = pr_arr2 m1
let _ = m1 |> to_rows |> map (reduce max) |> reduce max

(* largest and smallest element, at the same time *)
let minmax : 'a * 'a -> 'a * 'a -> 'a * 'a =
  fun (x1,y1) (x2,y2) -> (min x1 x2, max y1 y2)

let pair_dup : 'a -> 'a * 'a = fun x -> (x,x)

let _ = m1 |> pr_arr2
let _ = m1 |> to_rows |> map (map pair_dup >> reduce minmax) |> reduce minmax


let rho : ('bounds, 'a) arr -> 'bounds
    = function Arr (b, _) -> b

let get : ('bounds, 'a) arr -> 'bounds -> 'a
    = function Arr (b, xf) -> fun i -> xf i


(* materialize: as vector, as tile, etc *)

let materialize2 : 'a -> (d2,'a) arr -> (d2,'a) arr
 = fun iv -> function Arr ((upr,upc),xf) ->
   let arr = Array.make ((upr+1) * (upc+1)) iv in
   for i=0 to upr do
     for j=0 to upc do
       arr.(i*(upc+1) + j) <- xf (i,j)
     done
   done;
   Arr ((upr,upc), fun (i,j) -> arr.(i*(upc+1) + j))

let _ = m1 |> pr_arr2
let _ = m1 |> materialize2 (-1) |> pr_arr2

;;
