(* An Array Programming Language *)
(*
#use "intro2021.ml";;
*)

(* 入門 *)

(* arrayとは *)

(* 
内容と形
1 2 3 4 5 6

1 2 3
4 5 6

1 2
3 4
5 6

具体的に、形とは？
1 2 3 4 5 6
 a_0 = 1
 a_1 = 2
 ...
 a_5 = 6
それは、[0..5] -> [1..6]関数だね
そんな関数は、arrayの意味して、arrayを表現している
(因みに、Fortranでは、関数の呼出しとarrayから要素をとるのは、同じ表記だ: A(I))


[0..5](関数の定義域:domain)は、 形を表す
関数のrangeは、arrayの内容域

0 0 0 0 0 0
ならば、
それは、[0..5] -> 0関数で表現するね
[0..5]部分は(domain,形)前と同じけど、内容(range)が違う

1 2 3
4 5 6
場合は、
a_00 = 1, a_01 = 2, ...
[0..1] x [0..2] -> [1..6]の関数
domainが違ったが、rangeが同じ

1 2
3 4
5 6
場合は？

それは、[0..5] -> int
のような関数は、直接にOCaml上であんまり表現できないけど、間接にできる
*)

type ('i,'a) arr = Arr of 'i * ('i -> 'a)

(*
Arr (upb,indexf)
   upb: 最大index
   最小indexは明示的に指定しない (0の様に)

  だから、空arrayは存在しない
 *)

(*
最後に、scalarは、そのように表現できる？
*)

(* <hide> *)
(* scalarはrank=0のarrayである *)

let one : (unit,int) arr = Arr ((),fun _ -> 1)
(* </hide> *)
(*
scalarは、vectorの一種 (APLにとって)
でも、代数なで、scalarとvectorが全く別のもの
我々も、scalarとvectorを区別しよう
*)


(* vector: rank=1のarray *)

type d1 = int                     (* 本来、自然数 *)

(* 表示しよう *)
(* 以下は、今のところ、飛しょう *)

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


(* 2*v しよう *)
(*hide*)
(* 数学的な式を考えよう: y_i = 2*x_i
   OCaml表記は、数学表記と近い
 *)
let mul1 : int -> (d1,int) arr -> (d1,int) arr
    = fun n -> function Arr (upb,a) ->
      Arr(upb, fun i -> n * (a i))

(* 数学での合成。知ってる？ *)
(* 左から右まで合成 *)
let (>>) f g = fun x -> f x |> g

(* |>と>>の関係？

x |> f |> g === x |> (f >> g)
数学での合成と比べて
*)

(* まえのcurryingについての話 *)
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
(* a >> fの意味: fはaのrangeを変換する  fはaのrangeに適用する
array本体での処理のヒント
*)

(* 2*v+1
   僕等の演算順序 対 APLの順序
 *)
(*hide*)
let _ = pr_arr1 a1
let _ = a1 |> map1 (( * ) 2) |> map1 ((+) 1) |> pr_arr1

let _ = a1 |> (map1 (( * ) 2) >> map1 ((+) 1)) |> pr_arr1

let _ = a1 |> map1 ( ( * ) 2 >> (+) 1) |> pr_arr1

(*
map1 f >> map1 g === 
map1 (f >> g)
意味が同じ、効率が少し高める

symbolのいじる話
map1のような規則を(目が閉まったままさえ)適用すると、必ず正しい結果が出る
(意味保存)。
Algebra上での変換
*)
(*/hide*)

let iota : int -> (d1,int) arr = fun n ->
  assert (n>0);
  Arr(n-1,fun i -> i)

(* idは本当に基本だよ *)
let iota : int -> (d1,int) arr = fun n ->
  assert (n>0);
  Arr(n-1,Fun.id)

let _ = iota 5 |> pr_arr1

(*hide*)
let _ = iota 5 |> rev |> pr_arr1

let _ = iota 5 |> map1 (( * ) 2) |> rev |> pr_arr1
(*/hide*)


(*
a1では、内容はメモリに保存した。iotaは？
iota 5でも、iota 5000000でも、メモリ量が同じよ。

Unixでは、/dev/zero, /dev/randomなど

OSとPL(APL)は、とても別の世界なのに見えるけど。
原理/理念を理解するのは、何より。
*)

(* x+y *)
(*hide*)
(* 対応: 0 -> a_0 + b_0
         1 -> a_1 + b_1
         ...
一般化？ それを文字通りに実装
 *)
let add2 : (d1,int) arr -> (d1,int) arr -> (d1,int) arr
    = fun (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fun i -> (xf i + yf i))
(*/hide*)

(* そのままいいけど、そんな形の式は、よく起るよ。
APLでは、forkと喚ばれる
APLが提供する; OCamlで、自分で定義できるよ
APLでは、hookとforkが、大事なので、空白で表現する
*)

(* 有名なS combinator *)
(* `combinator'って、引数を並び換え、合成 *)
let hook : ('x -> 'a -> 'b) -> ('x -> 'a) -> 'x -> 'b =
  fun f g x -> f x (g x)

(* 型をよく見て 何かに気が付く？ *)
let fork : ('a->'b->'c) -> ('x->'a) -> ('x->'b) -> ('x->'c) = 
  fun h f g x -> h (f x) (g x)
(* a->b->cという関数は
(x->a)と(x->b)の引数を取らせる

*)

(* MapReduceという言葉は知ってる？ 
   Googleでの検索とMapReduce
*)

(* forkとhookは、どんな関係？ 
hookをforkによって表現。forkは、hookによって？
h (f x) (g x) = (h (f x)) (g x) = (f >> h) x (g x)
Scombinatorの意義を確認しよう
*)
(*
図を描く
hookの図 と forkの図、相互対応(変換)

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

(* 数学は、patternの科学だよ *)

(* いま、APLのように見える *)
(* lambdaあり *)
let add2 : (d1,int) arr -> (d1,int) arr -> (int,int) arr
    = fun (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fun i -> fork (+) xf yf i)

(* lambdaなし *)
let add2 : (d1,int) arr -> (d1,int) arr -> (int,int) arr
    = fun (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fork (+) xf yf)

let _ = pr_arr1 a1
let _ = a1 |> add2 (iota 7) |> pr_arr1

(* 一般化しよう *)
(* まず、型をよく見て 何かに気が付く？ *)
let zip_with : ('a -> 'b -> 'c) -> 
           (int,'a) arr -> (int,'b) arr -> (int,'c) arr
 (* add2の型を見ると、別に何も思い浮ばない。一方、zipはmap1と比べて
   一般化には、価値がある
 *)
 (*hide*)
    = fun f (Arr (upbx,xf)) (Arr (upby,yf)) ->
      assert (upbx = upby);
      Arr(upbx, fork f xf yf)

let _ = a1 |> zip_with (+) (iota 7) |> pr_arr1
(*/hide*)

(* APLでは、zip_withも、大事なので、空白で表現する *)

(* binary演算を考えよう op: 'a -> 'b -> 'c
引数は、x:'a, y:'bなら、op x y (infixなら(APLように)、x op y)
引数は、x:'t->'a, y:'t->'bなら、fork op x y (APLでそのまま: x op y)
引数は、x:(d1,'a)arr, y:(d1,'b)arrなら、zip_with op x y (APLでそのまま: x op y)

fork, zip_withは、2引数の適用を表す。深い関係。だから、APLで空白で表現される

unary場合: op: 'a -> 'b
x:'aなら、op x
x:'t->'aなら、fun t -> x t |> op === x >> op
x:(d1,'a)arrなら、map op x

合成とmapは、同じように、深い関係。同じように、APLで空白で表現される
*)
   

(* sum_{i=1}^{5} i^2をやろう (TeX表記を使えば) *)
(*hide*)
(* 一般的にといえば、sum_{i=0}^{n-1} a_i *)
(*再帰を使っても良いならば  初めて、再帰が現れる *)
(* 結合は、右から、左から？ *)
(*
  sum_{i=0}^{n-1} a_i
  = a_0 +  sum_{i=1}^{n-1} a_i
  = (a_0 + a_1) + sum_{i=2}^{n-1} a_i
  = (a_0 + a_1 + ... a_{k-1}) + sum_{i=k}^{n-1} a_i
     もう足した                   まだ
*)
let sum : (d1, int) arr -> int = function Arr (upb, xf) ->
  (* acc: もうたした i:それから、まだ *)
  let rec loop acc i =
    if i > upb then acc else loop (acc + xf i) (i+1)
  in loop (xf 0) 1
(* APLで、+/ と呼ばれる *)
(*/hide*)

(* 一歩一歩式を伸ばして *)
let _ = iota 5 |> map1 succ |> map1 (fun x -> x * x) |> 
  pr_arr1

let _ = iota 5 |> map1 succ |> map1 (fun x -> x * x) |> 
  sum

let sqr : int -> int = fun x -> x * x


let _ = iota 5 |> map1 succ |> map1 sqr |> sum

let _ = iota 5 |> (map1 succ >> map1 sqr) |> sum

let _ = iota 5 |> map1 (succ >> sqr) |> sum


(* 一般化 左から結合して. APL出は、/と呼ばれる *)
let reduce : ('a -> 'a -> 'a) -> (d1, 'a) arr -> 'a
  = fun f -> function Arr (upb, xf) ->
    let rec loop acc i =
      if i > upb then acc else loop (f acc (xf i)) (i+1)
    in loop (xf 0) 1

(* 想像:
reduce op a = a_0 op a_1 op a_2 ... op a_(n-1)
 *)
let _ = iota 10 |> map1 succ |> reduce (+)
let _ = iota 10 |> map1 succ |> reduce ( * )  (* factorial 階乗 *)
let _ = iota 10 |> map1 succ |> reduce min
let _ = iota 10 |> map1 succ |> reduce max

(* 統計
  APL/J: mean = +/ % #
  var = (- mean) ....
 *)

(*hide*)
let length : (d1,'a) arr -> int = function Arr(upb,_) -> upb + 1



(* iota & length *)


(* 流れを見よう flowchartを描こう 
型の話
*)
let mean : (d1,int) arr -> float = 
  fork (fun x y -> float_of_int x /. float_of_int y) (reduce (+)) length

let mean : (d1,float) arr -> float = 
  fork (/.) (reduce (+.)) (length >> float_of_int)

(* Gary Kildallの説明を参照 *)

(* 先ず 
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

let standard_dev : (d1,float) arr -> float = 
  let sqrf x = x *. x in
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
     map1 (fun x -> x *. x) |> reduce (+.) |> 
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

(* 後で: 等価性
let _ = assert ([|1;2|] = [|1;3|])

a1;;
match a1 with Arr (n,xf) -> xf 2;;

let arr_cmp : (d1,'a) arr -> (d1,'a) arr -> bool

let _ = assert (a1 = (a1 |> rev |> rev))
*)


(* dotの定義しよう APLとどう違うの *)
(*hide*)
let dot : (int,int) arr -> (int,int) arr -> int = fun arr1 arr2 ->
  zip_with ( * ) arr1 arr2 |> sum

let _ = let v = iota 5 in dot v v
let _ = dot (of_array [|0;1|]) (of_array [|5;0|])
(* 上記、詳しくしよう *)
(*/hide*)

(* もっと一般化 *)
(*hide*)
let dot_gen : ('a -> 'a -> 'a) ->
              ('a -> 'a -> 'a) ->
              (int,'a) arr -> (int,'a) arr -> 'a 
  = fun f g arr1 arr2 ->
  zip_with f arr1 arr2 |> reduce g

(*
前のdot = dot_gen ( * ) (+)
*)

(* lambdaなし？ (かき) *)

(* APLでは、g.f *)
let _ = let v1 = iota 5 and v2 = iota 5 |> map1 succ |> rev in
        dot_gen (fun x y -> x - y |> abs) (+) v1 v2

(* 他の内積の様な処理？
 距離、例えば、(2,1)と(3,3)との距離、何？
*)
let sqrf : float -> float = fun x -> x *. x

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
add/min, add/max (ある町から町数までの距離)
 and/or (同じのテストを合格。でも、どんなテスト？

 等価性との関係: L1、L2、Linf norm


 距離: zip_with (-) a1 a2 |> map sqr |> reduce (+)
 =     zip_with (fun x y -> x - y |> sqr) a1 a2 |> reduce (+)
forkの話を覚えよう。patternに気が付くよ

定義しよう

dt op1 op2: fun x y -> op1 x y |> op2
*)

let dt : ('a -> 'b -> 'c) -> ('c -> 'd) ->
  ('a -> 'b -> 'd) = fun f g -> fun x y -> f x y |> g

let dot_gen : ('a -> 'b -> 'c) -> ('c -> 'c -> 'c) ->
  ((d1,'a) arr -> (d1,'b) arr -> 'c) 
  = fun f g -> dt (zip_with f) (reduce g)

(* 前述で
let _ = assert (a1 = (a1 |> rev |> rev))
どうして問題が起るの？ どうして関数比較できないの

normについての話 L_1, L_2, L_inf 
*)
let distinf : (d1,int) arr -> (d1,int) arr -> int =
  dt (zip_with (dt (-) abs)) (reduce max) 

let distinf : (d1,int) arr -> (d1,int) arr -> int =
  dot_gen (dt (-) abs) max

let _ = distinf (iota 5) (iota 5 |> map1 sqr)
let _ = distinf a1 (a1 |> rev |> rev)

(* dtは関数に活用すれば *)
let dtf : ('a -> 'b -> 'c) -> ('c -> 'd) ->
  (('t -> 'a) -> ('t -> 'b) -> ('t -> 'd)) = 
  fun f g -> fun x y -> fun t -> f (x t) (y t) |> g

let dtf : ('a -> 'b -> 'c) -> ('c -> 'd) ->
  (('t -> 'a) -> ('t -> 'b) -> ('t -> 'd)) = 
  fun f g -> dt (fork f) ((>>) g)

(*
関数なら
             fun x y -> fun t -> op1 (x t) (y t) |> op2
             = fun x y -> fork op1 x y >> op2
             = dt (fork op1) ((>>) op2)
arrなら
dot_gen op1 op1:
             fun x y -> zip_with op1 x y |> reduce op2
             = dt (zip_with op1) (reduce op2)

だから
 距離: zip_with (-) x y |> map sqr |> reduce (+)
 =     zip_with (fun x y -> x - y |> sqr) x y |> reduce (+)
 =     dt (zip_with (dt (-) sqr)) (reduce (+))
*)


(* ------------------------------------------------------------------------
   2次元 
*)
type d2 = int * int

(* 2次元のarrayの例 *)
let mm1 = Arr ((1,1), function
  | (0,0) -> 1
  | (0,1) -> 2
  | (1,0) -> 3
  | (1,1) -> 4
 )

let rho2 : d2 -> (d1,'a) arr -> (d2,'a) arr 
 (* まず、自分で *)
 (*hide*)
 = fun (nr,nc) (Arr (upb,xf)) ->
   assert (nr*nc = upb+1);
   Arr((nr-1,nc-1), fun (i,j) -> i*nc + j |> xf)

(*
 i*nc + jに対して、定義？
lambdaなしで
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

(* 宿題: 
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
(* lambdaなし *)

let _ = m1 |> pr_arr2
let _ = m1 |> add2 1 |> pr_arr2
(*/hide*)
(* mapの一般化 *)

(*hide*)
let map2 : ('a -> 'b) -> (d2,'a) arr -> (d2,'b) arr
    = fun f -> function Arr (upbs,xf) ->
      Arr(upbs, fun (i,j) -> xf (i,j) |> f)
(*/hide*)

(* もっと般化？ *)
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
(* map1と同じ *)

let _ = m1 |> map succ |> pr_arr2

let _ = iota 6 |> map succ |> pr_arr1

let _ = one |> map succ |> (function Arr ((), xf) -> xf ())

(* 基本的な関数なので、APLで明示的に書かない
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


(* APLでは、A f.g Bは、scalarもvectorもmatrixも対象
   matrixとmatrixかけ算: +.*
   v1 +.* v2
 *)
(*/hide*)

(* m * m *)
(*hide*)
(* まず、数学的に考えよう *)
(* A=B*Cなら、a_ij = ? そして、dotを用いて、どうなる *)
(* a_ij = B[i;*] \dot C[*;j] = B[i;*] \dot C^T[j;*]  
   B[i;*]の表記について
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
(* from_rows? 問題 *)
(* curryと関係？ *)
(* 以前のpr_arr2をみよう？ 思い出す？ *)

let matmul : (d2,int) arr -> (d2,int) arr -> (d2,int) arr 
    = function Arr ((nr1,nc1), xf1) as m1 -> 
      function Arr ((nr2,nc2), xf2) as m2 -> 
      assert (nc1 = nr2);
      Arr ((nr1,nc2), fun (i,j) -> dot (row m1 i) (row (transpose m2) j))

(* forkが見えるの？ *)

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


(* APLでは、A f.g Bは、scalarもvectorもmatrixも対象
   matrixとmatrixかけ算: +.*
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

(* 応用: 1ー全ての最小距離 
d(0,1) = 10, d(1,2)=20, d(2,3)=15, d(0,3)=5
*)

(* pr_arr1, もっと一般的に (loopなし): cons, snoc (append), intercalate *)
(*hide*)
let append : (d1,'a) arr -> (d1,'a) arr -> (d1,'a) arr
    = function Arr (upb1,xf1) ->
      function Arr (upb2,xf2) ->
        Arr (upb1+upb2+1, fun i -> if i <= upb1 then xf1 i else xf2 (i-upb1-1))

let _ = append (iota 3) (iota 4) |> pr_arr1

(* 各要素の間にある値を入れる *)
let intercalate : 'a -> (d1,'a) arr -> (d1,'a) arr 
    = fun x -> function Arr (upb1,xf1) ->
      Arr (upb1*2, fun i -> if i mod 2 = 0 then xf1 (i / 2) else x)

let _ = iota 1 |> intercalate 10 |> pr_arr1
let _ = iota 2 |> intercalate 10 |> pr_arr1
let _ = iota 3 |> intercalate 10 |> pr_arr1
let _ = iota 4 |> intercalate 10 |> pr_arr1

(* mapのような副作用をやる関数 *)
let iter : ('a -> unit) -> ('s,'a) arr -> unit 
    = fun f -> function Arr (upb,xf) ->
      for i = 0 to upb do xf i |> f done

(* 今、pr_arr1を演算子だけで実装出来る。 *)
let pr_arr1' : (d1,int) arr -> unit = fun arr ->
  let arr' = arr |> map string_of_int |> intercalate " " in
  append (append (of_array [|"|"|]) arr') (of_array [|"|\n"|]) |>
  iter print_string

(*
  append (append (of_array [|"|"|]) arr') (of_array [|"|\n"|]) |>
は醜いね。
もっと綺麗二しよう

of_array [| of_array [|"|"|]; arr'; of_array [|"|\n"|] |]
 |> reduce append
APLらしい
*)

let _ = iota 1 |> pr_arr1'
let _ = iota 2 |> pr_arr1'
let _ = iota 3 |> pr_arr1'



(* min/max *)
(* matrixの最大の要素 *)
let _ = pr_arr2 m1
let _ = m1 |> to_rows |> map (reduce max) |> reduce max

(* 最大と最小の要素、同時に *)
let minmax : 'a * 'a -> 'a * 'a -> 'a * 'a = 
  fun (x1,y1) (x2,y2) -> (min x1 x2, max y1 y2)

let pair_dup : 'a -> 'a * 'a = fun x -> (x,x)

let _ = m1 |> pr_arr2
let _ = m1 |> to_rows |> map (map pair_dup >> reduce minmax) |> reduce minmax


let rho : ('bounds, 'a) arr -> 'bounds
    = function Arr (b, _) -> b

let get : ('bounds, 'a) arr -> 'bounds -> 'a
    = function Arr (b, xf) -> fun i -> xf i


(* materialize: vectorとして、tileとして、など *)

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
