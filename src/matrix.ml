(**
 * An MxN matrix is a size M array filled with size N arrays.
 * 0..M-1 increases going down the rows of the matrix
 * 0..N-1 increases going right across the columns of the matrix
 * The use of the term 'rowwise' below means left to right, top
 * to bottom traversal.
 *)

let rec for_in_range ?(step=1) x y f =
  if x = y then ()
  else begin
    f x ;
    for_in_range ~step (x + step) y f
  end

let rec fold_in_range ?(step=1) x y a f =
  if x = y then a
  else fold_in_range ~step (x + step) y (f a x) f

type 'a matrix = 'a array array

let make : int -> int -> 'a -> 'a matrix =
  fun m n ->
    (if m < 1 then failwith "Cannot create a matrix with non-positive number of rows.") ;
    (if n < 1 then failwith "Cannot create a matrix with non-positive number of columns.") ;
    Array.make_matrix m n

let dim : 'x matrix -> int * int =
  fun mtx -> (Array.length mtx, Array.length mtx.(0))

let init : int -> int -> (int -> int -> 'x) -> 'x matrix =
  fun m n f -> Array.init m (fun m -> Array.init n (fun n -> f m n))

let get : 'x matrix -> int -> int -> 'x =
  fun mtx m n -> mtx.(m).(n)

let set : 'x matrix -> int -> int -> 'x -> unit =
  fun mtx m n x -> mtx.(m).(n) <- x

(* Folds a single column in the given matrix, top to bottom. *)
let fold_col : ('a -> 'x -> 'a) -> 'a -> 'x matrix -> int -> 'a =
  fun f a mtx n ->
    let mmax, _ = dim mtx in
    let rec loop a m =
      if m < mmax
      then loop (f a (get mtx m n)) (m+1)
      else a
    in
    loop a 0

(* Folds a single row in the given matrix, left to right. *)
let fold_row : ('a -> 'x -> 'a) -> 'a -> 'x matrix -> int -> 'a =
  fun f a mtx m -> Array.fold_left f a mtx.(m)

let mut_row : ('x -> 'x) -> 'x matrix -> int -> unit =
  fun f mtx m ->
    let _, nmax = dim mtx in
    for_in_range 0 nmax (fun n -> set mtx m n (f (get mtx m n)))

(* Folds over all rows of the matrix. *)
let fold_rowwise : ('a -> 'x -> 'a) -> 'a -> 'x matrix -> 'a =
  fun f -> Array.fold_left (Array.fold_left f)

(* Folds while exposing matrix indices. *)
let fold_rowwise_mn : (int -> int -> 'a -> 'x -> 'a) -> 'a -> 'x matrix -> 'a =
  fun f a mtx ->
    let mmax, nmax = dim mtx in
    let _, _, a =
      fold_rowwise
        (fun (m, n, a) x ->
          let n' = if n+1 < nmax then n+1 else 0 in
          let m' = if n' = 0     then m+1 else m in
          (m', n', f m n a x))
        (0, 0, a)
        mtx
    in
    a

let equal_matrix : ('x -> 'x -> bool) -> 'x matrix -> 'x matrix -> bool =
  fun xeq mtxA mtxB ->
    (dim mtxA = dim mtxB) &&
    fold_rowwise_mn
      (fun i j acc x -> acc && xeq x (get mtxB i j))
      true
      mtxA

(* Mutably maps over the matrix rowwise. *)
let mut_matrix : (int -> int -> 'x -> 'x) -> 'x matrix -> unit =
  fun f mtx ->
    let mmax, nmax = dim mtx in
    let rec loop m n =
      if n < nmax
      then
        (mtx.(m).(n) <- f m n (get mtx m n) ;
         loop m (n+1))
      else if m+1 < mmax
      then loop (m+1) 0
      else ()
    in
    loop 0 0

(* Iterates over the matrix rowwise. *)
let iter_matrix : (int -> int -> 'x -> unit) -> 'x matrix -> unit =
  fun f -> fold_rowwise_mn (fun m n _ x -> f m n x) ()

(* Maps over the matrix rowwise. *)
let map_matrix : (int -> int -> 'x -> 'y) -> 'x matrix -> 'y matrix =
  fun f mtx ->
    let mmax, nmax = dim mtx in
    init mmax nmax (fun m n -> f m n (get mtx m n))

let copy_matrix : 'x matrix -> 'x matrix = fun mtx -> map_matrix (fun _ _ x -> x) mtx

(* Mutably swaps rows of the given matrix. *)
let swap_rows : 'x matrix -> int -> int -> unit =
  fun mtx m m' ->
    let tmp = mtx.(m') in
    mtx.(m') <- mtx.(m) ;
    mtx.(m)  <- tmp

let (=.) : float -> float -> bool =
  let epsilon = 1.0e-10 in
  fun a b -> (abs_float (a-.b)) < epsilon

let clean mtx =
  map_matrix
    (fun _ _ x ->
       if x =. 0.
       then 0.
       else
       if x =. 1.
       then 1.
       else x)
    mtx

let scalar_mult_row : float matrix -> int -> float -> unit =
  fun mtx m c -> mut_row (( *. ) c) mtx m

type rowop =  Add of float * int * int
           | Mult of float * int
           | Swap of int * int
type trace = (float matrix * rowop list) list

(* imperative and aesthetically displeasing, but it works well *)
let rec rref : float matrix -> trace =
  fun mtx ->
    let mmax, nmax = dim mtx in
    let copy = copy_matrix mtx in
    let trace = ref [] in
    let snap () = clean copy in
    let next_pivot () =
      let rec loop i j =
        if j >= nmax
        then None
        else
        if i >= mmax
        then loop (j+1) (j+1)
        else match
          fold_in_range i mmax None
            (fun a m ->
               let coef = get copy m j in
               match a with
               | None when not (coef =. 0. || coef =. 1.) -> Some (m, coef)
               | Some (m', coef') when coef > coef'       -> Some (m, coef)
               | _ -> a)
          with
          | Some (m, c) -> Some (m, j, c)
          | None        -> loop (j+1) (j+1)
      in
      loop 0 0
    in
    let eliminate =
      fun m n c ->
        let ops = ref [] in
        for_in_range (m+1) mmax
          (fun m' ->
             let c' = get copy m' n in
             if c' =. 0.
             then ()
             else
               let ratio = 0. -. c' in
               ops := (Add (ratio, m, m'))::(!ops) ;
               for_in_range n nmax
                 (fun n' -> set copy m' n' ((ratio *. (get copy m n')) +. (get copy m' n')))) ;
        !ops
    in
    let backsub =
      fun m ->
        let ops = ref [] in
        let n =
          fold_in_range 0 nmax ~-1
            (fun a n -> match a with
               | -1 when not (get copy m n =. 0.) -> n
               | _ -> a)
        in
        (if n = ~-1 then ()
         else
           for_in_range 0 m
             (fun m' ->
                let ratio = 0. -. (get copy m' n) in
                ops := (Add (ratio, m, m'))::(!ops) ;
                for_in_range 0 nmax
                  (fun n' -> set copy m' n' ((ratio *. (get copy m n')) +. (get copy m' n'))))) ;
        !ops
    in
    let rec loop () =
      match next_pivot () with
      | Some (m, n, c) ->
        let start = snap () in
        swap_rows copy m n ;
        scalar_mult_row copy n (1. /. c) ;
        let elimops = eliminate n n c in
        let segment = (start,
                       elimops @
                       (if not (c =. 1.) then [Mult (1. /. c, n)] else []) @
                       (if m <> n  then [Swap (m, n)] else []))
        in
        trace := segment::(!trace) ;
        loop ()
      | None ->
        let start = snap () in
        let ops =
          fold_in_range ~step:~-1 (mmax-1) ~-1 []
            (fun ops m -> (backsub m)@ops)
        in
        trace := (start, ops)::(!trace)
    in
    loop () ;
    ((* oh *)snap (), [])::(!trace)

let random : int -> int -> float matrix =
  fun m n ->
    Random.self_init () ;
    init m n (fun _ _ -> float_of_int (Random.int 10))

let mult : float matrix -> float matrix -> float matrix =
  fun mtxL mtxR ->
    let mML, nML = dim mtxL
    and mMR, nMR = dim mtxR in
    (if nML <> mMR then failwith "Cannot multiple given matrices--bad dimensions!") ;
    let out = make mML nMR 0. in
    let rec loop i j k acc =
      if      i >= mML then out
      else if j >= nMR then loop (i+1) 0 0 0.
      else if k >= mMR then begin set out i j acc ; loop i (j+1) 0 0. end
      else loop i j (k+1) (acc +. (get mtxL i k *. get mtxR k j))
    in
    loop 0 0 0 0.

let augment : 'a matrix -> 'a matrix -> 'a matrix =
  fun mtxL mtxR ->
    let mML, nML = dim mtxL
    and mMR, nMR = dim mtxR in
    (if mML <> mMR then failwith "Cannot augment given matrices--bad dimensions!") ;
    init mML (nML+nMR)
      (fun i j ->
         if j >= nML
         then get mtxR i (j mod nML)
         else get mtxL i j)

let submatrix : 'a matrix -> int -> int -> int -> int -> 'a matrix =
  fun mtx m0 m n0 n ->
    init (m-m0) (n-n0) (fun i j -> get mtx (m0+i) (n0+j))

let ident : int -> float matrix =
  fun dim -> init dim dim (fun i j -> if i = j then 1. else 0.)

let invert : float matrix -> (float matrix * trace) option =
  fun mtx ->
    let m, n = dim mtx in
    (if m <> n then failwith "Cannot invert a non-square matrix.") ;
    let ident = ident m in
    let ((reduced, _)::_) as tr = rref (augment mtx ident) in
    let maybe_ident = submatrix reduced 0 m 0 n in
    let maybe_inverse = submatrix reduced 0 m n (2*n) in
    if equal_matrix (=) maybe_ident ident
    then Some (maybe_inverse, tr)
    else None
      
