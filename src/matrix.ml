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
[@@deriving eq, ord, show]

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

(* pretty printing a la http://mancoosi.org/~abate/ocaml-format-module *)
let pp_matrix : float matrix -> unit =
  let pp_header widths fmt header =
    let first_row = Array.map (fun x -> Bytes.make (x + 1) ' ') widths in
    Array.iteri (fun j cell ->
        Format.pp_set_tab fmt ();
        for z=0 to (Bytes.length header.(j)) - 1 do Bytes.set cell z header.(j).[z] done ;
        Format.fprintf fmt "%s" cell
      ) first_row
  in
  let pp_row pp_cell fmt row =
    Format.pp_open_hbox fmt () ;
    Array.iteri (fun j cell ->
        Format.pp_print_tab fmt ();
        Format.fprintf fmt "%a" pp_cell cell
      ) row ;
    Format.pp_close_box fmt ()
  in
  let pp_tables pp_row fmt (header, table) =
    let widths = Array.make (Array.length table.(0)) 0 in
    Array.iter (fun row ->
        Array.iteri (fun j cell ->
            widths.(j) <- max (String.length (string_of_float cell)) widths.(j)
          ) row
      ) table ;
    Array.iteri (fun j cell ->
        widths.(j) <- max (String.length cell) widths.(j)
      ) header;
    Format.pp_open_tbox fmt ();
    Format.fprintf fmt "%a@\n" (pp_header widths) header;
    Array.iter (pp_row fmt) table;
    Format.pp_close_tbox fmt ()
  in
  let pp_float : Format.formatter -> float -> unit =
    fun fmt f -> Format.fprintf fmt "%s" (string_of_float f)
  in
  fun mtx ->
    let header = Array.init (Array.length (mtx.(0))) (fun i -> "") in
    Format.fprintf Format.std_formatter "%a\n%!" (pp_tables (pp_row pp_float)) (header, mtx)
