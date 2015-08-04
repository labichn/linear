open Matrix
open Ops
open Util

module M = Set.Make(struct type t = float matrix [@@deriving ord] end)

let col_space : float matrix -> float list list =
  fun mtx ->
    let (rr_mtx, _)::_ = rr mtx in
    let m, n = dim rr_mtx in
    let pivots =
      List.fold_right
        (fun n a -> match n with
           | Some i -> i::a
           | None   -> a)
        (fold_in_range 0 m []
           (fun l i ->
              (fold_in_range 0 n None
                 (fun o j -> match o with
                    | Some _                         -> o
                    | None when get rr_mtx i j =. 0. -> o
                    | None                           -> Some j))::l))
        []
    in
    List.fold_left
      (fun cols n -> (List.rev (fold_col (fun a f -> f::a) [] mtx n))::cols)
      []
      pivots

let null_space : float matrix -> float list list =
  fun mtx -> []

let rank : float matrix -> int =
  fun mtx -> List.length (col_space mtx)

let in_ref : float matrix -> bool =
  let pivots =
    fun mtx ->
      let m, n = dim mtx in
      List.rev
        (fold_in_range 0 m []
           (fun l i ->
              (fold_in_range 0 n None
                 (fun o j -> match o with
                    | Some _                      -> o
                    | None when get mtx i j =. 0. -> o
                    | None                        -> Some j))::l))
  in
  fun mtx ->
    let pivs = pivots mtx in
    match List.fold_left
            (fun a o -> match a, o with
               | `Ordered i, None                -> `Zero
               | `Ordered i, Some i' when i < i' -> `Ordered i'
               | `Ordered _, Some _
               | `Zero,      Some _              -> `Invalid
               | _ -> a)
            (`Ordered ~-1)
            (pivots mtx)
    with
    | `Invalid -> false
    | _        -> true

(*let num_pivots : float matrix (* in RREF *)

let cols_linearly_independent : float matrix -> bool =
  fun mtx ->
    let m, n = dim mtx in
    let aug = augment mtx (init m 1 (fun _ _ -> 0)) in
    match rref aug with
    | [] -> failwith "gah!"
    | (red, _) ->
      

let cols_linearly_dependent : float matrix -> bool =
  fun mtx ->
    let m, n = dim mtx in
    (n > m) || (* more vectors than entries in each vector, Theorem 8 (1.7) *)
    (fold_in_range 0 n false (* contains the zero vector, Theorem 9 (1.7) *)
       (fun acc j -> acc || fold_col (fun acc x -> acc && float_abs x =. 0.) true mtx j)) ||
    ()
*)
