open Matrix

let scalar_mult_row : float matrix -> int -> float -> unit =
  fun mtx m c -> mut_row (( *. ) c) mtx m

type rowop =  Add of float * int * int
           | Mult of float * int
           | Swap of int * int
type reduction_trace = (float matrix * rowop list) list

let rec rr : float matrix -> reduction_trace =
  fun mtx ->
    let mmax, nmax = dim mtx in
    let copy = copy_matrix mtx in
    let snap () = clean copy in
    let eliminate =
      fun m n c ->
        fold_in_range (m+1) mmax []
          (fun acc m' ->
             let c' = get copy m' n in
             if c' =. 0.
             then acc
             else begin
               let ratio = 0. -. c' in
               for_in_range n nmax
                 (fun n' -> set copy m' n' ((ratio *. (get copy m n')) +. (get copy m' n'))) ;
               (Add (ratio, m, m'))::acc
             end)
    in
    let rec next_pivot i0 j0 =
      let rec loop j =
        if j >= nmax then None
        else match
          fold_in_range i0 mmax None
            (fun a m ->
               let c = get copy m j in
               match a with
               | None          when not (c =. 0.) -> Some (m,  c)
               | Some (m', c') when not (c =. 0.) -> Some (m', c')
               | _ -> a)
          with
          | Some (m, c) -> Some (m, j, c)
          | None        -> loop (j+1)
      in
      loop j0
    in
    let rec loop tr pnum m0 n0 =
      match next_pivot m0 n0 with
      | Some (m, n, c) ->
        let start = snap () in
        (if m <> pnum then swap_rows copy m pnum) ;
        (if not (c =. 1.) then scalar_mult_row copy pnum (1. /. c)) ;
        let elimops = eliminate pnum n c in
        let seg_trace =
          elimops @
          (if not (c =. 1.) then [Mult (1. /. c, pnum)] else []) @
          (if m <> pnum then [Swap (m, pnum)] else [])
        in
        loop ((start, seg_trace)::tr) (pnum+1) (pnum+1) (n+1)
      | None -> tr
    in
    let trace = loop [] 0 0 0 in
    (snap (), [])::(List.filter (fun (_, tr) -> List.length tr > 0) trace)

let rec rref : float matrix -> reduction_trace =
  let backsub =
    fun mtx m ->
      let mmax, nmax = dim mtx in
      let n =
        fold_in_range 0 nmax ~-1
          (fun a n -> match a with
             | -1 when not (get mtx m n =. 0.) -> n
             | _ -> a)
      in
      (if n = ~-1
       then []
       else
         fold_in_range 0 m []
           (fun acc m' ->
              let ratio = 0. -. (get mtx m' n) in
              for_in_range 0 nmax
                (fun n' ->
                   set mtx m' n'
                     ((ratio *. (get mtx m n')) +. (get mtx m' n'))) ;
              if ratio =. 0.
              then acc
              else (Add (ratio, m, m'))::acc))
  in
  fun mtx ->
    let mmax, nmax = dim mtx in
    let (rr, [])::tr = rr mtx in
    let out = clean rr in
    let ops =
      fold_in_range ~step:~-1 (mmax-1) ~-1 []
        (fun ops m -> (backsub out m)@ops)
    in
    (out, [])::(rr, ops)::tr

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

let delsubmatrix : 'a matrix -> int -> int -> 'a matrix =
  fun mtx i' j' ->
    let m, n = dim mtx in
    assert (m > 1) ;
    init (m-1) (n-1)
      (fun i j ->
         let old_i = if i >= i' then i+1 else i
         and old_j = if j >= j' then j+1 else j in
         get mtx old_i old_j)

let ident : int -> float matrix =
  fun dim -> init dim dim (fun i j -> if i = j then 1. else 0.)

let invert_by_reduction : float matrix -> bool * float matrix * reduction_trace =
  fun mtx ->
    let m, n = dim mtx in
    (if m <> n then failwith "Cannot invert a non-square matrix.") ;
    let ident = ident m in
    let ((reduced, _)::_) as tr = rref (augment mtx ident) in
    let maybe_ident = submatrix reduced 0 m 0 n in
    let maybe_inverse = submatrix reduced 0 m n (2*n) in
    (equal_matrix (=) maybe_ident ident, maybe_inverse, tr)

let rec determinant : float matrix -> float =
  let num_zeros =
    fold_row
      (fun nz f -> if f = 0. then nz+1 else nz)
      0
  in
  fun mtx ->
    let m, n = dim mtx in
    assert (m = n) ;
    match m with
    | 1 -> get mtx 0 0
    | 2 -> (get mtx 0 0) *. (get mtx 1 1) -. (get mtx 0 1) *. (get mtx 1 0)
    | n ->
      let r =
        fold_in_range 1 m 0
          (fun msf m ->
             if num_zeros mtx m > num_zeros mtx msf
             then m
             else msf)
      in
      let det, _ =
        fold_row
          (fun (acc, j) aij -> match abs_float aij with
               | 0.  -> (acc, j+1)
               | aij -> (acc +. aij *. (cofactor mtx r j), j+1))
          (0., 0)
          mtx
          r
      in
      det

and cofactor : float matrix -> int -> int -> float =
  fun mtx i j ->
    (~-.1. ** (float_of_int (i+j))) *. (determinant (delsubmatrix mtx i j))

let adjugate : float matrix -> float matrix =
  fun mtx ->
    let m, n = dim mtx in
    clean (init m n (fun i j -> cofactor mtx j i))

let invert_by_cramer : float matrix -> float matrix =
  fun mtx ->
    let m, n = dim mtx in
    let det = determinant mtx in
    clean (init m n (fun i j -> cofactor mtx j i /. det))
