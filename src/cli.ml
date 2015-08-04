open Matrix
open Ops
open Study
open Util

let mtx_begin typ =
  if typ = "g"
  then "\\begin{gmatrix}[b]"
  else "\\begin{bmatrix}"
let mtx_end   typ =
  if typ = "g"
  then "\\end{gmatrix}"
  else "\\end{bmatrix}"

let tex_matrix_data : float matrix -> string =
  fun mtx ->
    let mmax, nmax = dim mtx in
    fold_rowwise_mn
      (fun m n a f ->
         if n+1 = nmax && m+1 = mmax
         then Printf.sprintf "%s %s \n" a (trunc f)
         else
         if n+1 = nmax
         then Printf.sprintf "%s %s \\\\\n" a (trunc f)
         else Printf.sprintf "%s %s &" a (trunc f))
      ""
      mtx

let tex_reduction_trace : reduction_trace -> string =
  let tex_op = function
    | Add (f, m, m') -> Printf.sprintf "\\add[%s]{%i}{%i}" (trunc f) m m'
    | Mult (f, m)    -> Printf.sprintf "\\mult{%i}{\\cdot %s}" m (trunc f)
    | Swap (m, m')   -> Printf.sprintf "\\swap{%i}{%i}" m m'
  in
  let tex_ops =
    let rec loop a = function
      | []      -> a
      | op::ops -> loop (tex_op op ^ "\n" ^ a) ops
    in
    fun ops -> match ops with
      | []  -> ""
      | ops -> "\\rowops\n" ^ loop "" ops
  in
  let tex_elt (mtx, ops) =
    let str_mtx = tex_matrix_data mtx in
    let str_ops = tex_ops ops in
    mtx_begin "g" ^ "\n" ^ str_mtx ^ str_ops ^ mtx_end "g"
  in
  let rec loop a = function
    | elt::tr -> loop (tex_elt elt ^ "\n\\sim\n" ^ a) tr
    | [] -> a
  in
  fun trace ->
   string_of_list
      ~sep:"\n\\sim\n"
      ~border:(fun s -> "\\(" ^ s ^ "\\)")
      tex_elt
      (List.rev trace)

let tex_matrix : string -> float matrix -> string =
  fun typ mtx -> mtx_begin typ ^ "\n" ^ tex_matrix_data mtx ^ mtx_end typ

let read_mtx : unit -> float matrix =
  let split on str =
    List.map String.trim (Str.split (Str.regexp on) str)
  in
  let rec loop acc =
    let inp = read_line () in
    if inp <> ""
    then loop (acc ^ "\n" ^ inp)
    else acc
  in
  fun () ->
    try
      let llf : float list list =
        List.map
          (fun l -> List.map float_of_string (split " " l))
          (split "\n" (loop ""))
      in
      let n0 = List.length (List.hd llf) in
      (if List.exists (fun l -> List.length l <> n0) llf then
         failwith "Given matrix has inconsistent row lengths.") ;
      Array.of_list (List.map Array.of_list llf)
    with _ -> print_endline "Bad input for matrix." ; exit 2

let study_matrix mtx =
  let in_ref = in_ref mtx in
  let   rank = rank mtx in
  let   cols = col_space mtx in
  let   null = null_space mtx in
  Printf.printf
    ("It is%s in row-echelon form.\n" ^^
     "Rank:         %i\n" ^^
     "Column Space: %s\n" ^^
     "Null Space:   %s\n" ^^
     "%!")
    (if in_ref then "" else " not")
    rank
    (string_of_list ~sep:"," ~border:(fun s->"{"^s^"}")
       (string_of_list ~sep:" " ~border:(fun s->"("^s^")") string_of_float)
       cols)
    (string_of_list ~sep:"," ~border:(fun s->"{"^s^"}")
       (string_of_list ~sep:" " ~border:(fun s->"("^s^")") string_of_float)
       null)

let _ =  
  let cmd_name = Sys.argv.(0) in
  let usage =
    Printf.sprintf
      ("Usage: %s {-row-reduce, -multiply} [-tex-out {b, g}]\n")
      cmd_name
  in

  let tex_mtx    = ref "" in
  let row_reduce = ref false in
  let row_reduce_echelon = ref false in
  let multiply   = ref false in
  let inv        = ref false in
  let inv_cramer = ref false in
  let adj        = ref false in
  let det        = ref false in
  let study      = ref false in

  Arg.parse
    (Arg.align
       [ ("-tex-out",     Arg.Set_string tex_mtx,
          "{g, b} -- outputs latex rather than pretty printing")
       ; ("-rr",          Arg.Set row_reduce,
          "       -- performs row reduction on the given matrix")
       ; ("-rref",        Arg.Set row_reduce_echelon,
          "       -- performs full row reduction on the given matrix")
       ; ("-multiply",    Arg.Set multiply,
          "       -- multiplies the two given matrices")
       ; ("-adjugate",    Arg.Set adj,
          "       -- computes a matrix's adjugate")
       ; ("-invert",      Arg.Set inv,
          "       -- inverts the given matrix by matrix reduction")
       ; ("-invert-cramer",      Arg.Set inv_cramer,
          "       -- inverts the given matrix by Cramer's rule")
       ; ("-det",         Arg.Set det,
          "       -- computes the determinant of a matrix")
       ; ("-study",       Arg.Set study,
          "       -- get a bunch of info about the given matrix")
       ])
    (fun anon_arg -> failwith ("Unexpected anonymous argument: " ^ anon_arg))
    usage ;

  if !row_reduce
  then begin
    let mtx = read_mtx () in
    let ((out, _)::_) as tr = rr mtx in
    match !tex_mtx with
    |  "" -> pp_matrix out
    | "g" -> print_endline (tex_reduction_trace tr)
    | "b" -> print_endline (tex_matrix "b" out)
  end
  else if !row_reduce_echelon
  then begin
    let mtx = read_mtx () in
    let ((out, _)::_) as tr = rref mtx in
    match !tex_mtx with
    |  "" -> pp_matrix out
    | "g" -> print_endline (tex_reduction_trace tr)
    | "b" -> print_endline (tex_matrix "b" out)
  end
  else if !multiply
  then begin
    print_endline "Multiplying matrices AB" ;
    print_endline "Matrix A:" ;
    let a = read_mtx () in
    print_endline "Matrix B:" ;
    let b = read_mtx () in
    let out = mult a b in
    match !tex_mtx with
    | ""        -> pp_matrix out
    | "v"       ->
      let out =
        "\\(\n" ^
        (tex_matrix "b" a) ^
        "\n" ^
        (tex_matrix "b" b) ^
        "\n=\n" ^
        (tex_matrix "b" out) ^
        "\\)"
      in
      print_endline out
    | typ -> print_endline (tex_matrix typ out)
  end
  else if !inv
  then begin
    print_endline "Attempting to invert matrix." ;
    let mtx = read_mtx () in
    let (invertible, inv, tr) = invert_by_reduction mtx in
    (match !tex_mtx with
     |  "" -> pp_matrix inv
     | "b" -> print_endline (tex_matrix "b" inv)
     | "g" ->
       print_endline (tex_reduction_trace tr) ;
       print_endline (tex_matrix "g" inv)) ;
    if not invertible
    then print_endline "Could not invert the given singular matrix."
  end
  else if !det
  then begin
    print_endline "Calculating the determinant by cofactor expansion." ;
    let mtx = read_mtx () in
    let det = determinant mtx in
    print_endline (string_of_float det)
  end
  else if !inv_cramer
  then begin
    print_endline "Inverting matrix by Cramer's rule." ;
    let mtx = read_mtx () in
    let inv = invert_by_cramer mtx in
    (match !tex_mtx with
     | "" -> pp_matrix inv
     | ty -> print_endline (tex_matrix ty inv))
  end
  else if !adj
  then begin
    print_endline "Calculating the adjugate." ;
    let mtx = read_mtx () in
    let adj = adjugate mtx in
    (match !tex_mtx with
     | "" -> pp_matrix adj
     | ty -> print_endline (tex_matrix ty adj))
  end
  else if !study
  then study_matrix (read_mtx ())
  else if !tex_mtx <> ""
  then print_endline (tex_matrix !tex_mtx (read_mtx ()))

  else print_endline "Doing nothing, for no reason."
  
