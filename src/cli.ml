open Matrix

let param_tex_mtx = ref ""
let mtx_begin () =
  if !param_tex_mtx = "g"
  then "\\begin{gmatrix}[b]"
  else "\\begin{bmatrix}"
let mtx_end   () =
  if !param_tex_mtx = "g"
  then "\\end{gmatrix}"
  else "\\end{bmatrix}"

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


let trunc f =
  let digits_after_dot = 3 in
  let  sf = string_of_float f in
  let len = String.length sf in
  let ind = String.rindex sf '.' in
  if ind = len-1
  then String.sub sf 0 (len-1)
  else String.sub sf 0 (min (ind + digits_after_dot + 1) len)

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

let tex_trace : trace -> string =
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
    mtx_begin () ^ str_mtx ^ str_ops ^ mtx_end ()
  in
  let rec loop a = function
    | elt::tr -> loop (tex_elt elt ^ "\n\\sim\n" ^ a) tr
    | [] -> a
  in
  fun tr -> "\\(" ^ (loop "" tr) ^ "\\)"

let tex_matrix : float matrix -> string =
  fun mtx -> mtx_begin () ^ "\n" ^ tex_matrix_data mtx ^ mtx_end ()

let tex_doc : trace -> string =
  let doc_wrap body =
    (*"\usepackage{amsmath}\n" ^*)
    "\\documentclass[11pt]{article}\n" ^
    "\\usepackage{gauss}\n" ^
    "\n" ^
    "\\begin{document}\n" ^
    body ^
    "\\end{document}\n"
  in
  fun trace -> doc_wrap (tex_trace trace)

let render_mult mtxL mtxR =
  print_endline (tex_matrix mtxL) ;
  print_endline (tex_matrix mtxR) ;
  print_endline "=" ;
  print_endline (tex_matrix (mult mtxL mtxR))

let read_mtx : unit -> float matrix =
  let split on str =
    List.map String.trim (Str.split (Str.regexp on) str)
  in
  let rec loop acc = function
    | 0 -> acc
    | n -> loop (acc ^ "\n" ^ (read_line ())) (n-1)
  in
  fun () ->
    print_string "Enter the dimensions of the matrix (MxN): " ;
    let dim = read_line () in
    let m, n =
      try
        let[@warning "-8"] [mstr; nstr] = split "x" (String.lowercase dim) in
        (int_of_string mstr, int_of_string nstr)
      with _ -> print_endline "Bad format for dimensions." ; exit 1
    in
    Printf.printf "Enter %i space separated float entries in %i rows:\n%!"
      n m ;
    let mtx =
      try
        let lolos = List.map (split " ") (split "\n" (loop "" m)) in
        let lolof = List.map (List.map float_of_string) lolos in
        if List.length lolof <> m || List.exists (fun l -> List.length l <> n) lolof
        then failwith "Matrix has incorrect dimensions."
        else Array.of_list (List.map Array.of_list lolof)
      with _ -> print_endline "Bad input for matrix." ; exit 2
    in
    mtx

let _ =  
  let cmd_name = Sys.argv.(0) in
  let usage =
    Printf.sprintf
      ("Usage: %s {-row-reduce, -multiply} [-tex-out {b, g}]\n")
      cmd_name
  in

  let row_reduce = ref false in
  let multiply   = ref false in
  let inv        = ref false in

  Arg.parse
    (Arg.align
       [ ("-tex-out",     Arg.Set_string param_tex_mtx,
          "{g, b} -- outputs latex rather than pretty printing")
       ; ("-row-reduce",  Arg.Set row_reduce,
          "       -- performs row reduction on the given matrix")
       ; ("-multiply",    Arg.Set multiply,
          "       -- multiplies the two given matrices")
       ; ("-invert",      Arg.Set inv,
          "       -- inverts the given matrix")
       ])
    (fun anon_arg -> failwith ("Unexpected anonymous argument: " ^ anon_arg))
    usage ;

  if !row_reduce
  then begin
    let      mtx = read_mtx () in
    let ((out, _)::_) as tr = rref mtx in
    match !param_tex_mtx with
    |  "" -> pp_matrix out
    | "g" -> print_endline (tex_trace tr)
    | "b" -> print_endline (tex_matrix out)
  end
  else if !multiply
  then begin
    print_endline "Multiplying matrices AB" ;
    print_string "Matrix A--" ;
    let a = read_mtx () in
    print_string "Matrix B--" ;
    let b = read_mtx () in
    let out = augment a b in
    if !param_tex_mtx = ""
    then pp_matrix out
    else print_endline (tex_matrix out)
  end
  else if !inv
  then begin
    print_endline "Attempting to invert matrix" ;
    let mtx = read_mtx () in
    match invert mtx with
    | Some (inv, tr) ->
      (match !param_tex_mtx with
       |  "" -> pp_matrix inv
       | "g" -> print_endline (tex_trace tr) ; print_endline (tex_matrix inv)
       | "b" -> print_endline (tex_matrix inv))
    | None -> print_endline "Cannot invert singular matrix."
  end

  else print_endline "Doing nothing, for no reason."
  
