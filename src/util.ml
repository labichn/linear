let string_of_list ?(sep=", ") ?(border=(fun s -> "[" ^ s ^ "]")) soe l =
  if List.length l > 0 then
    let elts = List.fold_right (fun elt a -> (soe elt)^sep^a) l "" in
    border (String.sub elts 0 ((String.length elts)-(String.length sep)))
  else border ""

let trunc f =
  let digits_after_dot = 3 in
  let  sf = string_of_float f in
  let len = String.length sf in
  let ind = String.rindex sf '.' in
  if ind = len-1
  then String.sub sf 0 (len-1)
  else String.sub sf 0 (min (ind + digits_after_dot + 1) len)
