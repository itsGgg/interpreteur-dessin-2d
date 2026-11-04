type pile_env = (string * int option) list list

let pile_init() = [[]]
let pile_push pile = [] :: pile
let pile_pop pile =
  match pile with
  | [] -> failwith "pile_pop error : Pile vide"
  | [_] -> pile
  | _::xs -> xs 

let rec remplacer_var liste nom_var valeur =
  match liste with
  | [] -> []
  | (n, v) :: ys ->
      if n = nom_var then
        (n, Some valeur) :: ys
      else
        (n, v) :: remplacer_var ys nom_var valeur

let pile_declare pile nom_var var_value =
  match pile with
  | [] -> failwith "pile_declare : pile vide"
  | x :: xs ->
      if List.mem_assoc nom_var x then
        failwith "variable deja declaree dans ce scope"
      else
        ((nom_var, var_value) :: x) :: xs
let rec pile_assign pile nom_var valeur =
  match pile with
  | [] -> failwith "variable non declaree"
  | x :: xs ->
      if List.mem_assoc nom_var x then
        (remplacer_var x nom_var valeur) :: xs
      else
        x :: pile_assign xs nom_var valeur

let rec pile_lire_var pile nom_var =
  match pile with
  | [] -> failwith ("variable " ^ nom_var ^ " non declaree")
  | x :: xs ->
      match List.assoc_opt nom_var x with
      | Some (Some v) -> v
      | Some None ->
          failwith ("variable " ^ nom_var ^ " non initialisee")
      | None ->
          pile_lire_var xs nom_var