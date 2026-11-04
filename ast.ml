exception Unexpected_character of string * Lexing.position


type expression=
  | Nombre of int
  | Nom_var of string
  | Operation of expression * string * expression

type condition=
| Boolean of bool
| Operation_comp of expression*string *expression
| Operation_bool_binaire of condition*string*condition
| Operation_bool_unaire of string*condition

type couleur =
  | Noir 
  | Rouge
  | Bleu
  | Vert
  | Jaune
  | Hexadecimal of string

type bloc_instruction = instruction list

and instruction =
  | BaisserPinceau
  | LeverPinceau
  | Avancer of expression
  | Tourner of expression
  | CouleurPinceau of couleur 
  | LargeurPinceau of expression
  | Var of string*expression option
  | Affectation of string*expression

  | Si_alors of condition*instruction
  | Si_alors_sinon of condition*instruction*instruction
  | Tant_que of condition*instruction
  | Repeter of expression *instruction
  | Debut_fin of bloc_instruction

type programe = bloc_instruction

let rec expression_to_string = function
  | Nombre x -> string_of_int x
  | Nom_var x -> x
  | Operation (x, op, z) -> "(" ^ expression_to_string x ^ " " ^ op ^ " " ^ expression_to_string z ^ ")"

let rec condition_to_string = function
  | Boolean x -> (string_of_bool x)
  | Operation_comp (x,op,y) ->"(" ^expression_to_string x ^ op ^expression_to_string y ^")"
  | Operation_bool_binaire(x,op,y) -> "("^condition_to_string x ^op^condition_to_string y^")"
  | Operation_bool_unaire(op,x) -> "("^op^condition_to_string x^")"
let couleur_to_string=function
  | Noir -> "noir"
  | Rouge -> "rouge"
  | Bleu -> "bleu"
  | Vert -> "vert"
  | Jaune -> "jaune"
  | Hexadecimal h -> h

let decaler n =
  let nb_espaces = 2 * n in
  String.make nb_espaces ' '

let rec bloc_instruction_to_string profondeur instructions =
  match instructions with
  | [] -> ""
  | x :: xs -> instruction_to_string profondeur x ^ bloc_instruction_to_string profondeur xs

and instruction_to_string profondeur i =
  match i with
  | BaisserPinceau -> decaler profondeur ^ "BaisserPinceau ;\n"
  | LeverPinceau -> decaler profondeur ^ "LeverPinceau ;\n"
  | Avancer exp -> decaler profondeur ^ "Avancer " ^ expression_to_string exp ^ ";\n"
  | Tourner exp -> decaler profondeur ^ "Tourner " ^ expression_to_string exp ^ ";\n"
  | CouleurPinceau col -> decaler profondeur ^ "CouleurPinceau " ^ couleur_to_string col ^ ";\n"
  | LargeurPinceau exp-> decaler profondeur ^ "LargeurPinceau "^ expression_to_string exp ^ ";\n"
  | Var (nom,exp)-> decaler profondeur ^ "Var " ^nom ^
    (match exp with 
    | None -> ""
    | Some exp -> " = "^expression_to_string exp) ^ ";\n"
  | Affectation (nom,exp) -> decaler profondeur ^ nom ^" = "^(expression_to_string exp) ^ ";\n"
  | Si_alors(cond,inst) -> decaler profondeur ^ "Si "^condition_to_string cond ^" Alors\n"^ 
                            instruction_to_string (profondeur + 1) inst
  | Si_alors_sinon (cond,inst1,inst2) -> decaler profondeur ^ "Si "^ condition_to_string cond ^
                            " Alors\n" ^ instruction_to_string (profondeur + 1) inst1 ^
                            decaler profondeur ^ 
                            "Sinon\n" ^ instruction_to_string (profondeur + 1) inst2
  | Tant_que (cond,inst) -> decaler profondeur ^ "Tant que " ^ condition_to_string cond ^
                          "Faire\n" ^ instruction_to_string (profondeur + 1) inst
  | Repeter (exp,inst)-> decaler profondeur ^ "Repeter " ^expression_to_string exp ^" Fois\n" ^ 
                          instruction_to_string (profondeur + 1) inst
  | Debut_fin b -> decaler profondeur ^ "Debut\n"^ bloc_instruction_to_string (profondeur + 1) b ^
                  decaler profondeur ^ "Fin;\n"

let programe_to_string = bloc_instruction_to_string 0