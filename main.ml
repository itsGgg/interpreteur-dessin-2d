open Ast
open Env_stack
open Graphics

(* -------- Variables globales & fonctions utilitaires --------*)
let lexbuf = Lexing.from_channel stdin 

let pile = ref (pile_init())
let is_down = ref false
let line_width = ref 1
let line_color=ref Graphics.black
let x= ref 400
let y= ref 400
let angle = ref 90 
let window_width = 800
let window_height = 800

exception Out_of_canvas of int * int

let print_error message =
  prerr_endline message;
  exit 1

let current_line () = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum

let calc_dst dst =
  let angle = float_of_int !angle *. Float.pi /. 180. in 
  let distance = float_of_int dst in
  let dx = int_of_float (distance *. cos angle) in
  let dy = int_of_float (distance *. sin angle) in
  let next_x = !x + dx in
  let next_y = !y + dy in
  if next_x < 0 || next_x >= window_width || next_y < 0 || next_y >= window_height then
    raise (Out_of_canvas (next_x, next_y));
  if !is_down=true
     then rlineto dx dy 
  else moveto next_x next_y;

  x := next_x;
  y := next_y
  
(* -------- Fonctions d'evaluation -------- *)
let rec eval_bloc b = 
  match b with
  | [] -> ()
  | x::xs -> let _ = eval_instruct x in eval_bloc xs 

and eval_instruct l=
  match l with
  | BaisserPinceau -> is_down := true
  | LeverPinceau -> is_down := false 
  | Avancer e -> let distance = eval_expression e in calc_dst distance
  | Tourner e -> angle := !angle - eval_expression e
  | CouleurPinceau c -> line_color:= eval_couleur c;
                        set_color !line_color;
  | LargeurPinceau e -> line_width:= eval_expression e;
                        set_line_width !line_width;
  | Var(s,e) -> (match e with
              | None -> pile := pile_declare !pile s None;
              | Some a -> 
                  let valeur = eval_expression a in 
                  pile := pile_declare !pile s (Some valeur));               
  | Affectation(s,e) ->
      let valeur = eval_expression e in
      pile := pile_assign !pile s valeur;
  | Si_alors (c,i) ->  if (eval_condition c)=true then (
                        pile := pile_push !pile;
                        (eval_instruct i);
                        pile := pile_pop !pile
                      )
  | Si_alors_sinon (c, i1, i2) ->(
                                  pile := pile_push !pile;
                                  if eval_condition c then 
                                    eval_instruct i1 
                                  else eval_instruct i2;
                                  pile := pile_pop !pile
                                )
    
  | Tant_que (c,i) ->  (
                        pile:=pile_push !pile;
                          while eval_condition c do
                            eval_instruct i
                          done;
                        pile:=pile_pop !pile;
                        )
  | Repeter (e,i) ->(
                      pile:=pile_push !pile;
                        let res_e = eval_expression e in
                        if res_e < 0 then
                          failwith (Printf.sprintf "Erreur d'execution : Repeter attend une valeur positive ou nulle, valeur reçue %d" res_e);
                        for _=1 to res_e do 
                          eval_instruct i 
                        done;
                      pile:=pile_pop !pile;
                    )
  | Debut_fin b ->( pile:=pile_push !pile;
                    eval_bloc b;
                    pile:=pile_pop !pile
                  )
and eval_expression e=
  match e with
  | Nombre n -> n
  | Nom_var id -> pile_lire_var !pile id
  | Operation (e1,op,e2)-> 
    let val1=eval_expression e1 in 
    let val2=eval_expression e2 in 
    eval_operator op val1 val2

and eval_operator op val1 val2= 
  match op with
  | "+" -> val1+val2
  | "-" -> val1-val2
  | "*" -> val1*val2
  | "/" -> if val2=0 then raise Division_by_zero 
            else val1/val2
  | _ -> failwith ("Operateur inconnu: " ^ op)

and eval_op_comp op val1 val2 = 
  match op with
  | ">" -> val1 > val2
  | ">=" -> val1 >= val2
  | "<" -> val1 < val2 
  | "<=" -> val1 <= val2 
  | "==" -> val1 = val2
  | "!=" -> val1 <> val2
  | _ -> failwith ("Operateur inconnu: " ^ op)

and eval_op_bool_binary op val1 val2=
  match op with
  | "||" -> val1 || val2
  | "&&" -> val1 && val2
  | _ -> failwith ("Operateur boolean binaire pas reconnu : " ^ op)
                                              
and eval_couleur c =
  match c with
  | Noir -> Graphics.black
  | Rouge -> Graphics.red
  | Bleu -> Graphics.blue
  | Vert -> Graphics.green 
  | Jaune -> Graphics.yellow
  | Hexadecimal h -> int_of_string h
and eval_condition c =
  match c with
  | Boolean b -> b  
  | Operation_comp(exp1,op,exp2) -> let val1= eval_expression exp1 in 
                                    let val2 = eval_expression exp2 in
                                    eval_op_comp op val1 val2 
  | Operation_bool_binaire(cond1,op,cond2) -> let val_cond1= eval_condition cond1 in
                                              let val_cond2 = eval_condition cond2 in
                                              eval_op_bool_binary op val_cond1 val_cond2
                                          
  | Operation_bool_unaire (_,cond) -> not (eval_condition cond)

(* -------- Fonction principale -------- *)
let run () =
  try
    let ast = Parser.programme Lexer.token lexbuf in
    Printf.printf "Parse:\n%s" (Ast.programe_to_string ast);
    open_graph " 800x800";
    set_window_title "Affichage";

    moveto !x !y;
    set_line_width !line_width;
    set_color !line_color;

    eval_bloc ast;
    
    ignore(read_key ());
    close_graph()
  with
  | Division_by_zero ->
                    print_error "Erreur d'execution : division par 0"
  | Out_of_canvas (nx, ny) ->
                    print_error ( Printf.sprintf "Erreur d'execution : le curseur sort du canevas aux coordonnees: %d, %d" nx ny)
  | Graphics.Graphic_failure msg ->
                    if msg = "graphic screen not opened" then ()
                    else print_error ("Erreur graphique : " ^ msg)
  | Unexpected_character (lexeme, pos) ->
                    let col = pos.pos_cnum - pos.pos_bol in
                    print_error (Printf.sprintf "Caractere inattendu \"%s\", ligne %d, colonne %d" lexeme pos.pos_lnum col)  
  | Failure msg ->
                    print_error (Printf.sprintf "Erreur : %s, ligne %d" msg (current_line()))
  | Parser.Error -> let pos = lexbuf.lex_curr_p in
                    let col = pos.pos_cnum - pos.pos_bol in
                    let tok = Lexing.lexeme lexbuf in
                        print_error
                          (Printf.sprintf
                            "Erreur de syntaxe, ligne %d, colonne %d, près de '%s'"
                            pos.pos_lnum col tok)
                      
let () = run ()
