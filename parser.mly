%{
  let is_hex6 s =
  if String.length s <> 6 then false
  else
    String.for_all (fun c ->
      match c with
      | '0'..'9' -> true
      | 'a'..'f' -> true
      | 'A'..'F' -> true
      | _ -> false
    ) s
%}

%{
open Ast
%}

%token EOF ADDITION SUBTRACTION MULTIPLICATION DIVISION LPAR RPAR BAISSER_PINCEAU LEVER_PINCEAU POINT_VIRGULE AVANCER TOURNER
%token <int> ENTIER

// etape 2
%token LARGEUR_PINCEAU
%token COULEUR_PINCEAU
%token NOIR ROUGE BLEU VERT JAUNE
%token <string> HEXADECIMAL

// etape 3 
%token VAR
%token <string> ID
%token EGAL

// etape 4
%token SI ALORS SINON TANT_QUE FAIRE REPETER FOIS 
%token VRAI FAUX
%token EGALITE DIFFERENT PLUS_PETIT PLUS_PETIT_OU_EGAL PLUS_GRAND PLUS_GRAND_OU_EGAL (* ==,!=,<,<=,>,>=*)
%token ET OU NOT (*&& || ! *)
%token DEBUT FIN

%start<Ast.programe> programme
%type <Ast.bloc_instruction> bloc_instructions
%type <Ast.condition> condition
%type <Ast.couleur> couleur
%type <Ast.expression> expression
%type <Ast.instruction> instruction

%left ADDITION SUBTRACTION 
%left MULTIPLICATION DIVISION 
%left OU
%left ET
%right NOT (* choisir le NOT le plus à droite*)
%nonassoc ALORS 
%nonassoc SINON (* resolution du dangling else*)


%%

programme: b=bloc_instructions EOF { b }

bloc_instructions :
  | { [] }
  | x=instruction POINT_VIRGULE xs=bloc_instructions { x :: xs }

instruction :
  | BAISSER_PINCEAU { BaisserPinceau }
  | LEVER_PINCEAU { LeverPinceau }
  | LARGEUR_PINCEAU exp=expression { LargeurPinceau exp}
  | COULEUR_PINCEAU col=couleur { CouleurPinceau col}
  | AVANCER exp=expression { Avancer exp }
  | TOURNER exp=expression { Tourner exp }
  | VAR id=ID { Var(id,None)}
  | VAR id=ID EGAL exp=expression { Var(id,Some exp)}
  | id=ID EGAL exp=expression{ Affectation(id,exp)}
 
  | SI cond=condition ALORS inst=instruction {Si_alors(cond,inst)}
  | SI cond=condition ALORS inst1=instruction SINON inst2=instruction{Si_alors_sinon(cond,inst1,inst2)}
  | TANT_QUE cond=condition FAIRE inst=instruction {Tant_que(cond,inst)}
  | REPETER exp=expression FOIS inst=instruction {Repeter(exp,inst)}
  | DEBUT bloc=bloc_instructions FIN {Debut_fin(bloc)}

condition : 
  | VRAI {Boolean true} 
  | FAUX {Boolean false}
  | LPAR cond=condition RPAR {cond}
  | exp1=expression EGALITE exp2=expression { Operation_comp(exp1,"==",exp2)}
  | exp1=expression DIFFERENT exp2=expression { Operation_comp(exp1,"!=",exp2)}
  | exp1=expression PLUS_PETIT exp2=expression { Operation_comp(exp1,"<",exp2)}
  | exp1=expression PLUS_PETIT_OU_EGAL exp2=expression { Operation_comp(exp1,"<=",exp2)}
  | exp1=expression PLUS_GRAND exp2=expression { Operation_comp(exp1,">",exp2)}
  | exp1=expression PLUS_GRAND_OU_EGAL exp2=expression { Operation_comp(exp1,">=",exp2)}
  | cond1=condition ET cond2=condition{Operation_bool_binaire(cond1,"&&",cond2) }
  | cond1=condition OU cond2=condition{Operation_bool_binaire(cond1,"||",cond2) }
  | NOT cond=condition {Operation_bool_unaire("!",cond)}


expression:
  | ent=ENTIER { Nombre ent }
  | id=ID { Nom_var id }
  | LPAR e=expression RPAR { e }
  | e1=expression ADDITION e2=expression { Operation (e1, "+", e2) }
  | e1=expression SUBTRACTION e2=expression { Operation (e1, "-", e2) }
  | e1=expression MULTIPLICATION e2=expression { Operation (e1, "*", e2) }
  | e1=expression DIVISION e2=expression { Operation (e1, "/", e2) }

couleur:
  | NOIR {Noir}
  | ROUGE { Rouge}
  | BLEU { Bleu}
  | VERT {Vert}
  | JAUNE { Jaune}
  | h=HEXADECIMAL {Hexadecimal ("0x"^h) }
  | id=ID {
    if is_hex6 id then
      Hexadecimal("0x"^id)
    else 
      failwith ("Erreur de syntaxe : La couleur " ^ id ^ " n'est pas un code hexadécimal valide")  
    }