{
  open Parser
  open Ast
}
let nombre = '-'? ['0'-'9']+
let whitespace = [' ' '\t' '\r']+
let hexadecimal = ['0'-'9' 'A'-'F' 'a'-'f']


rule token = parse
  | whitespace { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "LargeurPinceau" {LARGEUR_PINCEAU}
  | "BaisserPinceau" {BAISSER_PINCEAU}
  | "LeverPinceau" {LEVER_PINCEAU}
  | "Avancer" {AVANCER}
  | "Tourner" {TOURNER}
  | "CouleurPinceau" {COULEUR_PINCEAU}
  | "Var" {VAR}
  | "Debut" {DEBUT}
  | "Fin" {FIN}
  | "Si" {SI}
  | "Alors" {ALORS}
  | "Sinon" {SINON}
  | "Tant que"{TANT_QUE}
  | "Faire" {FAIRE}
  | "Repeter"{REPETER}
  | "Fois" {FOIS}
  | "vrai" {VRAI}
  | "faux" {FAUX}
  | "noir" {NOIR}
  | "rouge" {ROUGE}
  | "bleu" {BLEU}
  | "vert" {VERT}
  | "jaune" {JAUNE}
  | nombre as n { ENTIER (int_of_string n) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { ID id}
  | hexadecimal hexadecimal hexadecimal hexadecimal hexadecimal hexadecimal as h {HEXADECIMAL h}
  | "&&" {ET}
  | "||" {OU}
  | "==" {EGALITE}
  | "!=" {DIFFERENT}
  | "<=" {PLUS_PETIT_OU_EGAL}
  | ">=" {PLUS_GRAND_OU_EGAL}
  | "=" {EGAL}
  | "<" {PLUS_PETIT}
  | ">" {PLUS_GRAND}
  | "!" {NOT}
  | ")" {RPAR}
  | "(" {LPAR}
  | "+" {ADDITION}
  | "-" {SUBTRACTION}
  | "*" {MULTIPLICATION}
  | "/" {DIVISION}
  | ";" {POINT_VIRGULE}
  | eof { EOF }
  | _ {
      let lexeme = Lexing.lexeme lexbuf in 
      let pos = lexbuf.lex_curr_p in 
      raise (Unexpected_character (lexeme,pos))
      }