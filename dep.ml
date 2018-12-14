open Genlex

(* supporting format (ocamldep -one-line file.ml) is defined by
  file.cmo : dependent1.cm[iox] ... dependentn.cm[iox]
*)
let lexer = make_lexer [":";".";"/"]

let rec parse_path = parser
  | [< 'Ident dir; path = parse_dir >]
    -> dir ^ path
and parse_dir = parser
  | [< 'Kwd "/"; path = parse_path >]
    -> "/" ^ path
  | [< >]
    -> ""

(* if parse multi line *)
let rec parse_deps = parser
  | [< dep = parse_line; deps = parse_deps >]
    -> dep :: deps
  | [< >] -> []
(* return associated list with dependency *)
and parse_line = parser
  (* file.cm* : dependent1.cm* dependent12.cm* ... *)
  | [< f,suffix = parse_name; 'Kwd ":"; fs = parse_names >]
      -> if suffix = "cmo" then
        [ f,d | d <- fs; f <> d]
      else
        []
and parse_names = parser
  | [< f,_ = parse_name; fs = parse_names >]
      -> f :: fs
  | [< >] -> []
and parse_name = parser
  (* file.suffix *)
  | [< f = parse_path; 'Kwd "."; 'Ident suffix >]
      -> f, suffix

(* parsing *)
let pp p str =
  let t = Stream.of_string str in
  p (lexer t)

let from_line  str = pp parse_line str

let from_lines str = pp parse_deps str
