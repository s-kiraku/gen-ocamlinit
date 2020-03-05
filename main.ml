open Genlex
open Format

let usage =
"show dependencies between OCaml modules of the form .ocamlinit.

Usage: ocamlinit [options] [files]

Example: ocamlinit -I _build -beta -mod-ml main.ml"

let command = "ocamldep -one-line -pp camlp4of "
let remove_suffix s = String.sub s 0 (String.length s - 3)


(* ---------------------------- *)
(* ---------------------------- *)
(** IO functions **)

let finally x f g =
  try 
    let y = f x in g x; y
  with
    e -> g x; raise e

let rec read_lines in_ch =
  try
    let line = input_line in_ch in
    line :: read_lines in_ch
  with End_of_file ->
    close_in in_ch;
    []
;;

let run line =
  finally 
    (Unix.open_process_in line)
    (fun ch -> read_lines ch)
    (fun ch -> ignore (Unix.close_process_in ch))

(* ------------------------------------- *)
(** Algorithm **)
(* create dependency graph from ocamldep *)
module Table = Map.Make(String)

open List

let concatMap f xs = concat (map f xs)
let subset xs ys = for_all (fun x -> mem x ys) xs

let rec uniq = function
  | []      -> []
  | x :: xs -> x :: filter (fun y -> y <> x) (uniq xs)

(* return list of (vertex, vertex) *)
let analyze0 cmd fs =
  let mls = map (fun f -> f ^ ".ml") fs in
  run (sprintf "%s %s" cmd (String.concat " " mls))
  |> map Dep.from_line

let update es table =
  let maybe x o = match o with
                    Some y -> Some y
                  | None   -> Some (uniq x) in
  fold_left
    (fun t (k,deps) -> Table.update k (maybe deps) t)
    table
    es

let rec analyze cmd fs table =
  match filter
    (fun f -> not (Table.mem f table))
    fs
  with
    [] -> table
  | unsolves ->
    let es = analyze0 cmd unsolves in
    let new_vertexes = uniq (concatMap snd es) in
    analyze cmd new_vertexes (update es table)

let analyzes cmd fs = analyze cmd fs Table.empty


(* functions for Graph *)
(* this program take number of any files, thus origin is not unique *)
let origins vs es =
  let out_edges v es = filter (fun (u,_) -> v = u) es in
  filter (fun v -> subset (out_edges v es) [v,v]) vs

let lex_sort nfs = sort String.compare nfs

let dig vs es =
  let org = origins vs es in
  let vs' = filter (fun v -> not (mem v org)) vs in
  let es' = filter (fun (_,v) -> not (mem v org)) es in
  org, vs', es'

(* align verticals by depth first search and lexicographic sort *)
let rec ordering0 vs0 es0 =
  match dig vs0 es0 with
    vs,[],_   -> vs
  | vs,vs',es -> lex_sort vs @ ordering0 vs' es

let ordering table =
  let vs = map fst (Table.bindings table) in
  let es = concatMap (fun v ->
                 map (fun v' -> v,v') (Table.find v table))
               vs in
  ordering0 vs es


(* ---------------------------- *)
(* ---------------------------- *)
(** Application **)

let dep       = ref ""
let pre_f     = ref ""
let suff_f    = ref ""
let quotation = ref false
let files     = ref []
let all_f     = ref false

let set_pre pre   () = pre_f  := pre
let set_suff suff () = suff_f := suff
let set_all pre () = 
  quotation := true;
  match pre with
  | "load"    -> pre_f := "#load "         ; suff_f := ".cmo"
  | "use"     -> pre_f := "#use "          ; suff_f := ".ml"
  | "mod_use" -> pre_f := "#mod_use "      ; suff_f := ".ml"
  | _         -> eprintf "set_all: Error@." ; exit 1

let all () =
  if not !all_f then
    ()
  else
    iter print_endline
      (* [ "#load \"dynlink.cma\""; *)
        (* "#load \"camlp4of.cma\""; *)
        ["#use \"topfind\"";
         "#require \"camlp4.listcomprehension\"";
         "#load \"camlp4of.cma\"";
         "#load \"unix.cma\"";
         "#load \"nums.cma\"" ]

let put pre suff opt files =
  (* preliminary *)
  let cmd = command ^ opt
  and fs = map remove_suffix files in
  (* solve dependency *)
  try
    let dependency = uniq (ordering (analyzes cmd fs)) in
    let printer s =
      let quote x = if !quotation then sprintf "%S" x else x in
      print_endline (pre ^ quote (s ^ suff))
    in
    iter printer dependency
  with Invalid_argument _ ->
    failwith "put: cannnot solve dependency"


(** setting for options **)
let item s = "", Arg.Unit (fun () -> ()), "\n"^s

let add_dep s        = dep := !dep ^ " " ^ s
let add_dep_with w s = add_dep (w ^ " " ^ s)
let dep_abbr s ()    = add_dep s
let extensions = [
  item "Extensions";
  ("-D" , Arg.String add_dep                , "<opts> throw <opts> to ocamldep");
  ("-I" , Arg.String (add_dep_with "-I")    , "<dir> same as: -D '-I dir'");
  ("-q" , Arg.Set quotation                 , " encompass file name by double quotation");
  ]

let pre_options = [
  item "Declarations";
  ("-prefix", Arg.Set_string pre_f         , "<str> print with prefix <str>");
  ("-load"  , Arg.Unit (set_pre "#load ")   , " print with prefix \'#load'");
  ("-use"   , Arg.Unit (set_pre "#use ")    , " print with prefix \'#use'");
  ("-mod"   , Arg.Unit (set_pre "#mod_use "), " print with prefix \'#mod_use'");
  ]

let suff_options = [
  item "Suffixs";
  ("-suffix", Arg.Set_string suff_f     , "<str> print with suffix <str>");
  ("-cmo"   , Arg.Unit (set_suff ".cmo"), " print with suffix \'.cmo'");
  ("-cma"   , Arg.Unit (set_suff ".cma"), " print with suffix \'.cma'");
  ("-ml"    , Arg.Unit (set_suff ".ml") , " print with suffix \'.ml'");
  ]

let abbr_options = [
  item "Compositions";
  ("-load-cmo" , Arg.Unit (set_all "load")   , " print with \'#load' and '.cmo'");
  ("-use-ml"   , Arg.Unit (set_all "use")    , " print with \'#use' and '.ml'");
  ("-mod-ml"   , Arg.Unit (set_all "mod_use"), " print with \'#mod_use' and '.ml'");
  ("-beta"      , Arg.Set all_f               , " add directives of Camlp4,Num and Unix")
  ]
let () = Arg.parse
    (Arg.align (extensions @ pre_options @ suff_options @ abbr_options @ [item ""]))
    (fun file -> files := file :: !files)
    usage
    ;
    all ();
    put !pre_f !suff_f !dep !files
