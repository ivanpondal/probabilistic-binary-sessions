let parse_spec str =
  try
    let lexbuf = Lexing.from_string str in
    Parser.main Lexer.token lexbuf
  with
  | Lexer.UnexpectedCharacter ch ->
      print_endline ("unexpected character " ^ String.make 1 ch);
      exit 1
  | Parsing.Parse_error ->
      print_endline ("syntax error on line " ^ string_of_int (Lexer.get_line ()));
      exit 1

let parse_first_spec str =
  match parse_spec str with
  | x :: _ -> x
  | [] -> raise (Invalid_argument "Spec must have at least one item")

let get_spec_val_type spec =
  match spec with
  | Specification.Val (_, t) -> t
  | _ -> raise (Invalid_argument "Expected a specification val")

let parse_first_spec_type str = get_spec_val_type (parse_first_spec str)

let rec split_string s =
  try
    let i = String.index s '.' in
    let s1 = String.sub s 0 i in
    let s2 = String.sub s i (String.length s - i) in
    s1 :: split_string s2
  with Not_found -> [ s ]

let rec pp_specification = function
  | Specification.Type t -> Ast_processing.pp (Ast_processing.decode t)
  | Specification.Val (x, t) ->
      Format.open_hvbox 2;
      Format.print_string ("val " ^ x ^ " :");
      Format.print_break 1 0;
      Ast_processing.pp (Ast_processing.decode t);
      Format.close_box ()
  | Specification.Module (name, sl) ->
      Format.open_vbox 0;
      Format.open_vbox 2;
      Format.print_string ("module " ^ name ^ " : sig");
      Format.print_cut ();
      pp_specifications sl;
      Format.close_box ();
      Format.print_cut ();
      Format.print_string "end";
      Format.close_box ()

and pp_specifications = function
  | [] -> ()
  | [ x ] -> pp_specification x
  | x :: xs ->
      pp_specification x;
      Format.print_cut ();
      pp_specifications xs
