let parse_type s =
  try
    let lexbuf = Lexing.from_string s in
    Parser.main Lexer.token lexbuf
  with
  | Lexer.UnexpectedCharacter ch ->
      print_endline ("unexpected character " ^ String.make 1 ch);
      exit 1
  | Parsing.Parse_error ->
      print_endline ("syntax error on line " ^ string_of_int (Lexer.get_line ()));
      exit 1