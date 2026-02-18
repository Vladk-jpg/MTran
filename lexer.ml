type token_type =
  | TKeyword
  | TIdentifier
  | TIntLiteral
  | TFloatLiteral
  | TStringLiteral
  | TCharLiteral
  | TBoolLiteral
  | TOperator
  | TDelimiter
  | TAssign
  | TArrow
  | TCompose
  | TRange
  | TArrayOpen
  | TArrayClose
  | TPipe
  | TWildcard
  | TEOF

type token = {
  token_type : token_type;
  value : string;
  line : int;
  col : int;
}

let keywords = [
  "let"; "mutable"; "while"; "do"; "for"; "to"; "in";
  "fun"; "if"; "then"; "else"; "match"; "with"; "type";
  "true"; "false";
]

let is_keyword s = List.mem s keywords

let ident_table : (int * string) list ref = ref []
let next_ident_id = ref 1

let lookup_or_add_ident value =
  match List.find_opt (fun (_, v) -> v = value) !ident_table with
  | Some (id, _) -> id
  | None ->
    let id = !next_ident_id in
    ident_table := !ident_table @ [(id, value)];
    incr next_ident_id;
    id

type lexer_state = {
  src  : string;
  mutable pos  : int;
  mutable line : int;
  mutable col  : int;
}

let make_lexer src = { src; pos = 0; line = 1; col = 1 }

let peek ls =
  if ls.pos < String.length ls.src then Some ls.src.[ls.pos]
  else None

let peek_at ls offset =
  let i = ls.pos + offset in
  if i < String.length ls.src then Some ls.src.[i]
  else None

let advance ls =
  if ls.pos < String.length ls.src then begin
    if ls.src.[ls.pos] = '\n' then begin
      ls.line <- ls.line + 1;
      ls.col <- 1
    end else
      ls.col <- ls.col + 1;
    ls.pos <- ls.pos + 1
  end

let current_char ls =
  if ls.pos < String.length ls.src then ls.src.[ls.pos]
  else '\000'

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_alnum c = is_alpha c || is_digit c

let make_token tt value line col = { token_type = tt; value; line; col }

let read_string ls =
  let start_line = ls.line and start_col = ls.col in
  let buf = Buffer.create 64 in
  Buffer.add_char buf '"';
  advance ls;
  let rec loop () =
    match peek ls with
    | None ->
      Printf.eprintf "Error: unterminated string literal at line %d, col %d\n"
        start_line start_col;
      ()
    | Some '"' ->
      Buffer.add_char buf '"';
      advance ls
    | Some '\\' ->
      Buffer.add_char buf '\\';
      advance ls;
      (match peek ls with
       | Some c -> Buffer.add_char buf c; advance ls; loop ()
       | None -> ())
    | Some c ->
      Buffer.add_char buf c;
      advance ls;
      loop ()
  in
  loop ();
  make_token TStringLiteral (Buffer.contents buf) start_line start_col

let read_char ls =
  let start_line = ls.line and start_col = ls.col in
  let buf = Buffer.create 8 in
  Buffer.add_char buf '\'';
  advance ls;
  let rec loop () =
    match peek ls with
    | None ->
      Printf.eprintf "Error: unterminated char literal at line %d, col %d\n"
        start_line start_col
    | Some '\'' ->
      Buffer.add_char buf '\'';
      advance ls
    | Some '\\' ->
      Buffer.add_char buf '\\';
      advance ls;
      (match peek ls with
       | Some c -> Buffer.add_char buf c; advance ls; loop ()
       | None -> ())
    | Some c ->
      Buffer.add_char buf c;
      advance ls;
      loop ()
  in
  loop ();
  make_token TCharLiteral (Buffer.contents buf) start_line start_col

let read_number ls =
  let start_line = ls.line and start_col = ls.col in
  let buf = Buffer.create 32 in
  let is_float = ref false in
  while (match peek ls with Some c when is_digit c -> true | _ -> false) do
    Buffer.add_char buf (current_char ls);
    advance ls
  done;
  (match peek ls, peek_at ls 1 with
   | Some '.', Some c when is_digit c ->
     is_float := true;
     Buffer.add_char buf '.';
     advance ls;
     while (match peek ls with Some c when is_digit c -> true | _ -> false) do
       Buffer.add_char buf (current_char ls);
       advance ls
     done
   | Some '.', Some '.' -> ()
   | _ -> ());
  (match peek ls with
   | Some ('e' | 'E') ->
     is_float := true;
     Buffer.add_char buf (current_char ls);
     advance ls;
     (match peek ls with
      | Some ('+' | '-') ->
        Buffer.add_char buf (current_char ls);
        advance ls
      | _ -> ());
     while (match peek ls with Some c when is_digit c -> true | _ -> false) do
       Buffer.add_char buf (current_char ls);
       advance ls
     done
   | _ -> ());
  (match peek ls with
   | Some 'u' ->
     Buffer.add_char buf 'u';
     advance ls;
     (match peek ls with
      | Some ('y' | 's' | 'L') ->
        Buffer.add_char buf (current_char ls);
        advance ls
      | _ -> ())
   | Some ('y' | 's' | 'I') ->
     Buffer.add_char buf (current_char ls);
     advance ls
   | Some 'L' ->
     Buffer.add_char buf (current_char ls);
     advance ls
   | Some 'f' ->
     is_float := true;
     Buffer.add_char buf (current_char ls);
     advance ls
   | Some 'm' ->
     is_float := true;
     Buffer.add_char buf (current_char ls);
     advance ls
   | _ -> ());
  let tt = if !is_float then TFloatLiteral else TIntLiteral in
  make_token tt (Buffer.contents buf) start_line start_col

let read_identifier ls =
  let start_line = ls.line and start_col = ls.col in
  let buf = Buffer.create 32 in
  while (match peek ls with Some c when is_alnum c || c = '_' -> true | _ -> false) do
    Buffer.add_char buf (current_char ls);
    advance ls
  done;
  let s = Buffer.contents buf in
  if s = "true" || s = "false" then
    make_token TBoolLiteral s start_line start_col
  else if s = "_" then
    make_token TWildcard s start_line start_col
  else if is_keyword s then
    make_token TKeyword s start_line start_col
  else
    make_token TIdentifier s start_line start_col

let skip_whitespace ls =
  while (match peek ls with
         | Some (' ' | '\t' | '\r' | '\n') -> true
         | _ -> false) do
    advance ls
  done

let skip_line_comment ls =
  while (match peek ls with
         | Some c when c <> '\n' -> true
         | _ -> false) do
    advance ls
  done

let skip_block_comment ls =
  let depth = ref 1 in
  advance ls; advance ls;
  while !depth > 0 do
    match peek ls with
    | None ->
      Printf.eprintf "Error: unterminated block comment\n";
      depth := 0
    | Some '(' when peek_at ls 1 = Some '*' ->
      incr depth; advance ls; advance ls
    | Some '*' when peek_at ls 1 = Some ')' ->
      decr depth; advance ls; advance ls
    | _ -> advance ls
  done

let rec next_token ls =
  skip_whitespace ls;
  let line = ls.line and col = ls.col in
  match peek ls with
  | None -> make_token TEOF "" line col
  | Some '/' when peek_at ls 1 = Some '/' ->
    skip_line_comment ls;
    next_token ls
  | Some '(' when peek_at ls 1 = Some '*' ->
    skip_block_comment ls;
    next_token ls
  | Some '"' -> read_string ls
  | Some '\'' -> read_char ls
  | Some c when is_digit c -> read_number ls
  | Some c when is_alpha c || c = '_' -> read_identifier ls
  | Some '<' when peek_at ls 1 = Some '-' ->
    advance ls; advance ls;
    make_token TAssign "<-" line col
  | Some '-' when peek_at ls 1 = Some '>' ->
    advance ls; advance ls;
    make_token TArrow "->" line col
  | Some '>' when peek_at ls 1 = Some '>' ->
    advance ls; advance ls;
    make_token TCompose ">>" line col
  | Some '.' when peek_at ls 1 = Some '.' ->
    advance ls; advance ls;
    make_token TRange ".." line col
  | Some '[' when peek_at ls 1 = Some '|' ->
    advance ls; advance ls;
    make_token TArrayOpen "[|" line col
  | Some '|' when peek_at ls 1 = Some ']' ->
    advance ls; advance ls;
    make_token TArrayClose "|]" line col
  | Some '|' ->
    advance ls;
    make_token TPipe "|" line col
  | Some ('=' | '+' | '*' | '<' | '>' | '%') ->
    let c = current_char ls in
    advance ls;
    make_token TOperator (String.make 1 c) line col
  | Some '-' ->
    advance ls;
    make_token TOperator "-" line col
  | Some ('(' | ')' | '[' | ']' | '{' | '}' | ';' | ',' | ':' | '.') ->
    let c = current_char ls in
    advance ls;
    make_token TDelimiter (String.make 1 c) line col
  | Some c ->
    Printf.eprintf "Error: unexpected character '%c' at line %d, col %d\n" c line col;
    advance ls;
    next_token ls

let tokenize src =
  let ls = make_lexer src in
  let tokens = ref [] in
  let rec loop () =
    let tok = next_token ls in
    tokens := tok :: !tokens;
    if tok.token_type <> TEOF then loop ()
  in
  loop ();
  List.rev !tokens

let needs_ident_entry = function
  | TIdentifier | TIntLiteral | TFloatLiteral
  | TStringLiteral | TCharLiteral | TBoolLiteral | TWildcard -> true
  | _ -> false

let build_keyword_table tokens =
  let seen = Hashtbl.create 16 in
  let result = ref [] in
  let id = ref 1 in
  List.iter (fun tok ->
    if tok.token_type = TKeyword && not (Hashtbl.mem seen tok.value) then begin
      Hashtbl.add seen tok.value true;
      result := (!id, tok.value) :: !result;
      incr id
    end
  ) tokens;
  List.rev !result

let build_operator_table tokens =
  let seen = Hashtbl.create 16 in
  let result = ref [] in
  let id = ref 1 in
  List.iter (fun tok ->
    let is_op = match tok.token_type with
      | TOperator | TAssign | TArrow | TCompose | TRange -> true
      | _ -> false
    in
    if is_op && not (Hashtbl.mem seen tok.value) then begin
      Hashtbl.add seen tok.value true;
      result := (!id, tok.value) :: !result;
      incr id
    end
  ) tokens;
  List.rev !result

let build_delimiter_table tokens =
  let seen = Hashtbl.create 16 in
  let result = ref [] in
  let id = ref 1 in
  List.iter (fun tok ->
    let is_delim = match tok.token_type with
      | TDelimiter | TArrayOpen | TArrayClose | TPipe -> true
      | _ -> false
    in
    if is_delim && not (Hashtbl.mem seen tok.value) then begin
      Hashtbl.add seen tok.value true;
      result := (!id, tok.value) :: !result;
      incr id
    end
  ) tokens;
  List.rev !result

let print_table title table =
  Printf.printf "\n------ %s ------\n" title;
  Printf.printf "%-10s | %s\n" "ID" "Value";
  Printf.printf "---------- | --------------------\n";
  List.iter (fun (id, value) ->
    Printf.printf "%-10d | %s\n" id value
  ) table

let build_lexeme_stream tokens =
  let buf = Buffer.create 256 in
  List.iter (fun tok ->
    match tok.token_type with
    | TEOF -> ()
    | _ when needs_ident_entry tok.token_type ->
      let id = lookup_or_add_ident tok.value in
      Buffer.add_string buf (Printf.sprintf "<ИД%d> " id)
    | _ ->
      Buffer.add_string buf tok.value;
      Buffer.add_char buf ' '
  ) tokens;
  Buffer.contents buf

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let () =
  let filename =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "test/test1.fsx"
  in
  let src =
    try read_file filename
    with Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  in
  let tokens = tokenize src in

  let stream = build_lexeme_stream tokens in

  print_table "CONSTANTS AND IDENTIFIERS" !ident_table;

  let kw_table = build_keyword_table tokens in
  print_table "KEYWORDS" kw_table;

  let op_table = build_operator_table tokens in
  print_table "OPERATORS" op_table;

  let delim_table = build_delimiter_table tokens in
  print_table "DELIMITERS" delim_table;

  Printf.printf "\n------ LEXEME STREAM ------\n";
  Printf.printf "%s\n" stream
