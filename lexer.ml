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
  src : string;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
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

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_alnum c = is_alpha c || is_digit c

let make_token tt value line col = { token_type = tt; value; line; col }

let read_while ls pred buf =
  while (match peek ls with Some c when pred c -> true | _ -> false) do
    Buffer.add_char buf (Option.get (peek ls));
    advance ls
  done

let read_string ls =
  let start_line = ls.line and start_col = ls.col in
  let buf = Buffer.create 64 in
  Buffer.add_char buf '"';
  advance ls;
  let rec loop () =
    match peek ls with
    | None ->
      Printf.eprintf "Error: unexpected string literal at line %d, col %d\n"
        start_line start_col;
      exit 1
    | Some '"' ->
      Buffer.add_char buf '"';
      advance ls
    | Some '\\' ->
      Buffer.add_char buf '\\';
      advance ls;
      (match peek ls with
       | Some c -> Buffer.add_char buf c; advance ls; loop ()
       | None ->
         Printf.eprintf "Error: unexpected escape sequence in string at line %d, col %d\n"
           start_line start_col;
         exit 1)
    | Some c ->
      Buffer.add_char buf c;
      advance ls;
      loop ()
  in
  loop ();
  make_token TStringLiteral (Buffer.contents buf) start_line start_col

let read_char ls =
  let start_line = ls.line and start_col = ls.col in
  advance ls;
  (match peek ls with
   | None ->
     Printf.eprintf "Error: unexpected char literal at line %d, col %d\n"
       start_line start_col;
     exit 1
   | Some '\'' ->
     Printf.eprintf "Error: empty char literal at line %d, col %d\n"
       start_line start_col;
     exit 1
   | _ -> ());
  let ch =
    match peek ls with
    | Some '\\' ->
      advance ls;
      (match peek ls with
       | None ->
         Printf.eprintf "Error: unexpected escape sequence in char literal at line %d, col %d\n"
           start_line start_col;
         exit 1
       | Some c ->
         let valid = match c with
           | 'n' | 't' | 'r' | '\\' | '\'' | '"' | '0' | 'a' | 'b' | 'f' | 'v' -> true
           | _ -> false
         in
         if not valid then begin
           Printf.eprintf "Error: invalid escape sequence '\\%c' in char literal at line %d, col %d\n"
             c start_line start_col;
           exit 1
         end;
         advance ls;
         Printf.sprintf "\\%c" c)
    | Some c ->
      advance ls;
      String.make 1 c
    | None -> assert false
  in
  (match peek ls with
   | Some '\'' -> advance ls
   | Some c ->
     Printf.eprintf "Error: char literal contains more than one character (got '%c') at line %d, col %d\n"
       c start_line start_col;
     exit 1
   | None ->
     Printf.eprintf "Error: unexpected char literal at line %d, col %d\n"
       start_line start_col;
     exit 1);
  make_token TCharLiteral (Printf.sprintf "'%s'" ch) start_line start_col

let read_number ls =
  let start_line = ls.line and start_col = ls.col in
  let buf = Buffer.create 32 in
  let is_float = ref false in
  read_while ls is_digit buf;
  (match peek ls, peek_at ls 1 with
   | Some '.', Some c when is_digit c ->
     is_float := true;
     Buffer.add_char buf '.';
     advance ls;
     read_while ls is_digit buf;
     (match peek ls, peek_at ls 1 with
      | Some '.', Some c when is_digit c ->
        Printf.eprintf "Error: multiple decimal points in float literal at line %d, col %d\n"
          start_line start_col;
        exit 1
      | _ -> ())
   | Some '.', Some '.' -> ()
   | _ -> ());
  (match peek ls with
   | Some ('e' | 'E') ->
     is_float := true;
     Buffer.add_char buf (Option.get (peek ls));
     advance ls;
     (match peek ls with
      | Some ('+' | '-') ->
        Buffer.add_char buf (Option.get (peek ls));
        advance ls
      | _ -> ());
     let exp_digits = ref 0 in
     while (match peek ls with Some c when is_digit c -> true | _ -> false) do
       Buffer.add_char buf (Option.get (peek ls));
       advance ls;
       incr exp_digits
     done;
     if !exp_digits = 0 then begin
       Printf.eprintf "Error: exponent has no digits in float literal at line %d, col %d\n"
         start_line start_col;
       exit 1
     end
   | _ -> ());
  let check_no_trailing_alpha suffix =
    match peek ls with
    | Some c when is_alpha c || c = '_' ->
      Printf.eprintf "Error: invalid numeric suffix '%s%c' at line %d, col %d\n"
        suffix c start_line start_col;
      exit 1
    | _ -> ()
  in
  (match peek ls with
   | Some 'u' ->
     if !is_float then begin
       Printf.eprintf "Error: invalid suffix 'u' on float literal at line %d, col %d\n"
         start_line start_col;
       exit 1
     end;
     Buffer.add_char buf 'u';
     advance ls;
     (match peek ls with
      | Some ('y' | 's' | 'L' as c) ->
        Buffer.add_char buf c;
        advance ls;
        check_no_trailing_alpha (Printf.sprintf "u%c" c)
      | Some c when is_alpha c ->
        Printf.eprintf "Error: invalid numeric suffix 'u%c' at line %d, col %d\n"
          c start_line start_col;
        exit 1
      | _ -> ())
   | Some ('y' | 's' | 'I' | 'L' as c) ->
     if !is_float then begin
       Printf.eprintf "Error: invalid suffix '%c' on float literal at line %d, col %d\n"
         c start_line start_col;
       exit 1
     end;
     Buffer.add_char buf c; advance ls;
     check_no_trailing_alpha (String.make 1 c)
   | Some ('f' | 'm' as c) ->
     is_float := true;
     Buffer.add_char buf c; advance ls;
     check_no_trailing_alpha (String.make 1 c)
   | Some c when is_alpha c || c = '_' ->
     Printf.eprintf "Error: invalid numeric suffix '%c' at line %d, col %d\n"
       c start_line start_col;
     exit 1
   | _ -> ());
  let tt = if !is_float then TFloatLiteral else TIntLiteral in
  make_token tt (Buffer.contents buf) start_line start_col

let read_identifier ls =
  let start_line = ls.line and start_col = ls.col in
  let buf = Buffer.create 32 in
  read_while ls (fun c -> is_alnum c || c = '_') buf;
  let s = Buffer.contents buf in
  let tt =
    if s = "true" || s = "false" then TBoolLiteral
    else if s = "_" then TWildcard
    else if is_keyword s then TKeyword
    else TIdentifier
  in
  make_token tt s start_line start_col

let skip_whitespace ls =
  while (match peek ls with Some (' ' | '\t' | '\r' | '\n') -> true | _ -> false) do
    advance ls
  done

let skip_line_comment ls =
  while (match peek ls with Some c when c <> '\n' -> true | _ -> false) do
    advance ls
  done

let skip_block_comment ls =
  let depth = ref 1 in
  advance ls; advance ls;
  while !depth > 0 do
    match peek ls with
    | None ->
      Printf.eprintf "Error: unexpected block comment\n";
      exit 1
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
  | Some ('=' | '+' | '*' | '-' | '<' | '>' | '%') ->
    let c = Option.get (peek ls) in
    advance ls;
    make_token TOperator (String.make 1 c) line col
  | Some ('(' | ')' | '[' | ']' | '{' | '}' | ';' | ',' | ':' | '.') ->
    let c = Option.get (peek ls) in
    advance ls;
    make_token TDelimiter (String.make 1 c) line col
  | Some c ->
    Printf.eprintf "Error: unexpected character '%c' at line %d, col %d\n" c line col;
    exit 1

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

let build_table predicate tokens =
  let seen = Hashtbl.create 16 in
  let id = ref 1 in
  List.filter_map (fun tok ->
    if predicate tok.token_type && not (Hashtbl.mem seen tok.value) then begin
      Hashtbl.add seen tok.value true;
      let r = (!id, tok.value) in
      incr id; Some r
    end else None
  ) tokens

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

  let kw_table = build_table (fun t -> t = TKeyword) tokens in
  print_table "KEYWORDS" kw_table;

  let op_table = build_table (fun t -> match t with
    | TOperator | TAssign | TArrow | TCompose | TRange -> true | _ -> false) tokens in
  print_table "OPERATORS" op_table;

  let delim_table = build_table (fun t -> match t with
    | TDelimiter | TArrayOpen | TArrayClose | TPipe -> true | _ -> false) tokens in
  print_table "DELIMITERS" delim_table;

  Printf.printf "\n------ LEXEME STREAM ------\n";
  Printf.printf "%s\n" stream
