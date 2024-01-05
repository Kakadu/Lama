let failwiths ppf = Format.kasprintf failwith ppf
let make_kv k v = `Assoc [ ("kind", `String k); ("value", v) ]
let loc_to_json (a, b) = `List [ `Int a; `Int b ]

let json_to_loc : _ -> Language.Loc.t = function
  | `List [ `Int a; `Int b ] -> (a, b)
  | _ -> failwiths "Not implemented: %s %d" __FILE__ __LINE__

let json_to_patt : _ -> SM.patt = function
  | `String "StrCmp" -> StrCmp
  | `String "String" -> String
  | `String "Array" -> Array
  | `String "Sexp" -> Sexp
  | `String "Boxed" -> Boxed
  | `String "UnBoxed" -> UnBoxed
  | `String "Closure" -> Closure
  | _ -> failwiths "Not implemented: %s %d" __FILE__ __LINE__

let patt_to_json = function
  | SM.StrCmp -> `String "StrCmp"
  | String -> `String "String"
  | Array -> `String "Array"
  | Sexp -> `String "Sexp"
  | Boxed -> `String "Boxed"
  | UnBoxed -> `String "UnBoxed"
  | Closure -> `String "Closure"

let dsgn_to_json = function
  | Language.Value.Global s -> make_kv "Global" (`String s)
  | Fun s -> make_kv "Fun" (`String s)
  | Local s -> make_kv "Local" (`Int s)
  | Arg s -> make_kv "Arg" (`Int s)
  | Access s -> make_kv "Access" (`Int s)

let json_to_dsgn : Yojson.Safe.t -> Language.Value.designation = function
  | `Assoc [ ("kind", `String k); ("value", `String s) ]
    when String.lowercase_ascii k = "global" ->
      Global s
  | `Assoc [ ("kind", `String k); ("value", `String s) ]
    when String.lowercase_ascii k = "fun" ->
      Fun s
  | `Assoc [ ("kind", `String k); ("value", `Int n) ]
    when String.lowercase_ascii k = "local" ->
      Local n
  | `Assoc [ ("kind", `String k); ("value", `Int n) ]
    when String.lowercase_ascii k = "arg" ->
      Arg n
  | `Assoc [ ("kind", `String k); ("value", `Int n) ]
    when String.lowercase_ascii k = "access" ->
      Access n
  | _ -> failwiths "Not implemented: %s %d" __FILE__ __LINE__

let dsgn_of_json = json_to_dsgn

let rec scope_to_json { SM.blab; elab; names; subs } : Yojson.Safe.t =
  `Assoc
    [
      ("blab", `String blab);
      ("elab", `String elab);
      ( "names",
        `List (List.map (fun (s, n) -> `List [ `String s; `Int n ]) names) );
      ("subs", `List (List.map scope_to_json subs));
    ]

(** TODO: order of fields should not matter  *)

let rec json_to_scope : Yojson.Safe.t -> SM.scope = function
  | `Assoc
      [
        ("blab", `String blab);
        ("elab", `String elab);
        ("names", `List names);
        ("subs", `List subs);
      ] ->
      let names =
        names
        |> List.map (function
             | `List [ `String s; `Int n ] -> (s, n)
             | _ -> failwith "Bad input")
      in
      { SM.blab; elab; names; subs = List.map json_to_scope subs }
  | _ -> failwiths "Bad input in %s %d" __FILE__ __LINE__

let scope_of_json = json_to_scope

let to_insn : Yojson.Safe.t -> SM.insn = function
  | `Assoc [ ("kind", `String k); ("value", `String v) ]
    when String.uppercase_ascii k = "BINOP" ->
      BINOP v
  | `Assoc [ ("kind", `String k); ("value", `Int v) ]
    when String.uppercase_ascii k = "CONST" ->
      CONST v
  | `Assoc [ ("kind", `String k); ("value", `String v) ]
    when String.uppercase_ascii k = "STRING" ->
      STRING v
  | `Assoc [ ("kind", `String k); ("name", `String v); ("value", `Int n) ]
    when String.uppercase_ascii k = "SEXP" ->
      SEXP (v, n)
  | `Assoc [ ("kind", `String k); ("value", v) ]
    when String.uppercase_ascii k = "LD" ->
      LD (json_to_dsgn v)
  | `Assoc [ ("kind", `String k); ("value", v) ]
    when String.uppercase_ascii k = "LDA" ->
      LDA (json_to_dsgn v)
  | `Assoc [ ("kind", `String k); ("value", v) ]
    when String.uppercase_ascii k = "ST" ->
      ST (json_to_dsgn v)
  | `Assoc [ ("kind", `String k) ] when String.uppercase_ascii k = "STI" -> STI
  | `Assoc [ ("kind", `String k) ] when String.uppercase_ascii k = "STA" -> STA
  | `Assoc [ ("kind", `String k) ] when String.uppercase_ascii k = "ELEM" ->
      ELEM
      (* **************** **************** ***************** ******************** *)
  | `Assoc [ ("kind", `String k); ("value", `String v) ]
    when String.uppercase_ascii k = "LABEL" ->
      LABEL v
  | `Assoc [ ("kind", `String k); ("value", `String v) ]
    when String.uppercase_ascii k = "FLABEL" ->
      FLABEL v
  | `Assoc [ ("kind", `String k); ("value", `String v) ]
    when String.uppercase_ascii k = "SLABEL" ->
      SLABEL v
  | `Assoc [ ("kind", `String k); ("value", `String v) ]
    when String.uppercase_ascii k = "JMP" ->
      JMP v
  (* **************** **************** ***************** ******************** *)
  | `Assoc [ ("kind", `String k); ("name", `String name); ("value", `String v) ]
    when String.uppercase_ascii k = "CJMP" ->
      CJMP (name, v)
  | `Assoc
      [
        ("kind", `String k);
        ("s", `String fname);
        ("f1", `Int f1);
        ("f2", `Int f2);
        ("ds", `List ds);
        ("ss", `List ss);
        ("scopes", `List scopes);
      ]
    when String.uppercase_ascii k = "BEGIN" ->
      BEGIN
        ( fname,
          f1,
          f2,
          List.map dsgn_of_json ds,
          List.map (function `String s -> s | _ -> failwith "bad argument") ss,
          List.map scope_of_json scopes )
  | `Assoc [ ("kind", `String k) ] when String.uppercase_ascii k = "END" -> END
  | `Assoc js
    when List.assoc_opt "kind" js = Some (`String "CLOSURE")
         && List.assoc_opt "name" js <> None
         && List.assoc_opt "ds" js <> None ->
      let name =
        match List.assoc "name" js with
        | `String s -> s
        | _ -> failwiths "Bad json. %s %d" __FILE__ __LINE__
      in
      let dsgns =
        match List.assoc "ds" js with
        | `List s -> List.map dsgn_of_json s
        | _ -> failwiths "Bad json. %s %d" __FILE__ __LINE__
      in
      CLOSURE (name, dsgns)
  | `Assoc [ ("kind", `String k); ("f1", `String f1); ("f2", `String f2) ]
    when String.uppercase_ascii k = "PROTO" ->
      PROTO (f1, f2)
  | `Assoc [ ("kind", `String k); ("f1", `String f1); ("f2", `String f2) ]
    when String.uppercase_ascii k = "PPROTO" ->
      PPROTO (f1, f2)
  | `Assoc [ ("kind", `String k); ("f1", `Int f1); ("f2", `Bool f2) ]
    when String.uppercase_ascii k = "PCALLC" ->
      PCALLC (f1, f2)
  | `Assoc [ ("kind", `String k); ("f1", `Int f1); ("f2", `Bool f2) ]
    when String.uppercase_ascii k = "CALLC" ->
      CALLC (f1, f2)
  | `Assoc
      [
        ("kind", `String k);
        ("fname", `String fname);
        ("argc", `Int argc);
        ("flg", `Bool b);
      ]
    when String.uppercase_ascii k = "CALL" ->
      CALL (fname, argc, b)
  | `Assoc [ ("kind", `String k) ] when String.uppercase_ascii k = "RET" -> RET
  | `Assoc [ ("kind", `String k) ] when String.uppercase_ascii k = "DROP" ->
      DROP
  | `Assoc [ ("kind", `String k) ] when String.uppercase_ascii k = "DUP" -> DUP
  | `Assoc [ ("kind", `String k) ] when String.uppercase_ascii k = "SWAP" ->
      SWAP
  | `Assoc [ ("kind", `String k); ("f1", `String f1); ("f2", `Int f2) ]
    when String.uppercase_ascii k = "TAG" ->
      TAG (f1, f2)
  | `Assoc [ ("kind", `String k); ("value", `Int n) ]
    when String.uppercase_ascii k = "ARRAY" ->
      ARRAY n
  | `Assoc [ ("kind", `String k); ("value", n) ]
    when String.uppercase_ascii k = "PATT" ->
      PATT (json_to_patt n)
  | `Assoc [ ("kind", `String k); ("loc", l); ("leave", `Bool leave) ]
    when String.uppercase_ascii k = "FAIL" ->
      FAIL (json_to_loc l, leave)
  | `Assoc [ ("kind", `String k); ("value", `String fname) ]
    when String.uppercase_ascii k = "EXTERN" ->
      EXTERN fname
  | `Assoc [ ("kind", `String k); ("value", `String fname) ]
    when String.uppercase_ascii k = "PUBLIC" ->
      PUBLIC fname
  | `Assoc [ ("kind", `String k); ("value", `String fname) ]
    when String.uppercase_ascii k = "IMPORT" ->
      IMPORT fname
  | `Assoc [ ("kind", `String k); ("value", `Int n) ]
    when String.uppercase_ascii k = "LINE" ->
      LINE n
  | other ->
      Format.eprintf "%a\n%!" Yojson.Safe.pp other;
      failwiths "Not implemented: %s %d" __FILE__ __LINE__

let json_to_bytecode : Yojson.Safe.t -> SM.prg = function
  | `List items -> List.map to_insn items
  | _ -> failwiths "Bad json. %s %d" __FILE__ __LINE__
(* let json_to_bytecode : Yojson.Safe.t -> SM.insn = to_insn *)
(*  *)

let insn_to_json = function
  | SM.BINOP v -> make_kv "BINOP" (`String v)
  | CONST v -> make_kv "CONST" (`Int v)
  | STRING v -> make_kv "STRING" (`String v)
  | SEXP (s, n) ->
      `Assoc
        [ ("kind", `String "SEXP"); ("name", `String s); ("value", `Int n) ]
  | LD d -> make_kv "LD" (dsgn_to_json d)
  | LDA d -> make_kv "LDA" (dsgn_to_json d)
  | ST d -> make_kv "ST" (dsgn_to_json d)
  | STI -> `Assoc [ ("kind", `String "STI") ]
  | STA -> `Assoc [ ("kind", `String "STA") ]
  | ELEM -> `Assoc [ ("kind", `String "ELEM") ]
  | LABEL v -> make_kv "LABEL" (`String v)
  | FLABEL v -> make_kv "FLABEL" (`String v)
  | SLABEL v -> make_kv "SLABEL" (`String v)
  | JMP v -> make_kv "JMP" (`String v)
  | CJMP (a, b) ->
      `Assoc
        [ ("kind", `String "CJMP"); ("name", `String a); ("value", `String b) ]
  | BEGIN (s, a, b, ds, ss, scopes) ->
      `Assoc
        [
          ("kind", `String "BEGIN");
          ("s", `String s);
          ("f1", `Int a);
          ("f2", `Int b);
          ("ds", `List (List.map dsgn_to_json ds));
          ("ss", `List (List.map (fun s -> `String s) ss));
          ("scopes", `List (List.map scope_to_json scopes));
        ]
  | END -> `Assoc [ ("kind", `String "END") ]
  | CLOSURE (name, ds) ->
      `Assoc
        [
          ("kind", `String "CLOSURE");
          ("name", `String name);
          ("ds", `List (List.map dsgn_to_json ds));
        ]
  | PROTO (a, b) ->
      `Assoc [ ("kind", `String "PROTO"); ("f1", `String a); ("f2", `String b) ]
  | PPROTO (a, b) ->
      `Assoc
        [ ("kind", `String "PPROTO"); ("f1", `String a); ("f2", `String b) ]
  | PCALLC (n, b) ->
      `Assoc [ ("kind", `String "PCALLC"); ("f1", `Int n); ("f2", `Bool b) ]
  | CALLC (n, b) ->
      `Assoc [ ("kind", `String "CALLC"); ("f1", `Int n); ("f2", `Bool b) ]
  | CALL (s, n, b) ->
      `Assoc
        [
          ("kind", `String "CALL");
          ("fname", `String s);
          ("argc", `Int n);
          ("flg", `Bool b);
        ]
  | RET -> `Assoc [ ("kind", `String "RET") ]
  | DROP -> `Assoc [ ("kind", `String "DROP") ]
  | DUP -> `Assoc [ ("kind", `String "DUP") ]
  | SWAP -> `Assoc [ ("kind", `String "SWAP") ]
  | TAG (s, n) ->
      `Assoc [ ("kind", `String "TAG"); ("f1", `String s); ("f2", `Int n) ]
  | ARRAY n -> make_kv "ARRAY" (`Int n)
  | PATT n -> make_kv "PATT" (patt_to_json n)
  | FAIL (l, b) ->
      `Assoc
        [ ("kind", `String "FAIL"); ("loc", loc_to_json l); ("leave", `Bool b) ]
  | EXTERN v -> make_kv "EXTERN" (`String v)
  | PUBLIC v -> make_kv "PUBLIC" (`String v)
  | IMPORT v -> make_kv "IMPORT" (`String v)
  | LINE v -> make_kv "LINE" (`Int v)

module Expr = struct
  let patt_to_json : Language.Pattern.t -> Yojson.Safe.t = function
    | _ -> assert false

  let atr_to_json : Language.Expr.atr -> Yojson.Safe.t = function
    | x -> `String (GT.show Language.Expr.atr x)

  let qualifier_to_json : Language.Expr.qualifier -> Yojson.Safe.t = function
    | `Local -> `String "Local"
    | `Public -> `String "Public"
    | `Extern -> `String "Extern"
    | `PublicExtern -> `String "PublicExtern"

  let rec decl_to_json : Language.Expr.decl -> _ =
   fun (q, info) ->
    let rhs = function
      | `Fun (names, e) ->
          `Assoc
            [
              ("kind", `String "Fun");
              ("name", `List (List.map (fun s -> `String s) names));
              ("body", expr_to_json e);
            ]
      | `Variable None -> `Assoc [ ("kind", `String "Variable") ]
      | `Variable (Some e) ->
          `Assoc [ ("kind", `String "Variable"); ("payload", expr_to_json e) ]
    in
    `List [ qualifier_to_json q; rhs info ]

  and expr_to_json : Language.Expr.t -> Yojson.Safe.t = function
    | Const n -> make_kv "Const" (`Int n)
    | Array es -> make_kv "Array" (`List (List.map expr_to_json es))
    | String n -> make_kv "String" (`String n)
    | Sexp (name, args) ->
        `Assoc
          [
            ("kind", `String "Sexp");
            ("name", `String name);
            ("args", `List (List.map expr_to_json args));
          ]
    | Var n -> make_kv "Var" (`String n)
    | Ref n -> make_kv "Ref" (`String n)
    | Binop (n, l, r) ->
        `Assoc
          [
            ("kind", `String "Binop");
            ("name", `String n);
            ("l", expr_to_json l);
            ("r", expr_to_json r);
          ]
    | Elem (l, r) ->
        `Assoc
          [
            ("kind", `String "Elem");
            ("l", expr_to_json l);
            ("r", expr_to_json r);
          ]
    | ElemRef (l, r) ->
        `Assoc
          [
            ("kind", `String "ElemRef");
            ("l", expr_to_json l);
            ("r", expr_to_json r);
          ]
    | Call (name, args) ->
        `Assoc
          [
            ("kind", `String "Call");
            ("f", expr_to_json name);
            ("args", `List (List.map expr_to_json args));
          ]
    | Assign (l, r) ->
        `Assoc
          [
            ("kind", `String "Assign");
            ("l", expr_to_json l);
            ("r", expr_to_json r);
          ]
    | Seq (l, r) ->
        `Assoc
          [
            ("kind", `String "Seq"); ("l", expr_to_json l); ("r", expr_to_json r);
          ]
    | Skip -> `Assoc [ ("kind", `String "Skip") ]
    | If (cond, l, r) ->
        `Assoc
          [
            ("kind", `String "If");
            ("cond", expr_to_json cond);
            ("r", expr_to_json l);
            ("r", expr_to_json r);
          ]
    | While (cond, body) ->
        `Assoc
          [
            ("kind", `String "While");
            ("cond", expr_to_json cond);
            ("body", expr_to_json body);
          ]
    | DoWhile (cond, body) ->
        `Assoc
          [
            ("kind", `String "DoWhile");
            ("cond", expr_to_json cond);
            ("body", expr_to_json body);
          ]
    | Case (scru, pats, loc, atr) ->
        `Assoc
          [
            ("kind", `String "Case");
            ("scru", expr_to_json scru);
            ( "pats",
              `List
                (List.map
                   (fun (a, b) -> `List [ patt_to_json a; expr_to_json b ])
                   pats) );
            ("loc", loc_to_json loc);
            ("atr", atr_to_json atr);
          ]
    | Ignore e ->
        `Assoc [ ("kind", `String "Ignore"); ("value", expr_to_json e) ]
    | Unit -> `Assoc [ ("kind", `String "Unit") ]
    | Scope (decls, e) ->
        `Assoc
          [
            ("kind", `String "Scope");
            ( "decls",
              `List
                (List.map
                   (fun (name, d) -> `List [ `String name; decl_to_json d ])
                   decls) );
            ("body", expr_to_json e);
          ]
    | Lambda (args, body) ->
        `Assoc
          [
            ("kind", `String "Lambda");
            ("args", `List (List.map (fun name -> `String name) args));
            ("body", expr_to_json body);
          ]
    | Leave -> `Assoc [ ("kind", `String "Leave") ]
    | Intrinsic _ | Control _ -> failwith "Not implemented"
end
