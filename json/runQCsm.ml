(** Copyright 2023, Kakadu *)

open QCheck.Gen

let short_str = string_size ~gen:printable (return 3)
let short_str = return ""

let rec shrink_scope_name (s, n) =
  let open QCheck.Iter in
  QCheck.Shrink.string s >|= fun s -> (s, n)

and shrink_scope =
  let open QCheck.Iter in
  function
  | { SM.blab; elab; names; subs } ->
      QCheck.Iter.append_l
        [
          ( QCheck.Shrink.string blab >|= fun blab ->
            { SM.blab; elab; names; subs } );
          ( QCheck.Shrink.string elab >|= fun elab ->
            { SM.blab; elab; names; subs } );
          ( QCheck.Shrink.list ~shrink:shrink_scope_name names >|= fun names ->
            { SM.blab; elab; names; subs } );
          ( QCheck.Shrink.list ~shrink:shrink_scope subs >|= fun subs ->
            { SM.blab; elab; names; subs } );
        ]

let scope_gen =
  QCheck.Gen.(
    sized
    @@ fix (fun self -> function
         | 0 ->
             map2
               (fun blab elab -> { SM.blab; elab; names = []; subs = [] })
               short_str short_str
         | n ->
             map2
               (fun names sc -> { sc with SM.names })
               (list (tup2 short_str int))
             @@ map3
                  (fun blab elab subs -> { SM.blab; elab; names = []; subs })
                  short_str short_str
                  (list @@ self (n / 2))))

let design_gen =
  let open Language.Value in
  QCheck.Gen.(
    oneof
      [
        map (fun x -> Global x) short_str;
        map (fun x -> Arg x) int;
        map (fun x -> Access x) int;
        map (fun x -> Local x) int;
        map (fun x -> Fun x) short_str;
      ])

let patt_gen =
  let open SM in
  QCheck.Gen.(
    oneof
      [
        return StrCmp;
        return String;
        return Array;
        return Sexp;
        return Boxed;
        return UnBoxed;
        return Closure;
      ])

let loc_gen = QCheck.Gen.(tup2 int int)

let insn_gen =
  let const = map (fun x -> SM.CONST x) int in
  let str = map (fun x -> SM.STRING x) short_str in
  let sexp = map2 (fun x b -> SM.SEXP (x, b)) short_str int in
  let binop =
    map
      (fun x -> SM.BINOP x)
      (oneof [ return "+"; return "*"; return "-"; return "/" ])
  in
  QCheck.Gen.(
    oneof
      [
        const;
        str;
        sexp;
        binop;
        map (fun x -> SM.LD x) design_gen;
        map (fun x -> SM.LDA x) design_gen;
        map (fun x -> SM.ST x) design_gen;
        return SM.STI;
        return SM.STA;
        return SM.ELEM;
        map (fun x -> SM.LABEL x) short_str;
        map (fun x -> SM.FLABEL x) short_str;
        map (fun x -> SM.SLABEL x) short_str;
        map (fun x -> SM.JMP x) short_str;
        map2 (fun x b -> SM.CJMP (x, b)) short_str short_str;
        map (fun (a, b, c, d, e, f) -> SM.BEGIN (a, b, c, d, e, f))
        @@ tup6 short_str int int (list design_gen) (list short_str)
             (list scope_gen);
        return SM.END;
        map2 (fun x b -> SM.CLOSURE (x, b)) short_str (list design_gen);
        map2 (fun x b -> SM.PROTO (x, b)) short_str short_str;
        map2 (fun x b -> SM.PROTO (x, b)) short_str short_str;
        map2 (fun x b -> SM.PCALLC (x, b)) int bool;
        map2 (fun x b -> SM.CALLC (x, b)) int bool;
        map3 (fun x n b -> SM.CALL (x, n, b)) short_str int bool;
        return SM.RET;
        return SM.DROP;
        return SM.DUP;
        return SM.SWAP;
        map2 (fun x b -> SM.TAG (x, b)) short_str int;
        map (fun x -> SM.ARRAY x) int;
        map (fun x -> SM.PATT x) patt_gen;
        map2 (fun x b -> SM.FAIL (x, b)) loc_gen bool;
        map (fun s -> SM.EXTERN s) short_str;
        map (fun s -> SM.PUBLIC s) short_str;
        map (fun s -> SM.IMPORT s) short_str;
        map (fun s -> SM.LINE s) int;
      ])

(* let __ () =
   List.iter
     (fprintf std_formatter "%a\n" Printast.pp_named)
     (QCheck.Gen.generate ~n:20 lam_gen) *)
let arbitrary_scope =
  QCheck.make scope_gen ~print:SM.show_scope ~shrink:shrink_scope

let arbitrary_insn =
  let open QCheck.Iter in
  let shrink_insn : SM.insn -> SM.insn QCheck.Iter.t = function
    | SM.BINOP i -> QCheck.Shrink.string i >|= fun x -> SM.BINOP x
    (* | Abs (c, b) -> of_list [ b ] <+> (shrink_lam b >|= fun b' -> abs c b') *)
    | BEGIN (a, b, c, d, e, f) ->
        of_list []
        <+> (QCheck.Shrink.string a >|= fun a -> SM.BEGIN (a, b, c, d, e, f))
        <+> (QCheck.Shrink.int b >|= fun b -> SM.BEGIN (a, b, c, d, e, f))
        <+> (QCheck.Shrink.int c >|= fun c -> SM.BEGIN (a, b, c, d, e, f))
        (* <+> (shrink_lam b >|= fun b' -> app a b') *)
    | _ -> QCheck.Iter.empty
  in

  QCheck.make insn_gen ~print:(fun x -> GT.show SM.insn x)
(* ~shrink:shrink_insn *)

let test_insn =
  QCheck.(
    Test.make arbitrary_insn (fun l ->
        l = LibSerialize.to_insn @@ LibSerialize.insn_to_json l))

let test_scope =
  QCheck.(
    Test.make arbitrary_scope (fun l ->
        l = LibSerialize.json_to_scope @@ LibSerialize.scope_to_json l))

(* let _ = QCheck_base_runner.set_seed 473538955 *)

let () =
  Arg.parse
    [ ("-seed", Arg.Int QCheck_base_runner.set_seed, " ") ]
    (fun _ -> assert false)
    "help"

let __ () =
  print_endline "Testing scope...";
  Format.printf "Failed tests: %d\n%!"
  @@ QCheck_base_runner.run_tests [ test_scope ]

let () =
  print_endline "Testing instructions...";
  Format.printf "Failed tests: %d\n%!"
  @@ QCheck_base_runner.run_tests [ test_insn ]
