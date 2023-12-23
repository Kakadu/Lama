let failwiths ppf = Format.kasprintf failwith ppf

let bytecode_of_json : Yojson.Safe.t -> SM.prg =
  let of_dsgntn : Yojson.Safe.t -> Language.Value.designation = function
    | `Assoc [ ("kind", `String k); ("value", `String s) ]
      when String.lowercase_ascii k = "global" ->
        Language.Value.Global s
    | `Assoc [ ("kind", `String k); ("value", `Int n) ]
      when String.lowercase_ascii k = "local" ->
        Language.Value.Local n
    | _ -> failwiths "Not implemented: %s %d" __FILE__ __LINE__
  in
  let to_insn : Yojson.Safe.t -> SM.insn = function
    | `Assoc [ ("kind", `String k); ("value", `String v) ]
      when String.uppercase_ascii k = "BINOP" ->
        BINOP v
    | `Assoc [ ("kind", `String k); ("value", `Int v) ]
      when String.uppercase_ascii k = "CONST" ->
        CONST v
    | `Assoc [ ("kind", `String k); ("value", v) ]
      when String.uppercase_ascii k = "LD" ->
        LD (of_dsgntn v)
    | _ -> failwiths "Not implemented: %s %d" __FILE__ __LINE__
  in
  function `List items -> List.map to_insn items | _ -> failwith "Bad json"

let () =
  let on_file name =
    let contents = In_channel.with_open_text name In_channel.input_all in
    let j = Yojson.Safe.from_string contents in
    let bc = bytecode_of_json j in
    let _rez : int list = SM.run bc [] in
    Format.printf "Result: @[%a@]" Format.(pp_print_list pp_print_int) _rez;
    ()
  in
  Arg.parse [] on_file "help"
