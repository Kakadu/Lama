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
    | `Assoc
        [ ("kind", `String k); ("fname", `String fname); ("argc", `Int argc) ]
      when String.uppercase_ascii k = "CALL" ->
        CALL (fname, argc, false)
    | `Assoc [ ("kind", `String k); ("value", `String fname) ]
    | `Assoc [ ("kind", `String k); ("fname", `String fname) ]
      when String.uppercase_ascii k = "EXTERN" ->
        EXTERN fname
    | `Assoc [ ("kind", `String k); ("value", `String fname) ]
    | `Assoc [ ("kind", `String k); ("fname", `String fname) ]
      when String.uppercase_ascii k = "IMPORT" ->
        IMPORT fname
    | other ->
        Format.eprintf "%a\n%!" Yojson.Safe.pp other;
        failwiths "Not implemented: %s %d" __FILE__ __LINE__
  in
  function `List items -> List.map to_insn items | _ -> failwith "Bad json"

open Lwt.Syntax

type cfg = { mutable http_port : int }

let cfg = { http_port = 8040 }

open Printf

let start_server () =
  let open Opium in
  let print_json req =
    Printf.printf "%s %s %d\n%!" __FUNCTION__ __FILE__ __LINE__;
    req |> Request.to_json_exn |> fun _json ->
    Lwt.return (Response.make ~body:(Body.of_string "Received response") ())
  in
  let on_not_found _ =
    Printf.printf "%s %d\n%!" __FILE__ __LINE__;
    Lwt.return (Headers.empty, Body.empty)
  in

  let describe_lang1 _ =
    Lwt.return
      (Response.make ~body:(Body.of_string "Описание языка 1: арифметика") ())
  in
  let describe_lang2 _ =
    Lwt.return
      (Response.make
         ~body:(Body.of_string "Описание языка 2: ветвления и циклы")
         ())
  in

  let describe lang idx _ _req =
    let body =
      match (lang, idx) with
      | `Arith, 1 -> Body.of_string "1+2"
      | _ -> assert false
    in
    Lwt.return (Response.make ~body ())
  in

  let make_app () : Opium.App.t =
    Opium.App.empty
    |> Opium.App.cmd_name "sirius2024"
    |> Opium.App.port cfg.http_port
    |> App.get "/lang1" describe_lang1
    |> App.get "/lang1/1/text" (describe `Arith 1 `Text)
    |> App.get "/lang2" describe_lang2
    |> App.not_found on_not_found
  in

  let run_opium_server app =
    let open Lwt.Syntax in
    Lwt.async (fun () ->
        let* _server = Opium.App.start app in
        Lwt.return_unit);
    let forever, _ = Lwt.wait () in
    forever
  in
  Lwt_main.run @@ run_opium_server (make_app ())

let () =
  let on_file name =
    let contents = In_channel.with_open_text name In_channel.input_all in
    let j = Yojson.Safe.from_string contents in
    let bc = bytecode_of_json j in
    let rez : int list = SM.run bc [] in
    Format.printf "Result: @[%a@]\n%!" Format.(pp_print_list pp_print_int) rez;
    ()
  in
  Arg.parse
    [
      ("-server", Arg.Unit start_server, " Run a server");
      ("-batch", Arg.String on_file, " FILE Run of file");
    ]
    on_file "help"
