open Lwt.Syntax

type cfg = {
  mutable http_port : int;
  mutable out_file : string;
  mutable include_path : string;
}

let cfg = { http_port = 8040; out_file = ""; include_path = "" }

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
    let bc = LibSerialize.json_to_bytecode j in
    let rez : int list = SM.run bc [] in
    Format.printf "Result: @[%a@]\n%!" Format.(pp_print_list pp_print_int) rez;
    ()
  in
  let src2json name =
    let contents = In_channel.with_open_text name In_channel.input_all in
    let _ =
      let cmd =
        object
          method get_infile = name
          method basename = name
          method topname = name
          method dump_SM _ = ()

          method is_workaround =
            false (* True gives parsing error about expected import *)

          method get_include_paths = [ cfg.include_path ]
        end
      in
      let rez = Language.run_parser cmd in
      match rez with
      | `Fail s ->
          Printf.eprintf "Lama Parsing error:\n%s\n%!" s;
          exit 1
      | `Ok ((_, expr_ast) as prog) ->
          assert (cfg.out_file <> "");
          Out_channel.with_open_text cfg.out_file (fun ch ->
              Yojson.Safe.pretty_to_channel ch
                (LibSerialize.Expr.expr_to_json expr_ast));
          SM.ByteCode.compile cmd (SM.compile cmd prog)
    in
    ()
  in
  let compile_to_bc_json name =
    let contents = In_channel.with_open_text name In_channel.input_all in
    let _ =
      let cmd =
        object
          method get_infile = name
          method basename = name
          method topname = name
          method dump_SM _ = ()

          method is_workaround =
            false (* True gives parsing error about expected import *)

          method get_include_paths = [ cfg.include_path ]
        end
      in
      let rez = Language.run_parser cmd in
      match rez with
      | `Fail s ->
          Printf.eprintf "Lama Parsing error:\n%s\n%!" s;
          exit 1
      | `Ok ((_, expr_ast) as prog) ->
          assert (cfg.out_file <> "");
          let insns = SM.compile cmd prog in

          (* assert (List.length insns = 1); *)
          Out_channel.with_open_text cfg.out_file (fun ch ->
              Yojson.Safe.pretty_to_channel ch
              @@ `List (List.map LibSerialize.insn_to_json insns));
          ()
    in
    ()
  in
  Arg.parse
    [
      ( "-path",
        Arg.String (fun s -> cfg.include_path <- s),
        " Set include path for lama" );
      ("-o", Arg.String (fun s -> cfg.out_file <- s), " Set out file");
      ("-server", Arg.Unit start_server, " Run a server");
      ("-batch", Arg.String on_file, " FILE Run of file");
      ("-src2json", Arg.String src2json, " FILE Convert source to json");
      ("-compile-to-bc-json", Arg.String compile_to_bc_json, " FILE  ");
    ]
    on_file "help"
