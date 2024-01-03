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
    let bc = LibSerialize.json_to_bytecode j in
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
