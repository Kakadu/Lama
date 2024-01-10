open Lwt.Syntax

module SS = struct
  include Set.Make (String)

  let to_string m = m |> to_seq |> List.of_seq |> String.concat " "
  let to_list f set = set |> to_seq |> Seq.map f |> List.of_seq
end

module String_map = Map.Make (String)

type tasks = SS.t String_map.t

type cfg = {
  mutable http_port : int;
  mutable out_file : string;
  mutable include_path : string;
  mutable tasks_dir : string;
  mutable tasks : tasks;
}

let cfg =
  {
    http_port = 8040;
    out_file = "";
    include_path = "";
    tasks_dir = "";
    tasks = String_map.empty;
  }

open Printf

let describe_lang lang _ =
  let open Opium in
  let info, tasks =
    match lang with
    | `Lang1 ->
        Printf.printf "%s %d\n%!" __FILE__ __LINE__;
        ("Описание языка 1: арифметика", String_map.find "1expr" cfg.tasks)
    | `Lang2 ->
        ("Описание языка 1: арифметика", String_map.find "loops" cfg.tasks)
    | `Lang3 ->
        ("Описание языка 1: арифметика", String_map.find "funs" cfg.tasks)
  in
  let body_ =
    let open Tyxml.Html in
    [
      txt info;
      br ();
      ul
        (SS.to_list
           (fun task ->
             let url = task ^ "/text" in
             li [ a ~a:[ a_href url ] [ txt task ] ])
           tasks);
    ]
  in
  let html =
    let open Tyxml.Html in
    html
      ~a:[ a_lang "en" ]
      (head
         (title (txt "title"))
         [
           meta ~a:[ a_charset "utf-8" ] ();
           meta
             ~a:
               [
                 a_name "viewport";
                 a_content "width=device-width, initial-scale=1";
               ]
             ();
           meta ~a:[ a_name "theme-color"; a_content "#ffffff" ] ();
           link ~rel:[ `Stylesheet ]
             ~href:"https://unpkg.com/tailwindcss@^1.8/dist/tailwind.min.css" ();
         ])
      (body body_)
  in
  Lwt.return (Response.of_html html)

let start_server () =
  let open Opium in
  if not (Sys.file_exists cfg.tasks_dir) then (
    Printf.eprintf "Tasks dir %S doesn't exist" cfg.tasks_dir;
    exit 1)
  else
    let find_tasks dir =
      let tasks_in_dir dir =
        let rec loop acc h =
          match Unix.readdir h with
          | "." | ".." -> loop acc h
          | s -> loop (s :: acc) h
          | exception End_of_file -> List.rev acc
        in
        let h = Unix.opendir dir in
        let files = loop [] h in
        Unix.closedir h;
        List.fold_left
          (fun acc x ->
            if String.ends_with ~suffix:".lama" x then
              let b = Filename.chop_extension x in
              if Sys.file_exists (dir ^ "/" ^ b ^ ".lama.json") then
                SS.add x acc
              else acc
            else acc)
          SS.empty files
      in

      let expr_tasks = tasks_in_dir (dir ^ "/" ^ "1expr") in
      let loop_tasks = tasks_in_dir (dir ^ "/" ^ "2loops") in
      let fun_tasks = tasks_in_dir (dir ^ "/" ^ "3functions") in
      Printf.printf "Some tasks found:\n%!";
      Printf.printf "expr: %s\n%!" (SS.to_string expr_tasks);
      Printf.printf "loop: %s\n%!" (SS.to_string loop_tasks);
      Printf.printf "funs: %s\n%!" (SS.to_string fun_tasks);
      cfg.tasks <-
        String_map.(
          empty |> add "expr" expr_tasks |> add "loops" loop_tasks
          |> add "funs" fun_tasks)
    in
    let _ = find_tasks cfg.tasks_dir in

    let print_json req =
      Printf.printf "%s %s %d\n%!" __FUNCTION__ __FILE__ __LINE__;
      req |> Request.to_json_exn |> fun _json ->
      Lwt.return (Response.make ~body:(Body.of_string "Received response") ())
    in
    let on_not_found _ =
      Printf.printf "%s %d\n%!" __FILE__ __LINE__;
      Lwt.return (Headers.empty, Body.empty)
    in

    let describe_lang1 = describe_lang `Lang1 in

    let describe_lang2 =
      describe_lang `Lang2
      (* Lwt.return
         (Response.make
            ~body:(Body.of_string "Описание языка 2: ветвления и циклы")
            ()) *)
    in
    let describe_lang3 =
      describe_lang `Lang3
      (* Lwt.return
         (Response.make ~body:(Body.of_string "Описание языка 3: Функции") ()) *)
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
      ( "-tasks-dir",
        Arg.String (fun s -> cfg.tasks_dir <- s),
        " Where load tasks for server" );
      ("-batch", Arg.String on_file, " FILE Run of file");
      ("-src2json", Arg.String src2json, " FILE Convert source to json");
      ("-compile-to-bc-json", Arg.String compile_to_bc_json, " FILE  ");
    ]
    on_file "help"
