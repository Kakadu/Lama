(* https://getmdl.io/components/index.html#lists-section *)

open Lwt.Syntax

module SS = struct
  include Set.Make (String)

  let to_string m = m |> to_seq |> List.of_seq |> String.concat " "
  let to_list f set = set |> to_seq |> Seq.map f |> List.of_seq
end

module String_map = Map.Make (String)

module Task = struct
  type t = {
    name : GT.string;
    lama_file : GT.string;
    lama_json_file : GT.string;
  }
  [@@deriving gt ~plugins:{ show }]

  let make ~name lama_file lama_json_file = { name; lama_file; lama_json_file }
  let compare = compare
end

module Task_set = struct
  include Set.Make (Task)

  let to_string m =
    m |> to_seq |> Seq.map (GT.show Task.t) |> List.of_seq |> String.concat " "

  let to_list f set = set |> to_seq |> Seq.map f |> List.of_seq
end

type cfg = {
  mutable http_port : int;
  mutable out_file : string;
  mutable include_path : string;
  mutable tasks_dir : string;
  mutable tasks : Task_set.t String_map.t;
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

let preapre_html_head body_ =
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
         link ~rel:[ `Stylesheet ]
           ~href:"https://code.getmdl.io/1.3.0/material.indigo-pink.min.css" ();
         link ~rel:[ `Stylesheet ]
           ~href:"https://code.getmdl.io/1.3.0/material.indigo-pink.min.css" ();
         script
           ~a:
             [
               a_defer (); a_src "https://code.getmdl.io/1.3.0/material.min.js";
             ]
           (txt "");
       ])
    (body body_)

let make_get path f app =
  Printf.printf "creating page '%s'\n" path;
  Opium.App.get path f app

let describe_task app prefix { Task.lama_file; name; lama_json_file } =
  let open Opium in
  let prefix = prefix ^ "/" ^ name in
  app
  |> make_get (prefix ^ "/text") (fun _ ->
         (* Printf.printf "%s %d lama_file = %S\n%!" __FILE__ __LINE__ lama_file; *)
         Lwt.catch
           (fun () ->
             Lwt.return
               (Response.of_plain_text
                  (In_channel.with_open_text lama_file In_channel.input_all)))
           (fun exn ->
             Lwt.return (Response.of_plain_text (Printexc.to_string exn))))
  |> make_get (prefix ^ "/json") (fun _ ->
         (* Printf.printf "%s %d\n%!" __FILE__ __LINE__; *)
         Lwt.catch
           (fun () ->
             Lwt.return
               (Response.of_plain_text
                  (In_channel.with_open_text lama_json_file In_channel.input_all)))
           (fun exn ->
             Lwt.return (Response.of_plain_text (Printexc.to_string exn))))

let describe_lang lang app =
  let open Opium in
  let lang_prefix, info, tasks =
    match lang with
    | `Lang1 ->
        Printf.printf "%s %d\n%!" __FILE__ __LINE__;
        ( "expr",
          "Описание языка 1: арифметика",
          String_map.find "expr" cfg.tasks )
    | `Lang2 ->
        ("loops", "Описание языка 2: циклы", String_map.find "loops" cfg.tasks)
    | `Lang3 ->
        ("funs", "Описание языка 3: functions", String_map.find "funs" cfg.tasks)
  in
  let lang_prefix =
    if not (String.starts_with ~prefix:"/" lang_prefix) then "/" ^ lang_prefix
    else lang_prefix
  in
  let body_ =
    let open Tyxml.Html in
    [
      txt info;
      br ();
      ul
        ~a:[ a_class [ "mdl-list" ] ]
        (Task_set.to_list
           (fun { Task.name; _ } ->
             li
               ~a:[ a_class [ "mdl-list__item"; "mdl-list__item--three-line" ] ]
               [
                 (let url = lang_prefix ^ "/" ^ name ^ "/text" in
                  span
                    ~a:[ a_class [ "mdl-list__item-primary-content" ] ]
                    [ a ~a:[ a_href url ] [ txt "source" ] ]);
                 (let url = lang_prefix ^ "/" ^ name ^ "/json" in
                  span
                    ~a:[ a_class [ "mdl-list__item-secondary-content" ] ]
                    [ a ~a:[ a_href url ] [ txt "JSON" ] ]);
               ])
           tasks);
    ]
  in

  Printf.printf "Creating page for lang '%s'\n" lang_prefix;
  app
  |> make_get lang_prefix (fun _ ->
         Lwt.return (Response.of_html (preapre_html_head body_)))
  |> Task_set.fold (fun task app -> describe_task app lang_prefix task) tasks

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
              let task_name = Filename.chop_extension x in
              let lama_json_file = dir ^ "/" ^ task_name ^ ".lama.json" in
              let lama_file = dir ^ "/" ^ x in
              if Sys.file_exists lama_json_file then
                Task_set.add
                  (Task.make ~name:task_name lama_file lama_json_file)
                  acc
              else acc
            else acc)
          Task_set.empty files
      in

      let expr_tasks = tasks_in_dir (dir ^ "/" ^ "1expr") in
      let loop_tasks = tasks_in_dir (dir ^ "/" ^ "2loops") in
      let fun_tasks = tasks_in_dir (dir ^ "/" ^ "3functions") in
      Printf.printf "Some tasks found:\n%!";
      Printf.printf "expr: %s\n%!" (Task_set.to_string expr_tasks);
      Printf.printf "loop: %s\n%!" (Task_set.to_string loop_tasks);
      Printf.printf "funs: %s\n%!" (Task_set.to_string fun_tasks);
      cfg.tasks <-
        String_map.(
          empty |> add "expr" expr_tasks |> add "loops" loop_tasks
          |> add "funs" fun_tasks)
    in
    let _ = find_tasks cfg.tasks_dir in

    let eval_json req =
      (* Printf.printf "%s %s %d\n%!" __FUNCTION__ __FILE__ __LINE__; *)
      let* j = req |> Request.to_json_exn in
      Lwt.catch
        (fun () ->
          let bc = LibSerialize.json_to_bytecode j in
          let rez : int list = SM.run bc [] in
          let str_rez =
            Format.asprintf "Success:\n%a\n"
              (Format.pp_print_list
                 ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
                 Format.pp_print_int)
              rez
          in
          Lwt.return (Response.make ~body:(Body.of_string str_rez) ()))
        (fun exn ->
          let str =
            "Error:\n" ^ Printexc.get_backtrace () ^ "\n"
            ^ Printexc.to_string exn
          in
          Lwt.return (Response.make ~body:(Body.of_string str) ()))
    in
    let on_not_found _ =
      Printf.printf "%s %d\n%!" __FILE__ __LINE__;
      Lwt.return (Headers.empty, Body.empty)
    in

    let make_app () : Opium.App.t =
      Opium.App.empty
      |> Opium.App.cmd_name "sirius2024"
      |> Opium.App.port cfg.http_port
      |> describe_lang `Lang1 |> App.post "/eval" eval_json
      |> describe_lang `Lang2 |> describe_lang `Lang3
      |> App.not_found on_not_found
    in

    let run_opium_server app =
      let open Lwt.Syntax in
      Lwt.async (fun () ->
          printf "Starting server: https://localhost:%d\n%!" cfg.http_port;

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
