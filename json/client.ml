open Js_of_ocaml
open Js_of_ocaml.Firebug

module Predefined = struct
  module Expr = struct
    let write = [%blob "1write.lama"]
    let write_bc = [%blob "1write.bc.json"]
  end

  module Funs = struct
    let fac = [%blob "1append.lama"]
    let fac_bc = [%blob "1append.bc.json"]
  end

  module Other = struct
    let append = [%blob "1append.lama"]
    let append_bc = [%blob "1append.bc.json"]
    let test50arrays = [%blob "test050.lama"]
    let test50arrays_bc = [%blob "test050.bc.json"]
    let test081zip = [%blob "081zip.lama"]
    let test081zip_bc = [%blob "081zip.bc.json"]
  end
end

let eval_bc_string contents =
  print_endline contents;
  let j = Yojson.Safe.from_string contents in
  let bc = LibSerialize.json_to_bytecode j in
  let rez : int list = SM.run bc [] in
  Format.printf "Result: @[%a@]\n%!" Format.(pp_print_list pp_print_int) rez;
  ()

let __ () = eval_bc_string Predefined.Other.append

let clear_output_area () =
  (Dom_html.getElementById_exn "output_area")##.textContent := Js.null

let get_stru_text_exn () =
  Dom_html.getElementById_coerce "input_area" Dom_html.CoerceTo.textarea
  |> Option.get

let run_clicked () =
  clear_output_area ();
  let textarea = get_stru_text_exn () in
  (* (Dom_html.getElementById_exn "output_area")##.textContent
     := Js.some textarea##.value *)
  let j = Yojson.Safe.from_string (Js.to_string textarea##.value) in
  let bc = LibSerialize.json_to_bytecode j in
  let rez : int list = SM.run bc [] in
  (Dom_html.getElementById_exn "output_area")##.textContent
  := Js.some
     @@ Js.string
          (Format.asprintf "Result: @[%a@]\n%!"
             Format.(pp_print_list pp_print_int)
             rez)

let () =
  (Dom_html.getElementById_exn "send1")##.onclick
  := Dom.handler (fun _ ->
         run_clicked ();
         Js._true)

let () =
  let input_area = Dom_html.createTextarea Dom_html.document in
  input_area##setAttribute (Js.string "id") (Js.string "input_area");
  Dom.appendChild (Dom_html.getElementById_exn "left") input_area

let () =
  let examples =
    [
      ("zip", Predefined.Other.test081zip_bc);
      ("append", Predefined.Other.append_bc);
      ("test50array", Predefined.Other.test50arrays_bc);
      ("write", Predefined.Expr.write_bc);
    ]
  in
  let _combo =
    Dom_html.getElementById_coerce "demos" Dom_html.CoerceTo.select
    |> Option.get
  in
  List.iter
    (fun (name, _) ->
      _combo##add
        (let g = Dom_html.createOption Dom_html.document in
         g##.label := Js.string name;
         g)
        Js.null)
    examples;
  let on_change () =
    console##log _combo##.selectedIndex;
    let textarea = get_stru_text_exn () in

    textarea##.value :=
      Js.string (snd (List.nth examples _combo##.selectedIndex))
  in
  _combo##.onchange :=
    Dom_html.handler (fun _ ->
        on_change ();
        Js._true);
  _combo##.selectedIndex := 0;
  on_change ()

module Lama2JSON = struct
  let () =
    let input_area = Dom_html.createTextarea Dom_html.document in
    input_area##setAttribute (Js.string "id") (Js.string "input_area_lama2json");
    Dom.appendChild (Dom_html.getElementById_exn "left_lama2json") input_area

  let () =
    (Dom_html.getElementById_exn "left_lama2json")##.onclick
    := Dom.handler (fun _ ->
           console##log (Js.string "parse lama here");
           Js._true)

  let () =
    let examples =
      [
        ("zip", Predefined.Other.test081zip);
        ("append", Predefined.Other.append);
        ("test50array", Predefined.Other.test50arrays);
        ("write", Predefined.Expr.write);
      ]
    in
    let _combo =
      Dom_html.getElementById_coerce "lamaDemos" Dom_html.CoerceTo.select
      |> Option.get
    in
    List.iter
      (fun (name, _) ->
        _combo##add
          (let g = Dom_html.createOption Dom_html.document in
           g##.label := Js.string name;
           g)
          Js.null)
      examples;
    let on_change () =
      console##log _combo##.selectedIndex;
      let textarea =
        Dom_html.getElementById_coerce "input_area_lama2json"
          Dom_html.CoerceTo.textarea
        |> Option.get
      in

      textarea##.value :=
        Js.string (snd (List.nth examples _combo##.selectedIndex))
    in

    _combo##.onchange :=
      Dom_html.handler (fun _ ->
          on_change ();
          Js._true);
    _combo##.selectedIndex := 0;
    (* on_change (); *)
    ()

  let () =
    Language.interface_lookup.Language.ilookup <- `Hardcoded [%blob "Std.i"]

  let () =
    console##log (Js.string "HERE");
    console##log (Js.string Sys.os_type);
    (Dom_html.getElementById_exn "lamaToJsonBtn")##.onclick
    := Dom.handler (fun _ ->
           console##log (Js.string "parsing...");
           let contents =
             let textarea =
               Dom_html.getElementById_coerce "input_area_lama2json"
                 Dom_html.CoerceTo.textarea
               |> Option.get
             in
             Js.to_string textarea##.value
           in

           (* let contents = In_channel.with_open_text name In_channel.input_all in *)
           let ast =
             let name = "asdf.lama" in
             let cmd =
               object
                 method get_infile = name
                 method basename = name
                 method topname = name
                 method dump_SM _ = ()

                 method is_workaround =
                   false (* True gives parsing error about expected import *)

                 method get_include_paths = [ (* cfg.include_path *) ]
               end
             in
             let rez = Language.run_parser_string cmd contents in
             match rez with
             | `Fail s ->
                 Printf.eprintf "Lama Parsing error:\n%s\n%!" s;
                 exit 1
             | `Ok ((_, expr_ast) as prog) -> expr_ast
           in
           Js._true)
end
