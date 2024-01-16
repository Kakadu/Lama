open Js_of_ocaml
open Js_of_ocaml.Firebug

(* let () = Firebug.console##log (Js.string "asdf") *)
module Predefined = struct
  module Expr = struct
    let one = [%blob "1write.bc.json"]
  end

  module Funs = struct
    let fac = [%blob "1append.bc.json"]
  end

  module Other = struct
    let append = [%blob "1append.bc.json"]
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
    [ ("append", Predefined.Other.append); ("1", Predefined.Expr.one) ]
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
(*
   let () =
     let area = get_stru_text_exn () in
     area##.onchange :=
       Dom_html.handler (fun _ ->
           console##log (Js.string "json changed");
           Js._true) *)
