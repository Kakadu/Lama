open Js_of_ocaml
open Js_of_ocaml.Firebug

(* let () = Firebug.console##log (Js.string "asdf") *)
module Predefined = struct
  module Expr = struct
    (* let _ = [%blob "lama_on_node.ml"] *)
    let one = [%blob "1.lama.json"]
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

let get_stru_text_exn () =
  Dom_html.getElementById_coerce "my-text-area" Dom_html.CoerceTo.textarea
  |> Option.get

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
