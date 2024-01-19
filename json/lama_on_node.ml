open Js_of_ocaml
open Js_of_ocaml.Firebug

module Predefined = struct
  module Expr = struct
    let one = [%blob "1write.bc.json"]
  end

  module Funs = struct
    let fac = [%blob "1append.bc.json"]
  end

  module Other = struct
    let append = [%blob "1append.bc.json"]
    let test50arrays = [%blob "test050.bc.json"]
  end
end

let eval_bc_string contents =
  print_endline contents;
  let j = Yojson.Safe.from_string contents in
  let bc = LibSerialize.json_to_bytecode j in
  let rez : int list = SM.run bc [] in
  Format.printf "Result: @[%a@]\n%!" Format.(pp_print_list pp_print_int) rez;
  ()

let __ () = eval_bc_string Predefined.Other.test50arrays

let () =
  Language.interface_lookup.Language.ilookup <- `Hardcoded [%blob "Std.i"];
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
  let contents = {| var x |} in
  let rez =
    Printexc.record_backtrace true;
    try Language.run_parser_string cmd contents
    with Invalid_argument s ->
      Printf.eprintf "%s\n%!" s;
      Printf.eprintf "%s\n%!" (Printexc.get_backtrace ());
      exit 1
  in

  match rez with
  | `Fail s ->
      Printf.eprintf "Lama Parsing error:\n%s\n%!" s;
      exit 1
  | `Ok ((_, expr_ast) as prog) -> print_endline "parsed"
