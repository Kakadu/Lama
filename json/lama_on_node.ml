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

let () = eval_bc_string Predefined.Other.test50arrays
