type cfg = {
  mutable http_port : int;
  mutable out_file : string;
  mutable include_path : string;
}

let cfg = { http_port = 8040; out_file = ""; include_path = "" }

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
      ( "-path",
        Arg.String (fun s -> cfg.include_path <- s),
        " Set include path for lama" );
      ("-o", Arg.String (fun s -> cfg.out_file <- s), " Set out file");
      (* ("-batch", Arg.String on_file, " FILE Run of file"); *)
      (* ("-src2json", Arg.String src2json, " FILE Convert source to json"); *)
    ]
    on_file "help"
