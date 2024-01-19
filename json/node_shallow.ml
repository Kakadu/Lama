open Ostap
  module Types_ = Types

(* And now some "real-world" example *)
module ShallowLanguageImplemenation =
  struct

    let empty  x       = failwith @@ "Undefined variable " ^ x
    let update x v s y = if x = y then v else s y

    let runParser p s =
      match Util.parse
          (object
            inherit Matcher.t s
            inherit Util.Lexers.decimal s
            inherit Util.Lexers.ident ["if"; "then"; "else"; "fi"; "while"; "do"; "done"] s
            inherit! Util.Lexers.skip [
              Matcher.Skip.whitespaces " \t\n";
              Matcher.Skip.lineComment "--";
              Matcher.Skip.nestedComment "(*" "*)"
            ] s
           end)
          (ostap (p -EOF))
      with
      | `Ok   p  -> p
      | `Fail er -> failwith @@ Printf.sprintf "Syntax error: %s\n" er

    [@@@ocaml.warning "-8"]
    ostap (
      expr:
        !(Util.expr
           (fun x -> x)
           [|
             `Nona  , [ostap ("=="), (fun x y s -> if x s = y s then 1 else 0)];
             `Lefta , [ostap ("+" ), (fun x y s -> x s + y s); ostap ("-"), (fun x y s -> x s - y s)];
             `Lefta , [ostap ("*" ), (fun x y s -> x s * y s); ostap ("/"), (fun x y s -> x s / y s)]
           |]
           primary
         );

      primary: x:IDENT {fun s -> s x} | n:DECIMAL {fun s -> n} | -"(" expr -")";

      simpleStmt:
        x:IDENT ":=" e:expr                            {fun s -> update x (e s) s}
      | "if" c:expr "then" s1:stmt "else" s2:stmt "fi" {fun s -> (if c s = 0 then s2 else s1) s}
      | "while" c:expr "do" s1:stmt "done"             {fun s -> let rec w s = if c s = 0 then s else w (s1 s) in w s};

      stmt: <s::ss> : !(Util.listBy)[ostap (";")][simpleStmt] {List.fold_left (fun s ss d -> ss @@ s d) s ss}
    )

    let fact =
      let f = runParser stmt 
      {|result:=1; while 1 - (n == 0) do result := result * n; n := n - 1 done
      |} in
      fun n -> (f @@ update "n" n empty) "result"

    let _ = List.iter (fun n -> Printf.printf "fact %d = %d\n" n (fact n)) [1; 2; 3; 4; 5; 6; 7]

  end
