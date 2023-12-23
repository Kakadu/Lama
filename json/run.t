  $ echo '[' >> a.json
  $ echo '{ kind: "IMPORT", value: "Std" },' >> a.json
  $ echo '{ kind: "EXTERN", value: "Lwrite" },' >> a.json
  $ echo '{ kind: "CONST", value: 1 },' >> a.json
  $ echo '{ kind: "CONST", value: 5 },' >> a.json
  $ echo '{ kind: "binop", value: "+" },' >> a.json
  $ echo '{ kind: "CALL", fname: "Lwrite", argc: 1 }' >> a.json
  $ echo ']' >> a.json
  $ cat a.json
  [
  { kind: "CONST", value: 1 },
  { kind: "CONST", value: 5 },
  { kind: "binop", value: "+" }
  ]
  $ ./json_bytecode.exe a.json
  Result: 
  $ rm *.json -f

  $ echo '[' >> 02.json
$ echo '{ kind: "CONST", value: 1 },' >> 02.json
$ echo '{ kind: "LD", value: { kind: "Local", value: "s" } },' >> 02.json
  $ echo '{ kind: "CONST", value: 5 },' >> 02.json
  $ echo '{ kind: "binop", value: "+" }' >> 02.json
  $ echo ']' >> 02.json
  $ cat 02.json
  [
  { kind: "LD", value: { kind: "Local", value: "s" } },
  { kind: "CONST", value: 5 },
  { kind: "binop", value: "+" }
  ]
  $ ./json_bytecode.exe 02.json
  Fatal error: exception Failure("Not implemented: json/Json_bytecode.ml 11")
  [2]
