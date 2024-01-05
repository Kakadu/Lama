  $ echo '[' >> a.json
  $ echo '{ kind: "IMPORT", value: "Std" },' >> a.json
  $ echo '{ kind: "EXTERN", value: "Lwrite" },' >> a.json
  $ echo '{ kind: "CONST", value: 1 },' >> a.json
  $ echo '{ kind: "CONST", value: 5 },' >> a.json
  $ echo '{ kind: "binop", value: "+" },' >> a.json
  $ echo '{ kind: "CALL", fname: "Lwrite", argc: 1, "flg": false }' >> a.json
  $ echo ']' >> a.json
  $ cat a.json
  [
  { kind: "IMPORT", value: "Std" },
  { kind: "EXTERN", value: "Lwrite" },
  { kind: "CONST", value: 1 },
  { kind: "CONST", value: 5 },
  { kind: "binop", value: "+" },
  { kind: "CALL", fname: "Lwrite", argc: 1, "flg": false }
  ]
  $ ./json_bytecode.exe a.json
  Result: 6
