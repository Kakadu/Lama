type patt = StrCmp | String | Array | Sexp | Boxed | UnBoxed | Closure
[@@deriving gt ~options:{ show; enum }]

type scope = {
  blab : string;
  elab : string;
  names : (string * int) list;
  subs : scope list;
}

val show_scope : scope -> string

open Language

type insn =
   (** binary operator                           *)
   | BINOP of string
   (** put a constant on the stack               *)
   | CONST of int
   (** put a string on the stack                 *)
   | STRING of string
   (** create an S-expression                    *)
   | SEXP of string * int
   (** load a variable to the stack              *)
   | LD of Value.designation
   (** load a variable address to the stack      *)
   | LDA of Value.designation
   (** store a value into a variable             *)
   | ST of Value.designation
   (** store a value into a reference            *)
   | STI
   (** store a value into array/sexp/string      *)
   | STA
   (** takes an element of array/string/sexp     *)
   | ELEM
   (** a label                                   *)
   | LABEL of string
   (** a forwarded label                         *)
   | FLABEL of string
   (** a scope label                             *)
   | SLABEL of string
   (** unconditional jump                        *)
   | JMP of string
   (*8 conditional jump                          *)
   | CJMP of string * string
   (** begins procedure definition               *)
   | BEGIN of
       string * int * int * Value.designation list * string list * scope list
   (** end procedure definition                  *)
   | END
   (** create a closure                          *)
   | CLOSURE of string * Value.designation list
   (** proto closure                             *)
   | PROTO of string * string
   (** proto closure to a possible constant      *)
   | PPROTO of string * string
   (** proto call                                *)
   | PCALLC of int * bool
   (** calls a closure                           *)
   | CALLC of int * bool
   (** calls a function/procedure                *)
   | CALL of string * int * bool
   (** returns from a function                   *)
   | RET
   (** drops the top element off                 *)
   | DROP
   (*8 duplicates the top element                *)
   | DUP
   (** swaps two top elements                    *)
   | SWAP
   (** checks the tag and arity of S-expression  *)
   | TAG of string * int
   (** checks the tag and size of array          *)
   | ARRAY of int
   (** checks various patterns                   *)
   | PATT of patt
   (** match failure (location, leave a value    *)
   | FAIL of Loc.t * bool
   (** external definition                       *)
   | EXTERN of string
   (** public   definition                       *)
   | PUBLIC of string
   (** import clause                             *)
   | IMPORT of string
   (** line info                                 *)
   | LINE of int
[@@deriving gt ~options:{ show }]

type prg = insn list

val show_prg : insn list -> string
 
class indexer :
  insn list
  -> object
       method is_label : string -> bool
       method labeled : string -> insn list
     end

val run : prg -> int list -> int list

val compile :
  < dump_SM : insn list -> unit
  ; get_include_paths : string list
  ; topname : string
  ; .. > ->
  (string list * 'a) * Language.Expr.t ->
  insn list

module ByteCode : sig
  val compile : < basename : string ; .. > -> insn list -> unit
end
