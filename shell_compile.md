```shell
ocamlc -c -g ast.ml
ocamlyacc parser.mly
ocamlc -c -g parser.mli
ocamlc -c -g parser.ml
ocamllex scanner.mll
51 states, 1995 transitions, table size 8286 bytes
ocamlc -c -g scanner.ml
ocamlc -c -g interpret.ml
ocamlc -c -g bytecode.ml
ocamlc -c -g compile.ml
ocamlc -c -g execute.ml
ocamlc -c -g microc.ml
ocamlc -o microc -g ast.cmo parser.cmo scanner.cmo interpret.cmo bytecode.cmo compile.cmo execute.cmo microc.cmo

```

