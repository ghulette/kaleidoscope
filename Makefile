OCAMLMAKEFILE = ./OCamlMakefile

RESULT = kalc
SOURCES = util.ml \
		  ast.ml \
		  parser.mly \
		  lexer.mll \
		  kalc.ml

include $(OCAMLMAKEFILE)
