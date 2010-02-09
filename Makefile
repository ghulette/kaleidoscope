OCAMLMAKEFILE = ./OCamlMakefile

CC = g++
LIBDIRS = /usr/local/lib/ocaml/llvm
INCDIRS = /usr/local/lib/ocaml/llvm

LIBS = llvm
RESULT = kalc
SOURCES = util.ml \
		  ast.ml \
		  parser.mly \
		  lexer.mll \
		  codegen.ml \
		  kalc.ml

include $(OCAMLMAKEFILE)
