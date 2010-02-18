OCAMLMAKEFILE = ./OCamlMakefile

# This is required to link with LLVM
OCAMLLDFLAGS = -cc g++ 

# Suppress a huge number of warnings from g++
LDFLAGS = -w

RESULT = kalc

LIBS = llvm llvm_analysis llvm_executionengine llvm_target

SOURCES = kstdlib.c \
          util.ml \
		      ast.ml \
		      parser.mly \
		      lexer.mll \
		      codegen.ml \
		      kalc.ml

include $(OCAMLMAKEFILE)
