@echo off
echo Compiling to destination ../bin/opl.exe
echo.
ocamlc -c types.ml
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex lexer.mll
ocamlc -c lexer.ml
ocamlc -o ../bin/opl.exe lexer.cmo parser.cmo opl.ml
del *cmi 
del *cmo
del lexer.ml
del parser.mli
rem del parser.ml
echo Finished.