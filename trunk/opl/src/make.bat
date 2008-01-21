@echo off

rem Simple compilation script.

echo Compiling to destination ./opl.exe
echo.
ocamlc -c types.ml
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex lexer.mll
ocamlc -c lexer.ml
ocamlc -o ./opl.exe lexer.cmo parser.cmo opl.ml
del *cmi 
del *cmo
del lexer.ml
del parser.mli
del parser.ml
echo Finished.
