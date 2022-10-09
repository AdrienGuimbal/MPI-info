all: main

#unionfind.o : unionfind.c unionfind.h
#	gcc -c unionfind.c

automates.cmo: automates.ml
	ocamlc -c automates.ml

main: main.ml automates.cmo
	ocamlc -o $@ automates.cmo $@.ml

run: main automates.cmo
	./main


clean:
	rm -f unionfind_maze
	rm -f *.cmi *.cmo