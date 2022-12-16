all: main

graphes.o: graphes.c
	gcc -Wall -c graphes.c

graphes: graphes.c
	gcc -Wall -ggdb -o graphes graphes.c

main: graphes

run: main
	./graphes 10 grafield.dot

grafield.pdf: grafield.dot
	neato -Tpdf -o grafield.pdf grafield.dot

view: grafield.pdf
	evince grafield.pdf

rv : 
	make run; make view

clean:
	rm -f graphes
	rm -f graphes.o
	rm *.dot *.pdf
