all: main

graphes.o: graphes.c
	gcc -Wall -c graphes.c

graphes: graphes.c
	gcc -Wall -g -o graphes graphes.c

main: graphes

run: main
	./graphes 6 grafield.dot


clean:
	rm -f graphes
	rm -f graphes.o