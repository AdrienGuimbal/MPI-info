#include "graphes.h"

int main(const int argc, const char * argv[]) {
	if (argc <= 2) {
		printf("you need to specify [size] and [filename]");
		return 1;
	}
	int size = atoi(argv[1]);
	
	if (argc == 4) 
		srand(atoi(argv[3]));
	else
		srand(time(0));
	
	digraph grafield;
	init_graph(&grafield, size);

	scc(&grafield);

	to_neato(grafield, argv[2]);

	release_graph(&grafield);
	return 0;
}


/*****************\
|* Mise en place *|
\*****************/ 


int idx (uint i, uint j, uint width) { // converti un index bidimentionnnel en un monodimentionnel
	return i*width + j;
}

void set_edge (digraph *graph, uint s, uint t, bool is) {
	graph->edges[idx(s, t, graph->S)] = is; 
}

bool has_edge (const digraph *graph, uint s, uint t) {
	//if (s < 0 || t < 0 || s >= graph->S || t >= graph->S)
	//	return false;
	return graph->edges[idx(s, t, graph->S)];
}

void init_graph (digraph *graph, uint size) {
	uint S = size*size;

	// initialisation des constantes et allocations
	graph->s = size;
	graph->S = S;
	graph->vertices = (node*) malloc(S * sizeof(node));
	graph->edges = (bool*) calloc(S * S, sizeof(bool));

	assert( graph->vertices != NULL && graph->edges != NULL);
	
	// ajout aléatoire des arêtes
	forr ( i, size ) forr ( j, size ) {
		uint curr = idx(  i,   j, size);
		uint left = idx(  i, j+1, size);
		uint down = idx(i+1,   j, size);

		if (j != size-1) {
			bool dir = rand() % 2 ;
			set_edge(graph, curr, left,  dir);
			set_edge(graph, left, curr, !dir);
		}
		if (i != size-1) {
			bool dir = rand() % 2 ;
			set_edge(graph, curr, down,  dir);
			set_edge(graph, down, curr, !dir);
		}
	}

	// initialise les sommets
	forr (i, S) {
		(graph->vertices)[i].debut = 0;
		(graph->vertices)[i].fin = 0;
		(graph->vertices)[i].parent = i;
		(graph->vertices)[i].status = NonVu;
	}
}

void release_graph (digraph* graph) {
	free(graph->vertices);
	free(graph->edges);
}

void to_neato (digraph graph, const char* filename) {
	FILE* file = fopen(filename, "wt");
	assert(file != NULL);
	

	fprintf(file,"digraph G { \n\
	node [colorscheme=paired12\n\
		  shape=point\n\
	      width=0.4]\n\
	edge [colorscheme=paired12 penwidth=4]\n\
	edge [color=0]\n");

	forr (i, graph.S) {
//		fprintf(file, "\tnode[label=\"%i/%i\"]\n", (graph.vertices)[i].debut, (graph.vertices)[i].fin);
		fprintf(file, "\tnode[color=%i]\n", 1 + get_root(&graph, i)%12);
		fprintf(file, "\t%i[pos=\"%i,%i!\"];\n", i, i/graph.s, i%graph.s);
	}

	forr (i, graph.S)
		forr (j, graph.S)
			if (has_edge(&graph, i, j))
				fprintf(file, "\t%i -> %i;\n", i, j);
	
	fprintf(file,"}");
	fclose(file);
}


/************************\
|* Focntions Auxilières *|
\************************/

uint* range (uint a, uint b) { // renvoie liste d'entiers de [a;b[ dans l'ordre croissant
	assert(a < b);
	uint* t = malloc((b-a) * sizeof(uint));
	forr (i, b-a) {
		t[i] = a + i;
	}
	return t;
}

void visit (digraph* graph, uint s, uint* time) {

	(graph->vertices)[s].status = EnExploration;
	(graph->vertices)[s].debut = (*time)++;
	
	forr (j, graph->S) {
		if (has_edge(graph, s, j) && (graph->vertices)[j].status == NonVu) {
			(graph->vertices)[j].parent = s;
			printf("a visit %i %i\n", s, j);
			visit(graph, j, time);
		}
	}

	(graph->vertices)[s].status = Vu;
	(graph->vertices)[s].fin = (*time)++;
}

void depth_search (digraph* graph, uint* order) {
	uint time = 1;

	forr (i, graph->S) {
		(graph->vertices)[i].status = NonVu;
		(graph->vertices)[i].parent = i;
	}

	forr (_i, graph->S) {
		uint i = order[_i];
		if ((graph->vertices)[i].status == NonVu) {
			printf("New visit %i\n", i);
			visit(graph, i, &time);
		}
	}
}

digraph* transpose (digraph *graph) {
	digraph* graphT = malloc(sizeof(digraph));
	assert(graph != NULL && graphT != NULL);

	uint size = graph->s;
	uint S = size*size;

	// initialisation des constantes et allocations
	graphT->s = size;
	graphT->S = S;
	graphT->vertices = (node*) calloc(S, sizeof(node));
	graphT->edges = (bool*) malloc(S * S * sizeof(bool));

	assert( graphT->vertices != NULL && graphT->edges != NULL);
	
	forr (i, S) forr(j, S) {
		graphT->edges[i + j*S] = graph->edges[j + i*S];
	}
	return graphT;
}

uint valof (uint i, digraph* G, uint* table) {
	return (G->vertices)[table[i]].fin;
}

void swap (uint i, uint j, uint* table) {
	uint temp = table[i];
	table[i] = table[j];
	table[j] = temp;
}

void sort (digraph* G, uint* table, uint from, uint to, uint maxval) {
	//printf("sort %i %i \n", from, to);
	assert(from <= to);
	assert(0 <= from);
	assert(to < maxval);
	
	if (from == to) return;
	if (from+1 == to) {
		if (valof(from, G, table) < valof(to, G, table)) {
			swap(from, to, table);
		}
		return;
	}

	// pivot is the first value
	uint piv_val = valof(from, G, table);
	
	uint i = from+1; // first element smaller than the pivot
	uint j = to; // last element greater than the pivot

	while (i < j) {
		if (i>= maxval) {
			printf("I is getting out of control(%i)\n", i);
			exit(0);
		}
		// printf("%2i %2i ", i, table[i]);
		// printf("%3i ,", valof(i, G, table));
		// printf(" %2i %2i ", j, table[j]);
		// printf("%3i \n", valof(j, G, table));
		if (valof(i, G, table) < piv_val) {
			i++;
		} else if (valof(j, G, table) >= piv_val) {
			j--;
		} else {
			swap(i, j, table);
			i++;
			j--;
		}
	}
	swap(from, i, table);

	sort(G, table, from, i-1, maxval);
	if (i != to)
		sort(G, table, i+1, to, maxval);
}

/**************\
|* Calcul CFC *|
\**************/
void scc (digraph* graph) {
	uint* order = range(0, graph->S);

	depth_search(graph, order);
	sort (graph, order, 0, graph->S-1, graph->S);
	printf("S = %i\n", graph->S);

	digraph *graphT = transpose(graph);
	depth_search(graphT, order);
	
	forr (i, graph->S) {
		(graph->vertices)[i].parent = (graphT->vertices)[i].parent;
	}

	forr (i, graph->S) {
		printf("p : %i %i\n", i, get_root(graph, i));
	}

	release_graph(graphT);
	free(graphT);
}

uint get_root(digraph * graph, uint i) {
	if ((graph->vertices)[i].parent == i) return i;
	//printf("%i not self-enfanted\n", i);
	// pour optimiser, réasigne la valeur du parent avant de la renvoyer
	(graph->vertices)[i].parent = get_root(graph, (graph->vertices)[i].parent);
	return ((graph->vertices)[i].parent);
}