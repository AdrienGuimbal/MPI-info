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

	//uint* order = range(0, size*size);
	//depth_search(&grafield, order);

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
	graph->vertices = (node*) calloc(S, sizeof(node));
	graph->edges = (bool*) malloc(S * S * sizeof(bool));

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
		fprintf(file, "\t%i[pos=\"%i,%i!\" color=\"%i\"];\n", i, i/graph.s, i%graph.s, i%13);
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

	graph->vertices[s].debut = (*time)++;
	graph->vertices[s].status = EnExploration;
	
	forr (j, graph->S) {
		if (has_edge(graph, s, j) && graph->vertices[j].status == NonVu) {
			graph->vertices[j].parent = s;
			visit(graph, j, time);
		}
	}

	graph->vertices[s].fin = (*time)++;
	graph->vertices[s].status = Vu;
}

void depth_search (digraph* graph, uint* order) {
	uint time = 1;

	forr (i, graph->S) {
		graph->vertices[i].status = NonVu;
		graph->vertices[i].parent = i;
	}

	forr (_i, graph->S) {
		uint i = order[_i];
		if (graph->vertices[i].status == NonVu) {
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
	graphT->edges = (bool*) calloc(S * S, sizeof(bool));

	assert( graphT->vertices != NULL && graphT->edges != NULL);
	
	// ajout aléatoire des arêtes
	forr ( i, size ) forr ( j, size ) {
		uint curr = idx(  i,   j, graph->s);
		uint left = idx(  i, j+1, graph->s);
		uint down = idx(i+1,   j, graph->s);

		if (i < size )
			set_edge(graphT, left, curr, has_edge(graph, curr, left));
		
		if (j < size)
			set_edge(graphT, down, curr, has_edge(graph, curr, down));
	}
	return graphT;
}

void swap (uint* table, uint i, uint j) {
	table[i] += table[j];
	table[j] = table[i] - table[j];
	table[i] -= table[j];
}

uint valof (uint i, digraph* graph) {
	return graph->vertices[i].fin;
}

void sort (digraph *G, uint *order, uint from, uint to) {
	if (from == to) return;
	assert(from < to && from >=0);

	uint piv_val = valof(order[from], G); // pivot, ceux en dessous serons > ceux au dessus <=
	uint first_not_small = to;
	uint last_not_big = from + 1;

	
	while (last_not_big + 1 != first_not_small) {
		if (valof(last_not_big, G) > piv_val) {
			last_not_big++;
		} else if (valof(first_not_small, G) <= piv_val) {
			first_not_small--;
		} else {
			swap(order, last_not_big, first_not_small);
			last_not_big++;
			first_not_small--;
		}
	}
	
	swap(order, 0, last_not_big);

	sort(G, order, from, last_not_big-1);
	sort(G, order, first_not_small, to);
}

/**************\
|* Calcul CFC *|
\**************/
void scc (digraph* graph) {
	uint* order = range(0, graph->S);
	depth_search(graph, order);
	sort (graph, order, 0, graph->S-1);
	digraph *graphT = transpose(graph);
	depth_search(graphT, order);
	
	forr (i, graph->S) {
		graph->vertices[i].parent = graphT->vertices[i].parent;
	}

	release_graph(graphT);
	free(graphT);
}

uint get_root(digraph * graph, uint i) {
	if (graph->vertices[i].parent == i) return i;
	return (
		// pour optimiser, réasigne la valeur du parent avant de la renvoyer
		graph->vertices[i].parent = get_root(graph, graph->vertices[i].parent)
	);
}