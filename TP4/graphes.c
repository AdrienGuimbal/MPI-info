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
	to_neato(grafield, argv[2]);

	release_graph(&grafield);
	return 0;
}


/*****************\
|* Mise en place *|
\*****************/ 


int idx (uint i, uint j, uint width) {
	return i*width + j;
}

void set_edge (digraph *graph, uint s, uint t, bool is) {
	graph->edges[idx(s, t, graph->S)] = is; 
}

bool has_edge (const digraph *graph, uint s, uint t) {
	return graph->edges[idx(s, t, graph->S)];
}

void init_graph (digraph *graph, uint size) {
	uint S = size*size;
	
	graph->s = size;
	graph->S = S;
	graph->vertices = (node*) calloc(S, sizeof(node));
	graph->edges = (bool*) calloc(S * S, sizeof(bool));

	assert( graph->vertices != NULL && graph->edges != NULL);
	
	forr ( i, size ) forr ( j, size ) {
		//printf("%i %i\n", i, j);
		if (j != size-1) {
			bool dir = rand() % 2 ;
			set_edge(graph, idx(i,  j , size), idx(i, j+1, size),  dir);
			set_edge(graph, idx(i, j+1, size), idx(i,  j , size), !dir);
			//printf("\t%i\n", idx(i, j+1, size));
		}
		if (i != size-1) {
			bool dir = rand() % 2 ;
			set_edge(graph, idx( i , j, size), idx(i+1, j, size),  dir);
			set_edge(graph, idx(i+1, j, size), idx( i , j, size), !dir);
			//printf("\t%i\n", idx(i+1, j, size));
		}
	}
}

void release_graph (digraph* graph) {
	free(graph->vertices);
	free(graph->edges);
}

void to_neato (const digraph graph, const char* filename) {
	FILE* file = fopen(filename, "wt");
	assert(file != NULL);
	
	fprintf(file,"digraph G { \n\
	node [shape=point\n\
	      colorscheme=paired12\n\
	      width=0.4]\n\
	edge [colorscheme=paired12 penwidth=4]\n\
	edge [color=0]\n");

	forr (i, graph.S)
		fprintf(file, "\t%i[pos=\"%i,%i!\" color=\"%i\"];\n", i, i/graph.s, i%graph.s, i%13);
	
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

void recursive_deep_search (digraph* graph, uint parent, uint* time) {
	forr (j, graph->s) {
		if (has_edge(graph, parent, j)) {

			graph->vertices[j].parent = parent;
			graph->vertices[j].debut = (*time)++;

			recursive_deep_search(graph, j, time);
			
			graph->vertices[j].fin = (*time)++;
		}
	}
}

void depth_search (digraph* graph, uint* order) {
	uint* time;
	*time = 0;

	forr (i, order) {
		if (graph->vertices[i].parent == -1) {

			graph->vertices[i].debut = (*time)++;

			recursive_deep_search(graph, i, time);
			
			graph->vertices[i].fin = (*time)++;
		}
	}
}