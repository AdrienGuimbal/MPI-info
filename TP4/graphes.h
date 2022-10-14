#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define forr(i, MAX) for (uint i = 0; i < (MAX); i++)

typedef unsigned int uint;

enum state {
    NonVu,
    EnExploration,
    Vu
};

typedef struct node {
    uint debut;
    uint fin;
    int parent;
    enum state status;
} node;

typedef struct {
    uint S;
    uint s;
    node *vertices;
    bool *edges;
} digraph; // direct graphs

void set_edge (digraph *graph, uint s, uint t, bool is);
bool has_edge (const digraph *graph, uint s, uint t);
void init_graph (digraph * graph, uint size);
void release_graph (digraph* graph);
void to_neato (const digraph graph, const char* filename);
void depth_search (digraph* graph, uint* order);
uint* range (uint a, uint b);
digraph* transpose(digraph* graph);
void scc (digraph* graph);
uint get_root(digraph * graph, uint i);


int main();