#include "../src/aggregate.h"
#include "../src/aggregation.h"

int main() {
	Aggregation a = newAggregation(500);
	char* names[9][4] = { 
		{ "Braga", "Braga", "Dume", NULL },				// 1
		{ "Braga", "Guimarães", "Pevidém", NULL },		// 2
		{ "Porto", "Porto", "Miragaia", NULL },			// 3
		{ "Lisboa", "Lisboa", "Amadora", NULL},			// 4
		{ "Lisboa", "Lisboa", NULL },					// 5
		{ "Braga", NULL },								// 6
		{ "Braga", "Guimarães", NULL,},					// 7
		{ "Braga", "Guimarães", "Azurém", NULL},		// 8
		{ "Lisboa", "Lisboa", "Arroios", NULL}			// 9
	};

	for (int i = 0; i < 9; i++)
		updateAggregation(a, names[i], i + 1);

	printAggregation(a);
}
