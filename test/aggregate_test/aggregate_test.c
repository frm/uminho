#include "../../src/aggregate.h"
#include "../../src/aggregation.h"

int main() {
	Aggregation a = newAggregation(500);
	char* names[9][4] = { 
		{ "Braga", "Braga2", "Dume", NULL },				// 1
		{ "Braga", "Guimarães", "Pevidém", NULL },		// 2
		{ "Porto", "Porto2", "Miragaia", NULL },			// 3
		{ "Lisboa", "Lisboa2", "Amadora", NULL},			// 4
		{ "Lisboa", "Lisboa2", NULL },					// 5
		{ "Braga", NULL },								// 6
		{ "Braga", "Guimarães", NULL,},					// 7
		{ "Braga", "Guimarães", "Azurém", NULL},		// 8
		{ "Lisboa", "Lisboa2", "Arroios", NULL}			// 9
	};

	for (int i = 0; i < 9; i++)
		updateAggregation(a, names[i], i + 1);


	char* agg[3][4] = {
		{ "Braga", NULL },
		{ "Braga", "Guimarães", NULL },
		{ "Braga", "Braga2", "Dume", NULL }
	};

	collectAggregate(a, agg[0], 0, "1");		// Total de Braga (distrito)
	collectAggregate(a, agg[0], 1, "2");		// Concelhos de Braga: Braga, Guimarães
	collectAggregate(a, agg[0], 2, "3");		// Freguesias de Braga (distrito): Dume, Pevidém, Azurém
	collectAggregate(a, agg[1], 0, "4");		// Total de Braga:Guimarães
	collectAggregate(a, agg[1], 1, "5");		// Freguesias de Guimarães: Pevidém, Azurém
	collectAggregate(a, agg[2], 0, "6");		// Total de Dume
	collectAggregate(a, agg[2], 1, "7");		// Vazio
	collectAggregate(a, agg[2], 1, "8");		// Vazio


	printAggregation(a);
}
