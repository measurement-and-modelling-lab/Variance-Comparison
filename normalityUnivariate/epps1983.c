#include "math.h"
void epps1983(double* x, double* term1, double S2, int* n){
	double term =0;
	for(int i=0; i< *n; i++){
		for(int j=0; j< *n; j++){
			term += exp((-0.5*pow((x[i]-x[j]),2))/(S2));
			//term = term + 1.0;
			
			
		}
	}
	*term1 = term;
}