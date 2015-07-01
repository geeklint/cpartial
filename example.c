/*
Copyright (c) 2015, Sky Leonard
All rights reserved.
For more info see COPYING or http://opensource.org/licenses/BSD-3-Clause
 */

#include <stdlib.h>
#include <time.h>

#include "partial.h"

#define NARRAYELEMENTS 24

static int array[NARRAYELEMENTS];

static void shuffle(){
	int i;
	int t;
	size_t j;

	for (i = 0; i < NARRAYELEMENTS - 1; ++i){
		j = i + rand() / (RAND_MAX / (NARRAYELEMENTS - i) + 1);
		t = array[j];
		array[j] = array[i];
		array[i] = t;
	}
}

static void printarray(){
	int i;

	for (i = 0; i < NARRAYELEMENTS; ++i){
		printf("%d ", array[i]);
	}
	puts("\n");
}

static int compar(int ud, int * a, int * b){
	if (*a <  *b)
		return -ud;
	else if (*a >  *b)
		return ud;
	else
		return 0;
}

int main(){
	int i, res;

	srand(time(NULL));
	for (i = 0; i < NARRAYELEMENTS; ++i){
		array[i] = i;
	}

	shuffle();
	printarray();

	res = partial_quick(
			NULL,								// return value (void)
			"v pTT*", &qsort,					// lib func
			"i ipp", &compar, 1,				// our func
			array, NARRAYELEMENTS, sizeof(int),	// qsort args
			1);									// compar args
	if (res != 0){
		return 1;
	}

	printarray();
	shuffle();
	printarray();

	res = partial_quick(
			NULL,								// return value (void)
			"v pTT*", &qsort,					// lib func
			"i ipp", &compar, 1,				// our func
			array, NARRAYELEMENTS, sizeof(int),	// qsort args
			-1);								// compar args
	if (res != 0){
		return 2;
	}

	printarray();

	return 0;
}
