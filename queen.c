#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define print printf

int *pos;
int nqueen;

int setrow(int row);
int safep(int row, int col);
int abs(int x);
int show(void);

main(int argc, char **argv) {
	nqueen = 8;
	if(argc > 1)
		nqueen = atoi(argv[1]);

	pos = malloc(nqueen * sizeof(pos[0]));
	setrow(0);
}

// 0 ... (nqueen - 1)
setrow(int row) {
	int i;

	if(row >= nqueen) { // done
		show();
		return 0;
	}
	for(i=0; i<nqueen; i++) {
		if(!safep(row, i))
			continue;
		pos[row] = i;
		if(setrow(row + 1) == 0)
			return;	// continue, to enum all the results
	}
	return -1;
}

safep(int row, int col) {
	int i;

	for(i=0; i<row; i++) {
		if(pos[i] == col)
			return 0;
		if(abs(pos[i] - col) == abs(row - i))
			return 0;
	}
	return 1;
}

abs(int x) {
	if(x < 0)
		return -x;
	return x;
}

show(void) {
	int i, j;

	for(i=0; i<nqueen; i++) {
		for(j=0; j<pos[i]; j++)
			print("+");
		print("o");
		for(j++; j<nqueen; j++)
			print("+");
		print("\n");
	}
}
