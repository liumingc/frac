#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *vram;
static int row, col;

void do_display()
{
	int x, y;
	for(y=0; y<col; y++)
	{
		for(x=0; x<row; x++)
			putchar(vram[y*row+x]);
		putchar('\n');
	}
	printf("------------\n\n");
}

int fastpow(int base, int n)
{
	if(n == 0)
		return 1;
	if(n & 1)
	{
		return base * fastpow(base, n - 1);
	}
	else
	{
		return fastpow(base*base, n/2);
	}
}

int curdir;
int prevdir;
int curx;
int cury;

extern void side(int n);
extern void init(int n);
extern void right(int deg);
extern void left(int deg);
extern void line();

void
koch(int n)
{
	side(n-1);
	right(120);
	side(n-1);
	right(120);
	side(n-1);
}

void
side(int n)
{
	if(n == 0)
	{
		line();
		return;
	}

	side(n-1);
	left(60);
	side(n-1);
	right(120);
	side(n-1);
	left(60);
	side(n-1);
}

void
init(int n)
{
	int w = fastpow(3, n-1)*2;
	int h = fastpow(3, n-1) * 4/3+1;
	printf("w=%d, h=%d\n", w, h);
	row = w;
	col = h;

	vram = (char*)malloc(w*h);
	//memset(vram, '*', w*h);
	memset(vram, ' ', w*h);
	//do_display();
	curdir = 0;
	prevdir = curdir;

	// curx, cury
	curx = 0;
	cury = h/4;
}

void
left(int deg)
{
	prevdir = curdir;
	curdir += deg;
	if(curdir >= 360)
		curdir -= 360;
}

void
right(int deg)
{
	prevdir = curdir;
	curdir -= deg;
	if(curdir < 0)
		curdir += 360;
}

void
line()
{
	if(curdir == 0)
	{
		vram[cury*row+curx] = '_';
		vram[cury*row+curx+1] = '_';
		curx++;
		curx++;
	}
	else if(curdir < 90)
	{
		vram[cury*row+curx] = '/';
		curx++;
		cury--;
	}
	else if(curdir < 180)
	{
		vram[cury*row+curx-1] = '\\';
		curx--;
		cury--;
	}
	else if(curdir == 180)
	{
		if(prevdir != 300)
			vram[cury*row+curx-1] = '_';
		vram[cury*row+curx-2] = '_';
		curx-=2;
	}
	else if(curdir < 270)
	{
		vram[(cury+1)*row+curx-1] = '/';
		cury++;
		curx--;
	}
	else
	{
		vram[(cury+1)*row+curx] = '\\';
		curx++;
		cury++;
	}
	//do_display();	// to view the side effect
}

int
main(int argc, char **argv)
{
	/*
	int i;
	for(i=0; i<8; i++)
		printf("3^%d=%d\n", i, fastpow(3, i));
	*/
	int n = 2;
	if(argc == 2)
		n = atoi(argv[1]);
	setbuf(stdout, 0);
	init(n);
	koch(n);
	do_display();
	return 0;
}
