#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


typedef struct {
	int type;
} obj;

typedef struct {
	int type;
	obj *car, *cdr;
} pair;

typedef struct {
	int type;
	int size;
	char *sval;
} str;

typedef struct {
	int type;
	int size;
	obj *vval;
} vec;

typedef struct {
	int type;
	int ival;
} num;

typedef struct {
	int type;
	char *name;
	obj *val;
} sym;

#define nil 0
#define print printf
#define eprint(a...) fprintf(stderr, a)
#define fatal(a...)			\
	do {					\
		fprintf(stderr, a);	\
		exit(1);			\
	} while(0);

enum
{
	TYPE_SYM,
	TYPE_NUM,
	TYPE_STR,
	TYPE_VEC,
	TYPE_PAIR,
	TYPE_BOOL,
//	TYPE_NIL,
};

enum
{
	MAX_LINE = 1024,
};

obj* sexp_read(void);
void sexp_write(obj* o, int recur);
obj* sexp_eval(obj *o);
void sexp_free(obj* o);
int read_char(void);
int peek_char(void);
int unread_char(int c);
num* make_num(int ival);
str* make_str(char *sval);
pair* make_pair(obj* car, obj* cdr);
sym* make_sym(char *name);
int islit(int c);


// to simplify, do no check. do concern about effectioncy

obj* sexp_read(void)
{
	int c;

	c = read_char();
	while(isblank(c))
		c = read_char();

	if(c == '"') {
		// str
		char sval[MAX_LINE];
		int i;

		for(i=0; i<MAX_LINE-1; i++)
		{
			c = read_char();
			if(c == '\0' || c == '"')
				break;
			sval[i] = c;
		}
		sval[i] = '\0';

		return (obj*)make_str(sval);
	} else if(c == '#') {
		// vector, true, false, etc.
	} else if(c == '(') {
		// pair

		pair *start, *cur, *prev;

		start = cur = prev = nil;

		for(;;) {
			obj *op = sexp_read();

			if(op == nil)
				return (obj*)start;

			cur = make_pair(op, nil);

			if(start == nil)
				start = cur;

			if(prev)
				prev->cdr = (obj*)cur;

			prev = cur;
		}

	}else if (c == ')') {

		return nil;

	} else if(isdigit(c)) {
		// num
		int ival = 0;
		while(isdigit(c)) {
			ival = ival*10 + c -'0';
			c = read_char();
		}
		unread_char(c);
		return (obj*)make_num(ival);
	} else {
		// symbol
		char sval[MAX_LINE];
		int i;

		for(i=0; i<MAX_LINE; i++) {
			sval[i] = c;
			c = read_char();
			if(!islit(c))
			{
				unread_char(c);
				break;
			}
		}
		return (obj*)make_sym(sval);
	}

	return nil;
}

void sexp_write(obj* o, int recur)
{
	if(o == nil) {
		if(recur)
			print(")");
		else
			print("()");

		return;
	}

	pair* pp = (pair*)o;
	switch(o->type)
	{
	case TYPE_NUM:
		{
			num *n = (num*)o;
			print("%d", n->ival);
		}
		break;
	case TYPE_PAIR:
		if(!recur)
			print("(");

		sexp_write(pp->car, 0);
		if(pp->cdr == nil)
			print(")");
		else {
			print(" ");
			sexp_write(pp->cdr, 1);
		}

		break;
	default:
		eprint("type=%d, not supported\n");
		break;
	}
}

obj* sexp_eval(obj *o)
{
}

int islit(int c)
{
	static char* lit_str = "!?.+-*/%$&@<>";

	return (isalnum(c) || strchr(lit_str, c));
}

static char linebuf[MAX_LINE];
static char *linep = linebuf;
int read_char(void)
{
	if(linep >= &linebuf[MAX_LINE])
		return 0;

	return *linep++;
}

int peek_char(void)
{
	return *linep;
}

int unread_char(int c)
{
	*--linep = c;
}

num* make_num(int ival)
{
	num *n = malloc(sizeof(num));

	n->type = TYPE_NUM;
	n->ival = ival;
	return n;
}

str* make_str(char *sval)
{
	str *s = malloc(sizeof(sval));
	int len = strlen(sval) + 1;

	s->type = TYPE_STR;
	s->size = len;
	s->sval = malloc(len);
	strcpy(s->sval, sval);

	return s;
}

pair* make_pair(obj* car, obj* cdr)
{
	pair* pp = malloc(sizeof(obj));

	pp->type = TYPE_PAIR;
	pp->car = car;
	pp->cdr = cdr;
	return pp;
}

sym* make_sym(char *sval)
{
	sym* s = malloc(sizeof(sym));
}

void sexp_free(obj* o)
{
	if(o == nil)
		return;

	switch(o->type)
	{
	case TYPE_NUM:
		free(o);
		break;
	case TYPE_STR:
		{
			str *s = (str*)o;
			free(s->sval);
			free(s);
		}
		break;
	case TYPE_PAIR:
		{
			pair* pp = (pair*)o;
			sexp_free(pp->car);
			sexp_free(pp->cdr);
			free(pp);
		}
		break;
	default:
		break;
	}
}

int main(int argc, char **argv)
{
	strcpy(linebuf, "(1 2 (3 4) 5)");
	//linep = linebuf;

	obj* op = sexp_read();
	sexp_write(op, 0);
	sexp_free(op);
	print("\n\n");
	return 0;
}
