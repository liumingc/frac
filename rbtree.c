#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define nil ((void*)0)

enum
{
	RED = 0,
	BLACK = 1,
};

typedef struct Node {
	struct Node *parent;
	struct Node *left;
	struct Node *right;
	// todo need generic
	int key;
	int value;
	int color;
} Node;

typedef struct {
	Node *root;
} RBT;

Node *rbt_get(RBT*, int);
void rbt_put(RBT *tree, int key, int value);
void rbt_left_rotate(RBT *tree, Node *x);
void rbt_right_rotate(RBT *tree, Node *x);
void rbt_put_fixup(RBT *tree, Node *x);
void rbt_show(RBT *tree);
void rbt_del(RBT *tree, int key);

Node* rbt_get(RBT* tree, int key)
{
	Node *x = tree->root;
	while(x!=nil)
	{
		int cmp = key - x->key;
		if(cmp < 0)
			x = x->left;
		else if(cmp > 0)
			x = x->right;
		else
			return x;
	}

	return nil;
}

Node *node(int key, int value)
{
	Node *n = malloc(sizeof(Node));
	memset(n, 0, sizeof(*n));
	n->key = key;
	n->value = value;
	n->color = RED;
	return n;
}

void rbt_transplant(RBT *tree, Node *u, Node *v)
{
	if(u->parent == nil)
	{
		tree->root = v;
		//if(v)
			v->parent = nil;
	}
	else if(u == u->parent->left)
	{
		u->parent->left = v;
		//if(v)
			v->parent = u->parent;
	}
	else
	{
		u->parent->right = v;
		//if(v)
			v->parent = u->parent;
	}
}

void rbt_put(RBT *tree, int key, int value)
{
	Node *y = nil;
	Node *x = tree->root;
	while(x!=nil)
	{
		int cmp = key - x->key;
		y = x;
		if(cmp < 0)
			x = x->left;
		else if(cmp > 0)
			x = x->right;
		else
		{
			x->value = value;
			return;
		}
	}

	x = node(key, value);
	if(y == nil)
	{
		tree->root = x;
		x->color = BLACK;
	}
	else
	{
		if(key < y->key)
			y->left = x;
		else
			y->right = x;
		x->parent = y;
		if(y->color == RED)
			rbt_put_fixup(tree, x);
	}
}

int isred(Node *n)
{
	return (n!=nil && n->color == RED);
}

int isblack(Node *n)
{
	return (n==nil || n->color == BLACK);
}


void rbt_left_rotate(RBT *tree, Node *x)
{
	Node *y = x->right;
	Node *z = y->left;

	x->right = z;
	if(z)
		z->parent = x;
	
	rbt_transplant(tree, x, y);
	y->left = x;
	x->parent = y;
	//printf("one step ...\n");
	//rbt_show(tree);
}

void rbt_right_rotate(RBT *tree, Node *x)
{
	Node *y = x->left;
	Node *z = y->right;

	x->left = z;
	if(z)
		z->parent = x;
	
	rbt_transplant(tree, x, y);
	y->right = x;
	x->parent = y;
	//printf("one step ...\n");
	//rbt_show(tree);
}

void rbt_put_fixup(RBT *tree, Node *x)
{
	Node *y;

	while(isred(x->parent))
	{
		Node *g = x->parent->parent;
		if(x->parent == g->left)
			y = g->right;
		else
			y = g->left;

		if(isred(y))
		{
			x->parent->color = BLACK;
			y->color = BLACK;
			x = g;
			x->color = RED;
			//printf("one step ...\n");
			//rbt_show(tree);
			continue;
		}
		else
		{
			if(x->parent == g->left)
			{
				if(x == x->parent->right)
				{
					rbt_left_rotate(tree, x->parent);
					x = x->left;
				}
				g->color = RED;
				x->parent->color = BLACK;
				rbt_right_rotate(tree, g);
			}
			else
			{
				if(x == x->parent->left)
				{
					rbt_right_rotate(tree, x->parent);
					x = x->right;
				}
				g->color = RED;
				x->parent->color = BLACK;
				rbt_left_rotate(tree, g);
			}
		}
	}

	tree->root->color = BLACK;
}

Node fake = {
	nil, nil, nil,
	-1, -1, BLACK,
};

void rbt_delete_fixup(RBT *tree, Node *x)
{
	Node *w;
	while(x != tree->root && isblack(x)) {
		if(x == x->parent->left) {
			w = x->parent->right;

			if(w->color == RED) {
				x->parent->color = RED;
				w->color = BLACK;
				rbt_left_rotate(tree, x->parent);
			} else {
				if(isblack(w->left) && isblack(w->right)) {
					w->color = RED;
					x = x->parent;
				} else {
					if(isblack(w->right)) {
						w->color = RED;
						w->left->color = BLACK;
						rbt_right_rotate(tree, w);
						w = x->parent->right;
					}

					// w->right->color == RED
					w->color = x->parent->color;
					x->parent->color = BLACK;
					w->right->color = BLACK;
					rbt_left_rotate(tree, x->parent);
					x = tree->root;
				}
			}
		} else {
			w = x->parent->left;

			if(w->color == RED) {
				x->parent->color = RED;
				w->color = BLACK;
				rbt_left_rotate(tree, x->parent);
			} else {
				if(isblack(w->left) && isblack(w->left)) {
					w->color = RED;
					x = x->parent;
				} else {
					if(isblack(w->left)) {
						w->color = RED;
						w->left->color = BLACK;
						rbt_left_rotate(tree, w);
						w = x->parent->left;
					}

					// w->left->color == RED
					w->color = x->parent->color;
					x->parent->color = BLACK;
					w->left->color = BLACK;
					rbt_right_rotate(tree, x->parent);
					x = tree->root;
				}
			}
		}
	}

	x->color = BLACK;
}

void rbt_transplant_nil(RBT *tree, Node *y)
{
	if(y != &fake)
		return;
	
	if(y->parent == nil)
		tree->root = nil;
	else if(y == y->parent->left)
		y->parent->left = nil;
	else
		y->parent->right = nil;
}

Node* rbt_next(RBT *tree, Node *x)
{
	Node *y = x->right;

	while(y->left)
		y = y->left;
	return y;
}

void rbt_del(RBT *tree, int key)
{
	Node *x = rbt_get(tree, key);
	if(x == nil)
		return;

	Node *y, *z;
	if(x->left == nil) {
		z = x;
		y = x->right;
	} else if(x->right == nil) {
		z = x;
		y = x->left;
	} else {
		z = rbt_next(tree, x);
		y = z->right;
	}

	if(y == nil) {
		y = &fake;
		y->parent = z;
	}
	rbt_transplant(tree, z, y);

	x->key = z->key;
	x->value = z->value;

	if(isblack(z))
		rbt_delete_fixup(tree, y);

	rbt_transplant_nil(tree, y);

	free(z);
}

void show(Node *x, int indent)
{
	if(x == nil)
		return;
	int i;
	show(x->left, indent+1);
	for(i=0; i<indent; i++)
		printf("   ");
	printf("%d%c\n", x->key, x->color == RED?'*':' ');
	show(x->right, indent+1);
}

void rbt_show(RBT *tree)
{
	show(tree->root, 0);
}

int main(int argc, const char *argv[])
{
	int c;
	setbuf(stdout, nil);
	RBT * tree = malloc(sizeof(RBT));
	tree->root = nil;
	while((scanf(" %d", &c) == 1))
	{
		if(c > 0) {
			printf("add %d\n", c);
			rbt_put(tree, c, 1);
			rbt_show(tree);
			printf("=========\n\n");
		} else {
			c = -c;
			printf("del %d\n", c);
			rbt_del(tree, c);
			rbt_show(tree);
			printf("=========\n\n");
		}
	}
	return 0;
}
