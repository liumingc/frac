#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <set>

using namespace std;

#define DBG 0

//#define nil ((void*)0)
#define nil 0

enum Color {
	R = 0,
	B,
};

struct Node
{
	struct Node *left;
	struct Node *right;
	struct Node *parent;
	int key;
	int value;
	int color;

	Node()
	{
		//printf("sv\n");
		left = nil;
		right = nil;
		parent = nil;
		key = 0;
		value = 0;
		color = R;
	}

	Node(int k, int v)
	{
		//printf("sii\n");
		key = k;
		value = v;
		left = nil;
		right = nil;
		parent = nil;
		color = R;
	}
};

struct Rbt
{
	Node *root;

	Rbt()
	{
		root = nil;
	}

	void add(int key, int value)
	{
		if(root == nil)
		{
			root = new Node(key, value);
			root->color = B;
			return;
		}

		Node *np = root;
		Node *parent = nil;
		for(;np;)
		{
			parent = np;
			if(key < np->key)
				np = np->left;
			else if(key > np->key)
				np = np->right;
			else
			{
				np->value = value;
				return;
			}

		}
		np = new Node(key, value);
		np->parent = parent;
		if(key < parent->key)
			parent->left = np;
		else
			parent->right = np;
		if(parent->color == R)
			addfix(np);
	}

	void addfix(Node *x)
	{
		Node *p, *g, *w;

		for(;x!=root && isRed(x->parent);)
		{
			p = x->parent;
			g = p->parent;
			if(p  == g->left)
			{
				w = g->right;
				if(isRed(w))
				{
					w->color = B;
					p->color = B;
					g->color = R;
					x = g;
				}
				else
				{
					if(x == p->right)
					{
						rotate(p, x);
						p = x;
					}
					rotate(g, p);
					p->color = B;
					g->color = R;
					x = root;
				}
			}
			else
			{
				w = g->left;
				if(isRed(w))
				{
					w->color = B;
					p->color = B;
					g->color = R;
					x = g;
				}
				else
				{
					if(x == p->left)
					{
						rotate(p, x);
						p = x;
					}
					rotate(g, p);
					p->color = B;
					g->color = R;
					x = root;
				}
			}
		}
		root->color = B;

	}

	void rotate(Node *x, Node *y)
	{
		if(DBG)
		{
			printf("[before rotate %d %d\n", x->key, y->key);
			show();
			printf("]\n");
		}
		if(y == x->left)
		{
			x->left = y->right;
			if(y->right)
				y->right->parent = x;	// fix: add this at 12:20 20170226
			y->right = x;
		}
		else
		{
			x->right = y->left;
			if(y->left)
				y->left->parent = x;
			y->left = x;
		}

		if(x->parent == nil)
		{
			root = y;
		}
		else
		{
			if(x == x->parent->right)
				x->parent->right = y;
			else
				x->parent->left = y;
		}
		y->parent = x->parent;
		x->parent = y;
		if(DBG)
		{
			printf("[after\n");
			show();
			printf("]\n");
		}
	}

	bool isRed(Node *np)
	{
		return np && np->color == R;
	}

	bool isBlack(Node *np)
	{
		return np == nil || np->color == B;
	}

	void show()
	{
		show1(root, 0);
	}

	void show1(Node *n, int depth)
	{
		if(n == nil)
			return;
		show1(n->left, depth+1);
		for(int i=0; i<depth; i++)
			for(int j=0; j<4; j++)
				printf(" ");
		printf("%3d%c\n", n->key, n->color==R?'*':' ');
		//printf("%d:%d%c\n", n->key, n->parent?n->parent->key:-1, n->color==R?'*':' ');
		show1(n->right, depth+1);
	}

	int count()
	{
		return count1(root);
	}

	int count1(Node *n)
	{
		if(n == 0)
			return 0;
		return 1 + count1(n->left) + count1(n->right);
	}
};



int main(int argc, char **argv)
{
	int k;
	Rbt *rbt = new Rbt();
	setbuf(stdout, nil);
	/*
	while(scanf("%d", &k) == 1)
	{
		rbt->add(k, 1);
		rbt->show();
		printf("============\n\n");
	}
	*/
	set<int> coll;

	for(int i=0; i<100; i++)
	{
		int r = random() % 500;
		if(coll.find(r) != coll.end())
			continue;
		coll.insert(r);

		printf("[In%d] === %d ===\n", i, r);
		rbt->add(r, 1);
		rbt->show();
		printf("============\n\n");
		if(rbt->count() != coll.size())
		{
			printf("oops!\n");
			break;
		}
	}
	return 0;
}
