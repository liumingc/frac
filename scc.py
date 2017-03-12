#!/usr/bin/env python3


index = 0
stack = []

class Vet(object):
    def __init__(self, value):
        self.value = value
        self.on_stack = False
        self.index = False
        self.next = []

    def __repr__(self):
        return '%s' % self.value

    def __str__(self):
        return '%s' % self.value

    def __eq__(self, other):
        return self.value == other.value

def tarjan(vs):
    for v in vs:
        if v.index == False:
            strong_connect(v)

def strong_connect(v):
    global index
    global stack

    v.index = index
    v.low = index
    index = index + 1

    stack.append(v)
    v.on_stack = True

    for w in v.next:
        if w.index == False:
            strong_connect(w)
            v.low = min(v.low, w.low)
        elif w.on_stack == True:
            v.low = min(v.low, w.low)

    if v.low == v.index:
        #scc = [v]
        scc = []
        w = stack.pop()
        w.on_stack = False
        scc.append(w)

        while w != v and len(stack) > 0 :
            w = stack.pop()
            w.on_stack = False
            scc.append(w)

        print(scc)


if __name__ == '__main__':
    coll = {}
    f = open('input.txt')
    for line in f:
        pass
        a,b = line[:-1].split(',')

        if b not in coll:
            vb = Vet(b)
            coll[b] = vb
        else:
            vb = coll[b]

        if a not in coll:
            v = Vet(a)
            v.next.append(vb)
            coll[a] = v
        else:
            coll[a].next.append(vb)

    f.close()

    vs = coll.values()
    tarjan(vs)
