#!/usr/bin/python

import sys
import re

tokens = []
syms = []	# syntax objs, or symbols
#splitter = re.compile(r'[:\| \t]')

class Tok(object):
    def __init__(self, path):
        self.fin = open(path)
        self.tokens = []

    def get(self):
        if len(self.tokens) == 0:
            # need to read from file
            while True:
                line = self.fin.readline()
                if line == '':
                    return ''

                #lst = splitter.split(line)
                lst = line.split()
                if len(lst) > 0:
                    self.tokens = [x.strip() for x in lst]
                    break

        # return from self.tokens
        ans, self.tokens = self.tokens[0], self.tokens[1:]
        #print('get -> "%s"' % ans)
        return ans

    def put(self, x):
        self.tokens.insert(0, x)

def read_tokens(ts):
    global tokens

    while True:
        tok = ts.get()
        if tok == '':
            break;
        if tok == '%%':
            print('end for tokens')
            print(tokens)
            break
        if tok == '%token':
            continue
        else:
            tokens.append(tok)
            syms.append(tok)


# a rule is lhs = rhs, where lhs is a syntax-object,
# rhs is a list of syntax-object
rules = []

def print_rules(rs):
    for r in rs:
        print('%s ::= %s' % (r[0], r[1]))

def print_syntax_objects():
    out = ''
    for x in syms:
        out += '%s ' % x
    print(out)

def read_rules(ts):
    global rules

    STAT_GET_LHS = 1
    STAT_GET_RHS = 2
    state = STAT_GET_LHS
    while True:
        tok = ts.get()
        if tok == '':
            return
        
        if tok == '%%':
            print('end for rules')
            #print(rules)
            print_rules(rules)
            break

        ts.put(tok)

        # left-hand-side
        while True:
            tok = ts.get()
            if tok == ':':
                break
            else:
                lhs = tok
                if not tok in syms:
                    syms.append(tok)

        # right-hand-side
        rhs = []
        while True:
            tok = ts.get()
            if tok == ';':
                rules.append((lhs, rhs))
                break
            elif tok == '':
                return
            elif tok == '|':
                rules.append((lhs, rhs))
                rhs = []
            else:
                rhs.append(tok)
                if not tok in syms:
                    syms.append(tok)

def test_ts(path):
    ts = Tok(path)
    while True:
        tok = ts.get()
        print('tok -> "%s"' % tok)
        if tok == '':
            break


def get_prod(sym):
    prod = []
    global rules
    for i, r in enumerate(rules):
        if r[0] == sym:
            prod.append(i)

    return prod

def print_item(rulno, pos):
    global rules

    rul = rules[rulno]
    out = '%s ::= ' % rul[0]
    rhs = rul[1]
    for i, v in enumerate(rhs):
        if i == pos:
            out += '_'
        out += ' %s ' % v
    if pos >= len(rhs):
        out += '_'
    print(out)

def print_items(items):
    for i in items:
        print_item(i[0], i[1])

# item is (rule-no, pos)
# rule is (lhs, rhs)
# todo
def closure(items):
    global rules

    j = items

    while True:
        oj = j
        nomore = True
        for x in oj:
            rule, pos = rules[x[0]], x[1]
            rhs = rule[1]
            if pos >= len(rhs):
                continue

            sym = rule[1][pos]
            prods = get_prod(sym)
            for y in prods:
                if not (y, 0) in j:
                    j.append((y, 0))
                    nomore = False
        if nomore == True:
            break

    return j

# item is (ruleno, pos)
def get_next_sym(item):
    global rules

    ruleno, pos = item
    rule = rules[ruleno]
    if pos >= len(rule[1]):
        return None

    return rule[1][pos]


# i is an item-collections
# will return a new item-collections
# which skip pass symbol x
def gotox(items, sym):
    j = []
    for x in items:
        s = get_next_sym(x)
        if s == sym:
            # yes
            j.append((x[0], x[1] + 1))
    return closure(j)

# so far so good.
def lr0():
    global rules

    i0 = closure([(0, 0)])
    C = [i0]
    while True:
        fini = True
        for icol in C:
            for sym in syms:
                inext = gotox(icol, sym)
                if not inext in C:
                    C.append(inext)
                    fini = False
        if fini == True:
            break

    return C

def print_lr0(C):
    for i,icol in enumerate(C):
        print(' --- i%d ---' % i)
        print_items(icol)


added = False
Empty = '**empty**'
End = '**end**'
def add_elem(s, e):
    global added
    if e in s:
        return
    s.append(e)
    added = True

def add_set(s, s1):
    for x in s1:
        add_elem(s, x)

def is_token(t):
    global tokens
    if t in tokens:
        return True
    else:
        return False

# yeah, first seems to work
def first(syms):
    global added
    fset = {}
    for x in syms:
        if is_token(x):
            fset[x] = [x]
        else:
            fset[x] = []


    while True:
        added = False
        for x in syms:
            if is_token(x):
                continue
            prods = get_prod(x)
            for ruleno in prods:
                global rules
                rule = rules[ruleno]
                if len(rule[1]) == 0:
                    add_elem(fset[x], Empty)
                else:
                    empty = True
                    for y in rule[1]:
                        add_set(fset[x], fset[y])
                        if not Empty in fset[y]:
                            empty = False
                            break

                    if empty == True:
                        add_elem(fset[x], Empty)
        if added == False:
            return fset

def set_sub(s, e):
    return [x for x in s if x != e]

def follow(syms, ffirst, start):
    global added, End, Empty, rules
    ffol = {}

    for x in syms:
        ffol[x] = []

    ffol[start].append(End)

    while True:
        added = False
        for rule in rules:
            lhs, rhs = rule

            n = len(rhs)
            if n == 0:
                continue

            for i in range(n):
                empty = True
                for j in range(i+1, n):
                    s = ffirst[rhs[j]]
                    add_set(ffol[rhs[i]], set_sub(s, Empty))
                    if not Empty in s:
                        empty = False
                        break
                if empty == True:
                    add_set(ffol[rhs[i]], ffol[lhs])

        if added == False:
            return ffol

def print_first_follow(ff, fol):
    print(' --- first ---')
    for x in ff:
        print('%s -> %s' % (x, ff[x]))
    print(' --- follow ---')
    for x in fol:
        print('%s -> %s' % (x, fol[x]))

def do_test(path):
    ts = Tok(path)
    read_tokens(ts)
    read_rules(ts)
    print('=========')
    print_syntax_objects()

    i0 = closure([(0, 0)])
    print(i0)
    print_items(i0)

    ff = first(syms)
    fol = follow(syms, ff, 'start')
    print_first_follow(ff, fol)

if __name__ == '__main__':
    do_test('test1.y')

    print('==========')
    C = lr0()
    print_lr0(C)
