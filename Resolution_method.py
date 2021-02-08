import ply.lex as lex
import ply.yacc as yacc

tokens = (
    'NAME',
    'AND',
    'OR',
    'NOT',
    'IMPLY',
    'LEFT',
    'RIGHT',
)

t_NAME = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_AND = r'/\\'
t_OR = r'\\/'
t_NOT = r'~'
t_IMPLY = r'->'
t_LEFT = r'\('
t_RIGHT = r'\)'
t_ignore = ' '


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()


def check_different_pair(pair):
    left = pair[0]
    right = pair[1]
    if left[0] == '~' and right[0] != '~' and left[1:] == right:
        return False
    if left[0] != '~' and right[0] == '~' and left == right[1:]:
        return False
    if left == right:
        return (('0', left))
    return pair


def imply(left, right):
    if left[0] == '~':
        left = left[1:]
    else:
        left = '~' + left
    return((left, right))


original = []


def p_expression(t):
    '''expression : NAME
                  | NOT NAME'''
    if t[1] == '~':
        if (('0', '~' + t[2])) not in original:
            original.append(('0', '~' + t[2]))
    else:
        if (('0', t[1])) not in original:
            original.append(('0', t[1]))


def p_expression_group(t):
    'expression : LEFT expression RIGHT'
    t[0] = t[2]


def p_expression_and(t):
    'expression : expression AND expression'


def p_expression_imply(t):
    'expression : two IMPLY two'
    pair = imply(t[1], t[3])
    result = check_different_pair(pair)
    if result and result not in original:
        original.append(result)


def p_expression_or(t):
    'expression : two OR two'
    pair = ((t[1], t[3]))
    result = check_different_pair(pair)
    if result and result not in original:
        original.append(result)


def p_two(t):
    '''two : NAME
           | NOT NAME'''
    if t[1] == '~':
        t[0] = '~' + t[2]
    else:
        t[0] = t[1]


def p_error(t):
    print("Syntax error at '%s'" % t.value)


parser = yacc.yacc()


def find_new(old_pairs, new_pairs):
    new = []
    long = len(new_pairs)
    for i in range(long):
        for old in old_pairs:
            result = compare(new_pairs[i], old)
            if result:
                new_pair = check_different_pair(result)
                if new_pair:
                    if new_pair == (('0', '0')):
                        return False
                    new.append(new_pair)
        for j in range(i+1, long):
            result = compare(new_pairs[i], new_pairs[j])
            if result:
                new_pair = check_different_pair(result)
                if new_pair:
                    if new_pair == (('0', '0')):
                        return False
                    new.append(new_pair)
    return new


def compare(pair_1, pair_2):
    if pair_1[0][0] == '~':
        word = pair_1[0][1:]
        if word == pair_2[0]:
            if pair_2[1] == '0':
                return ((pair_2[1], pair_1[1]))
            return ((pair_1[1], pair_2[1]))
        if word == pair_2[1]:
            if pair_2[0] == '0':
                return ((pair_2[0], pair_1[1]))
            return ((pair_1[1], pair_2[0]))
    if pair_1[1][0] == '~':
        word = pair_1[1][1:]
        if word == pair_2[0]:
            if pair_2[1] == '0':
                return ((pair_2[1], pair_1[0]))
            return ((pair_1[0], pair_2[1]))
        if word == pair_2[1]:
            if pair_2[0] == '0':
                return ((pair_2[0], pair_1[0]))
            return ((pair_1[0], pair_2[0]))
    if pair_2[0][0] == '~':
        word = pair_2[0][1:]
        if word == pair_1[0]:
            if pair_2[1] == '0':
                return ((pair_2[1], pair_1[1]))
            return ((pair_1[1], pair_2[1]))
        if word == pair_1[1]:
            if pair_2[1] == '0':
                return ((pair_2[1], pair_1[0]))
            return ((pair_1[0], pair_2[1]))
    if pair_2[1][0] == '~':
        word = pair_2[1][1:]
        if word == pair_1[0]:
            if pair_2[0] == '0':
                return ((pair_2[0], pair_1[1]))
            return ((pair_1[1], pair_2[0]))
        if word == pair_1[1]:
            if pair_2[0] == '0':
                return ((pair_2[0], pair_1[0]))
            return ((pair_1[0], pair_2[0]))
    return False


def add(old_pairs, new_pairs):
    old = old_pairs.copy()
    for new_pair in new_pairs:
        if new_pair not in old:
            old.append(new_pair)
    return old


while True:
    try:
        s = input('2-CNF > ')
    except EOFError:
        break
    parser.parse(s)
    old_pairs = []
    new_pairs = original.copy()
    while True:
        new = find_new(old_pairs, new_pairs)
        old = add(old_pairs, new_pairs)
        if new is False:
            print(old)
            print('no')
            break
        if new is []:
            print(old)
            print('yes')
            break
        old_new = add(old, new)
        if old == old_new:
            print(old)
            print('yes')
            break
        old_pairs = old
        new_pairs = new
    original = []
