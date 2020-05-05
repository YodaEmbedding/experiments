import pyparsing as pp

def parse_opand(s, locs, tokens):
    opand, = tokens
    return float(opand)

def parse_term(s, locs, tokens):
    print("term", s, locs, tokens)
    if len(tokens) == 1: return tokens[0]
    total = tokens[0]
    for op, opand in zip(tokens[1::2], tokens[2::2]):
        if not isinstance(opand, (int, float)): opand, = opand
        if   op == "*": total *= opand
        elif op == "/": total /= opand
        else: raise ValueError
    return total

def parse_expr(s, locs, tokens):
    print("expr", s, locs, tokens)
    if len(tokens) == 1: return tokens[0]
    total = tokens[0]
    for op, opand in zip(tokens[1::2], tokens[2::2]):
        if not isinstance(opand, (int, float)): opand, = opand
        if   op == "+": total += opand
        elif op == "-": total -= opand
        else: raise ValueError
    return total

def main():
    LPAR, RPAR = map(pp.Suppress, "()")
    expr = pp.Forward()
    opand = pp.Word(pp.nums)
    opand.setParseAction(parse_opand)
    factor = opand | pp.Group(LPAR + expr + RPAR)
    term = factor + pp.ZeroOrMore(pp.oneOf('* /') + factor)
    term.setParseAction(parse_term)
    expr <<= term + pp.ZeroOrMore(pp.oneOf("+ -") + term)
    expr.setParseAction(parse_expr)

    test = "1 + 3 * (1 + 9 / 2 - 4 / 4 * (1 + 2 / 2 - 1)) * 2"
    expected = 1 + 3 * (1 + 9 / 2 - 4 / 4 * (1 + 2 / 2 - 1)) * 2
    actual = expr.parseString(test)[0]

    print(f"\n>>> {test}\n{actual}\n{expected} (expected)")

main()
