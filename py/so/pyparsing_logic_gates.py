import json
import pyparsing as pp


def parse_id(s, locs, tokens):
    (opand,) = tokens
    return int(opand)


def parse_call(s, locs, tokens):
    type_, _dash, id_, args = tokens
    return {"type": type_, "id": id_, "i": list(args)}


def parse_args(s, locs, tokens):
    return tokens[::2]


def parse_root(s, locs, tokens):
    (root,) = tokens
    return root


def make_parser():
    LPAR, RPAR = map(pp.Suppress, "()")
    LBRA, RBRA = map(pp.Suppress, "[]")
    type_ = pp.Word(pp.alphas)
    id_ = pp.Word(pp.nums)
    func = type_ + "-" + id_
    call = pp.Forward()
    args = pp.Optional(call + pp.ZeroOrMore("," + call))
    call <<= func + pp.Group(LPAR + args + RPAR)
    many = args
    root = pp.Group(LBRA + many + RBRA)

    id_.setParseAction(parse_id)
    args.setParseAction(parse_args)
    call.setParseAction(parse_call)
    root.setParseAction(parse_root)

    return root


def main():
    test = """[
        output-0(
            or-0(
                and-0(
                    not-0(
                        input-0()),
                    not-1(
                        input-1())),
                and-2(
                    input-0(),
                    not-1(
                        input-1())))),
        output-1(
            or-1(
                and-1(
                    not-0(
                        input-0()),
                    input-1()),
                and-2(
                    input-0(),
                    not-1(
                        input-1()))))
    ]"""

    parser = make_parser()
    result = parser.parseString(test)[:]
    print(json.dumps(result, indent=4))


main()
