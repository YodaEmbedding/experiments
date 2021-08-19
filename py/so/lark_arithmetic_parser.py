from lark import Lark

# NOTE This doesn't properly handle right-assosciative expressions like "4^5^6"
parser = Lark(
    """
    ?sum: product
        | sum "+" product       -> add
        | sum "-" product       -> sub

    ?product:
        | product "*" exponent  -> mul
        | product "/" exponent  -> div
        | exponent

    ?exponent:
        | item "^" exponent     -> exp
        | item

    ?item: NUMBER               -> number
        | "-" item              -> neg
        | "(" sum ")"

    %import common.NUMBER
    %import common.WS
    %ignore WS""",
    start="sum",
)

s = "4 * 2 ^ 3 + 4 ^ 1"
tree = parser.parse(s)
print(tree.pretty())
