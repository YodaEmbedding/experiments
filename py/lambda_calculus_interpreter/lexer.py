from enum import Enum

Token = Enum('Token', ['LPAREN', 'RPAREN', 'DOT', 'LAMBDA', 'ID', 'EOF'])

def tokenize(input_str):
    tokens = []
    buffer = []

    def flush_buffer(buffer, tokens):
        if len(buffer) == 0:
            return
        identifier = ''.join(buffer)
        tokens.append((Token.ID, identifier))
        del buffer[:]

    for i, c in enumerate(input_str):
        if 'a' <= c <= 'z' or 'A' <= c <= 'Z':
            buffer.append(c)
            continue

        flush_buffer(buffer, tokens)

        if   c == ' ':
            pass
        elif c == '(':
            tokens.append((Token.LPAREN,))
        elif c == ')':
            tokens.append((Token.RPAREN,))
        elif c == '.':
            tokens.append((Token.DOT,))
        elif c == 'λ' or c == '\\':
            tokens.append((Token.LAMBDA,))
        else:
            raise ValueError('Invalid character')

    flush_buffer(buffer, tokens)
    tokens.append((Token.EOF,))
    return tokens

def __main():
    from pprint import pprint
    input_str = "(λx.x x)(λy.y)"
    tokens = tokenize(input_str)
    print(input_str)
    pprint(tokens)

if __name__ == "__main__":
    __main()
