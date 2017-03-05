#!/usr/bin/python3

# GRAMMAR
# S -> A
# A -> B C
# B -> b
# C -> c | c C
# bc bcc bccc ...

def source():
    return ""

class LexerException(Exception):
    def __init__(self, line, column, msg="Token error."):
        self.line = line
        self.column = column
        self.msg = msg

class ParserException(Exception):
    def __init__(self, line, column, msg="Syntax error."):
        self.line = line
        self.column = column
        self.msg = msg

#------------------------------------------------------------------------------#

class Token:
    def __init__(self, char, line, column):
        self.lexeme = char
        self.line = line
        self.column = column
        if char == 'b':
            self.type = 'B'
        elif char == 'c':
            self.type = 'C'
        else:
            raise LexerException(line, column, "bad token: {}".format(char))

def tokenize(source):
    column = 0
    tokens = []
    for char in source:
        token = Token(char, 0, column)
        tokens.append(token)
        column += 1
    return tokens

#------------------------------------------------------------------------------#

def parse(tokens):
    return _s(tokens)

def _s(tokens0):
    print("_s | tokens: ", [t.lexeme for t in tokens0])

    ast1, tokens1 = _a(tokens0)
    if [] != tokens1:
        raise ParserException(tokens1[0].line, tokens1[0].column, "")
    return ast1

def _a(tokens0):
    print("_a | tokens: ", [t.lexeme for t in tokens0])

    if 'B' == tokens0[0].type:
        ast1, tokens1 = _b(tokens0)
    else:
        raise ParserException(tokens0[0].line, tokens0[0].column)

    if 'C' == tokens1[0].type:
        ast2, tokens2 = _c(tokens1)
    else:
        raise ParserException(tokens1[0].line, tokens1[0].column)

    return (ast1 + ast2, tokens2)

def _b(tokens0):
    print("_b | tokens: ", [t.lexeme for t in tokens0])

    return (['B'], tokens0[1:])

def _c(tokens0):
    print("_c | tokens: ", [t.lexeme for t in tokens0])

    if [] == tokens0[1:]:
        return (['C'], [])
    else:
        ast1, tokens1 = _c(tokens0[1:])
        return (['C'] + ast1, tokens1)

#------------------------------------------------------------------------------#

def main():
    print(parse(tokenize(source())))

def test():
    try:
        ['B', 'C'] == parse(tokenize("bc"))
        ['C', 'B'] == parse(tokenize("cb"))
        ['C', 'C'] == parse(tokenize("cc"))
    except Exception as e:
        print("Error: {}".format(e))
    try:
        ['A', 'D'] == parse(tokenize("ad"))
        raise Exception("Tokenized something that shouldn't have!")
    except LexerException as e:
        pass
    try:
        ['B', 'B', 'B'] == parse(tokenize("bbb"))
        raise Exception("Parsed something that shouldn't have!")
    except ParserException as e:
        pass
    print("tests pass!")

if __name__ == "__main__":
    main()
