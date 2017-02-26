#!/usr/bin/python3

# GRAMMAR
# S -> A A
# A -> B | C
# B -> b
# C -> c

def source():
    return ""
    # return "bb"
    # return "bc"
    # return "cb"
    # return "cc"

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
    ast1, tokens1 = _a(tokens0)
    ast2, tokens2 = _a(tokens1)
    if [] != tokens2:
        raise ParserException(tokens2[0].line, tokens2[0].column, "")
    return ast1 + ast2

def _a(tokens):
    if tokens[0].type == 'B':
        return (_b(tokens), tokens[1:])
    elif tokens[0].type == 'C':
        return (_c(tokens), tokens[1:])
    else:
        raise ParserException(tokens[0].line, tokens[0].column)

def _b(tokens):
    if [] != tokens[1:]:
        raise ParserException(tokens[0].line, tokens[0].column, "")
    return ['B']

def _c(tokens):
    if [] != tokens[1:]:
        raise ParserException(tokens[0].line, tokens[0].column, "")
    return ['C']

#------------------------------------------------------------------------------#

def main():
    print(parse(tokenize(source())))

def test():
    try:
        ['B', 'B'] == parse(tokenize("bb"))
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
    test()
