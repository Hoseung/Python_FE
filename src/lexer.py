from xml.dom import SyntaxErr
from tokens import *
from ops import switcher, double_switcher

def Lexer_r(data: str) -> TokenInfo:
    """Lexer for Regex tokens
    """
    pos = 0
    while pos < len(data):
        for tokenId in Token:
            # perform regex match
            # :=  assignment within an expression
            if match := tokenId.value.match(data, pos):
                pos = match.end(0)
                if tokenId == Token.WHITESPACE or tokenId == Token.COMMENT:
                    # ignore whitespaces and comments
                    break
                yield TokenInfo(tokenId.name, match.group(0))
                break
        else:
            # in cas pattern doesn't match, send the character as illegal
            yield TokenInfo(ILLEGAL, data[pos])
            pos +=1
    else:
        # in parser we read the token two times each iteration
        # once for current token and once for next token
        # so handling it by sending EOF twice.
        yield TokenInfo(EOF, '\x00')
        yield TokenInfo(EOF, '\x00')


class Lexer(object):
    def __init__(self, input_data:str) -> None:
        """Regex lexer와는 꽤 다른 요소들이 있음. 
           pos, ch등 
        """
        self.input: str = input_data
        self.pos: int   = 0 # next position to read
        self.ch:str     = "" # current character
        self.read_char()

    def read_char(self) -> None:
        # read next character
        # update the position to next value 
        if self.pos >= len(self.input):
            self.ch = "\x00"
        else:
            self.ch = self.input[self.pos]
        self.pos += 1

    def __iter__(self):
        # making the class iterator
        return self

    def __next__(self) -> TokenInfo:
       # making the class generator
       # checking for end of file
       # remove all whitespaces -- no mendatory indentation
       # remove all comments
       # check for opeartors
       # check for string
       # check for integers
       # if nothing matches, its illegal value
       # update position
       # return token
       return self.lex_eol() \
            or self.remove_whitespace() \
            or self.remove_comments()  \
            or self.lex_operator()  \
            or self.lex_string()  \
            or self.lex_letters() \
            or self.lex_integer() \
            or self.lex_illegal()

    def lex_eol(self) -> TokenInfo:
        if self.ch == '\x00':
            return TokenInfo(EOF, self.ch)
    
    def remove_whitespace(self) -> None:
        while is_whitespace(self.ch):
            self.read_char()

    def remove_comments(self) -> None:
        if self.ch == '#"':
            self.read_char()
            while self.ch not in ["\n", "\r", "\x00"]:
                self.read_char()

    def lex_operator(self) -> TokenInfo:
        """If encounter an operator, search for = in the statement"""
        if tok := switcher.get(self.ch, None):
            # check for single character ops
            ch = self.ch 
            self.read_char()
            
            # check for double character ops like 
            # '>=', '<=', '!='
            if new_tok := double_switcher.get(ch+self.ch, None):
                tok = new_tok
                ch += self.ch
                self.read_char()
            return TokenInfo(tok.name, ch)

    def lex_string(self) -> TokenInfo:
        if self.ch == '\"':
            pos_start = self.pos 
            self.read_char()
            while self.ch != '\"':
                if self.ch in ["\n", "\r", "\x00"]:
                    raise SyntaxErr("string literal not closed")
                self.read_char()
            data: str = self.input[pos_start: self.pos]
            self.read_char()
            return TokenInfo(Token.STRING.name, data)
    
    def lex_letters(self) -> TokenInfo:
        """process letters in a string"""
        if is_letter(self.ch):
            pos_start = self.pos -1 # b.c .pos is the 'next' position
            while is_letter(self.ch):
                self.read_char()
            data:str = self.input[pos_start:self.pos-1]
            # check if the string is a keyword
            ttype = get_token(data)
            return TokenInfo(ttype, data)

    def lex_integer(self) -> TokenInfo:
        # check for float and integer
        if is_digit(self.ch):
            pos_start = self.pos -1
            ttype = Token.INT.name
            while is_digit(self.ch):
                self.read_char()
                if self.ch == ".":
                    ttype = Token.FLOAT.name
                    continue
            data: str = self.input[pos_start: self.pos -1]
            return TokenInfo(ttype, data)

    def lex_illegal(self):
        tok = TokenInfo(ILLEGAL, self.ch)
        self.read_char()
        return tok 

    
def is_letter(char: str) -> bool:
    """Checking if given self.ch is letter"""
    return ("a" <= char <="z") | ("A" <= char <= "Z") \
            | ('_' == char)

def is_whitespace(char: str) -> bool:
    """checking if the given input is whitespace"""
    return (char == " ") | (char == "\t") \
          |(char == "\n") | (char == "\r")

def is_digit(char: str) -> bool:
    """checking if the given input is numeric"""
    return ("0" <= char <= "9") | (char == ".")

