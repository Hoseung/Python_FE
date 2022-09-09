from tokens import *

def Lexer(data: str) -> TokenInfo:
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