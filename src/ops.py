from tokens import *

switcher = {  
    "+": Token.PLUS,    "-": Token.MINUS,
    "/": Token.DIVIDE,  "*": Token.TIMES,
    "%": Token.MODULUS, "^": Token.POWER,   
    "=": Token.ASSIGN,  "!": Token.NOT,
    "|": Token.OR,      "&": Token.AND,     
    ",": Token.COMMA,   ";": Token.SEMICOLON,  
    "(": Token.LPAREN,  ")": Token.RPAREN,  
    "{": Token.LBRACE,  "}": Token.RBRACE,     
    "[": Token.LSQUARE, "]": Token.RSQUARE, 
    "<": Token.SMALL,   ">": Token.LARGE    
}

double_switcher = { ">=": Token.LARGEEQ,  "<=": Token.SMALLEQ,
                    "==": Token.EQUAL,    "!=": Token.NOTEQ   }


Priority = NewEnum("priority", [
    "LOWEST", 
    "LOWER", 
    "LOW", 
    "HIGH", 
    "HIGHER", 
    "HIGHEST"
])

precedence = {
    Token.EQUAL.name: Priority.LOWER,        # ==
    Token.NOTEQ.name : Priority.LOWER,       # !=
    Token.SMALL.name : Priority.LOW,         # <
    Token.LARGE.name : Priority.LOW,         # >
    Token.PLUS.name  : Priority.HIGH,        # +
    Token.MINUS.name : Priority.HIGH,        # -
    Token.TIMES.name : Priority.HIGHER,      # *
    Token.DIVIDE.name: Priority.HIGHER,      # /
    Token.LPAREN.name: Priority.HIGHEST,     # ()  
}
        
def get_precedence(token: Token) -> Priority:
    return precedence.get(token.name, Priority.LOWEST) # if no match, get "LOWEST" by default