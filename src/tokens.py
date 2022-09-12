from collections import namedtuple
from enum import Enum

class NewEnum(Enum):
    def __eq__(self,b) -> bool:
        """E1 == E2 -> check the name of two enum"""
        if isinstance(b, str):
            return self.name == b
        else:
            return self.name == b.name 

    def __hash__(self):
        return id(self.name)

TokenInfo = namedtuple('TokenInfo', ["name", "value"])

EOF = "EOF"
ILLEGAL = "ILLEGAL"


#########################
# Tokens
#########################
Token = NewEnum("Token", [
        "ASSIGN",    # "="
    ##############
    # Arthemetic operators
    ##############
        "PLUS",      # "+"
        "MINUS",     # "-"
        "DIVIDE",    # "/"
        "TIMES",     # "*"
        "POWER",     # "^"
        "MODULUS",   # "%"
    ##############
    # Logical operators 
    #############
        "AND",       # "&"
        "OR",        # "|"
        #"XOR",       # "^"
        "NOT",       # "!"
    ##############
    # Comparisions
    ##############
        "EQUAL",     # "=="
        "NOTEQ",     # "!="
        "SMALL",     # "<"
        "LARGE",     # ">"
        "SMALLEQ",   # "<="
        "LARGEEQ",   # ">="
    ##############
    # Brackets
    ##############
        "COMMA",     # ","
        "SEMICOLON", # ";"
        "LPAREN",    # "("
        "RPAREN",    # ")"
        "LBRACE",    # "{"
        "RBRACE",    # "}"
        "LSQUARE",   # "["
        "RSQUARE",   # "]"
    ##############
    # EXTRA
    ##############
        "ID",        #  Variables
    ##############
    # Datatypes
    ##############  
        "INT",       
        "FLOAT",     
        "STRING",    
        "TRUE",      
        "FALSE",     
    ##############
    # Keywords
    ##############
        "FUNC",  
        "LET",       
        "IF",        
        "ELSE",      
        "RETURN",    
        "PRINT",
    #############
    # Comments
    #############
        "COMMENT"
    ])

keywords = {
    "func": Token.FUNC.name,
    "let": Token.LET.name,
    "if": Token.IF.name,
    "else": Token.ELSE.name,
    "true": Token.TRUE.name,
    "false": Token.FALSE.name,
    "return": Token.RETURN.name,
    "print": Token.PRINT.name,
}

def get_token(data:str) -> Token:
    # checks if the given string is keyword or not
    # if its a keyword return the respective Token name
    # or by default return Token.Id treating it as variable 
    return keywords.get(data, Token.ID.name)   