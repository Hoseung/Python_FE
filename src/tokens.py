from collections import namedtuple
from enum import Enum
import re 

TokenInfo = namedtuple("Tokens", ["name", "value"])


###################################
# Special Tokens for error handling
###################################
ILLEGAL = 'ILLEGAL'
EOF     = "EOF"

class NewEnum(Enum):
    def __eq__j(self, b) -> bool:
        """ when == between the Enum,
        check the name, not the value"""
        if isinstance(b, str):
            return self.name == b
        else:
            return self.name == b.name

    def __hash__(self):
        return id(self.name)

class Token(NewEnum):
    # data types
    STRING  = re.compile(r'(\".*\")|(\'.*\')')
    FLOAT   = re.compile(r'\d+\.\d+')
    INT     = re.compile(r'\d+')
    # brackets
    LPAREN  = re.compile(r'\(')
    RPAREN  = re.compile(r'\)')
    LBRACE  = re.compile(r'\{')
    RBRACE  = re.compile(r'\}')
    LSQUARE = re.compile(r'\[')
    RSQUARE = re.compile(r'\]')
    # oeprators
    ASSIGN  = re.compile(r'\=')
    # athemetic operators
    PLUS    = re.compile(r'\+')
    MINUS   = re.compile(r'\-')
    TIMES   = re.compile(r'\*')
    DIVIDE  = re.compile(r'/')
    MODULUS = re.compile(r'%')
    POWER   = re.compile(r'\^')
    # logical operators
    AND     = re.compile(r'&')
    OR      = re.compile(r'\|')
    NOT     = re.compile(r'!')
    # conditional operators
    EQUAL   = re.compile(r'\=\=')
    SMALL   = re.compile(r'<')
    SMALLEQ = re.compile(r'<\=')
    LARGE   = re.compile(r'>')
    LARGEEQ = re.compile(r'>\=')
    NOTEQ   = re.compile(r'!\=')
    # keywords
    IF      = re.compile(r'if')
    WHILE   = re.compile(r'while')
    TRUE    = re.compile(r'true')
    FALSE   = re.compile(r'false')
    FUNC    = re.compile(r'func')
    ELSE    = re.compile(r'else')
    NAN     = re.compile(r'nan')
    LET     = re.compile(r'let')
    PRINT   = re.compile(r'(println)|(print)')
    RETURN  = re.compile(r'return')
    # variables
    ID      = re.compile(r'[_a-zA-Z][_a-zA-Z0-9]*')
    # comments
    COMMENT = re.compile(r'#.*')
    # delimier 
    COMMA   = re.compile(r',')
    SEMICOLON  = re.compile(r';')
    WHITESPACE = re.compile(r'(\t|\n|\s|\r)+')