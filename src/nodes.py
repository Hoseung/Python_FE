# There're two types of nodes in AST
# 1. Statements
#    that doesn't produce a value: a = 345 
# 2. Expressions
#    that produces a value: 2+3 (=5)

# In this example, we take a top-down approach called 
# "Pratt Parser", opposed to context free grammars and BNF.
#
#
from ast import operator
from tokens import *
from tempfile import TemporaryFile

# Merely interfaces 
class Node: ...

class Statement(Node):...

class Expression(Node):...



class Literal(Expression):
    def __init__(self, typ:str, value:str):
        self.type = typ
        self.value = value 

    def __repr__(self) -> str:
        return str(self.value)

    def get_type(self) -> str:
        # returns data type
        if self.type == Token.FALSE or self.type == Token.TRUE:
            return "bool" 
        return self.type.lower()

    def type_casting(self):
        match self.type:
            case Token.INT:
                return int(self.value)
            case Token.FLOAT:
                return float(self.value)
            case Token.STRING:
                return str(self.value)#[1:-1] ???
            case Token.TRUE:
                return True
            case Token.FALSE:
                return False 

    def eval(self):
        try:
            out = self.type_casting()
        except ValueError:
            raise(f"Error converting {self.value} to {self.type}")
        return out


class PrefixExpression(Expression):
    """operand == RHS"""
    def __init__(self, opeartor: Statement, right: Expression):
        self.operator = opeartor
        self.right = right

    def get_type(self) -> str:
        # reutnrs data type
        return self.type.right 

    def __repr__(self) -> str:
        return f"({self.operator}{self.right})"
    
    def eval(self):
        operator = {
            "!" : lambda a: not a,
            "-" : lambda a: -a,
        }
        try: 
            return operator[self.operator](self.right.eval())
        except TypeError:
            raise TypeError(f"can't perform {self.operator} between {self.left_type()} and {self.left_type()}")

class InfixExpression(Expression):
    def __init__(self, left:Expression, operator:Statement, right:Expression):
        self.left = left 
        self.operator = operator
        self.right = right 

    def __repr__(self) -> str:
        return f"({self.left} {self.operator} {self.right})"

    def eval(self):
        operator = {
            "%" : lambda a,b: a%b,
            "^" : lambda a,b: a**b,
            "+" : lambda a,b: a+b,
            "-" : lambda a,b: a-b,
            "*" : lambda a,b: a*b,
            "/" : lambda a,b: a//b,
            "&" : lambda a,b: a and b,
            "|" : lambda a,b: a or b,
        }
        try:
            data = operator[self.operator](self.left.eval(), self.right.eval())
            return data
        except TypeError:
            raise TypeError(f"can't perform {self.operator} between {self.left_type()} and {self.right_type()}")


## No postfix expression such as a++. 