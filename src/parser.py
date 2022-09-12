from tokens import *
from nodes import *
from ops import Priority, get_precedence

class Parser:
    def __init__(self, lex):
        self.lex = lex # iterator. 
        self.curr_token = None 
        self.next_token = None 
        self.update() # initialize self.next_token
        self.update() # initialize self.curr_token

    def update(self):
        self.curr_token = self.next_token
        self.next_token = next(self.lex)
        print("update", self.curr_token, self.next_token)

    def __iter__(self):
        return self 

    def __next__(self) -> Statement:
        """parser's main function. returns AST
        """
        while self.curr_token.name != EOF:
            if statement := self.parse_statement():
                return statement
        else:
            return EOF       
    
    def parse_statement(self):
        expression = self.parse_expression(Priority.LOWEST)
        if self.next_token == Token.SEMICOLON:
            self.update()
            self.update()
        return expression

    def parse_expression(self, precedence: Priority) -> Expression:
        """Evaluate all tokens whose priority >= *precedence* """
        expression = self.parse_datatypes() or \
                     self.parse_identifier() or \
                     self.parse_unary() or \
                     self.parse_group()

        if expression == None:
            print("curr_token", self.curr_token)
            raise SyntaxError(f"operand '{self.curr_token.value}' not defined")

        while self.next_token != Token.SEMICOLON and \
            precedence.value <= get_precedence(self.next_token).value:
            if new_expression := self.parse_infix_expression(expression):
                expression = new_expression
            else:
                break
        return expression
        
    def parse_datatypes(self) -> Expression:
        """parse supported datatypes only"""
        datatypes =[
            Token.INT.name,
            Token.FLOAT.name, 
            Token.TRUE.name, 
            Token.FALSE.name, 
            Token.STRING.name
        ]
        if self.curr_token.name in datatypes:
            return Literal(self.curr_token.name, self.curr_token.value)

    def parse_unary(self):
        """parse supported unary operations only"""
        if self.curr_token.name in [Token.NOT, Token.MINUS]:
            operator = self.curr_token.value 
            self.update()
            right = self.parse_expression(Priority.HIGHER)
            return PrefixExpression(operator, right)

    def parse_infix_expression(self, left: Expression) -> Expression:
        infix_list = [
            #arithmetic
            Token.PLUS, Token.MINUS, Token.DIVIDE, 
            Token.TIMES, Token.MODULUS, Token.POWER,
            # comparison
            Token.EQUAL, Token.LARGE, Token.LARGEEQ, 
            Token.SMALL, Token.SMALLEQ, Token.NOTEQ,
            # logical
            Token.AND, Token.OR            
        ]
        if self.next_token.name in infix_list:
            self.update() # go to the next token. next -> curr
            operator = self.curr_token.value 
            precedence = get_precedence(self.curr_token)
            self.update()
            right = self.parse_expression(precedence)
            return InfixExpression(left, operator, right)

    def parse_group(self):
        if self.curr_token.name == Token.LPAREN:
            self.update()
            expression = self.parse_expression(Priority.LOWEST)
            self.is_next(Token.RPAREN)
            return expression 

    def is_next(self, t):
        if self.next_token != t:
            raise AssertionError(f"expected {t} but got {self.next_token.name}")
        self.update()

    def parse_identifier(self):
        if self.curr_token == Token.ID:
            return Identifier(self.curr_token.value)

    def parse_let_statement(self) -> Statement:
        if self.curr_token == Token.LET:
            self.is_next(Token.ID)
            variable = self.curr_token.value 
            self.is_next(Token.ASSIGN)
            self.update() 
            value = self.parse_expression(Priority.LOWEST)
            self.is_next(Token.SEMICOLON)
            self.update()
            return LetStatement(variable, value)

    def parse_assign_statement(self) -> Statement:
        if self.curr_token == Token.ID:
            variable = self.curr_token.value 
            self.is_next(Token.ASSIGN)
            self.update() 
            value = self.parse_expression(Priority.LOWEST)
            self.is_next(Token.SEMICOLON)
            self.update()
            return AssignStatement(variable, value)

    def print_statement(self) -> Statement:
        if self.curr_token == Token.PRINT:
            state = self.curr_token.value 
            self.is_next(Token.LPAREN)
            self.update()
            value = self.parse_expression(Priority.LOWEST)
            self.is_next(Token.RPAREN)
            self.is_next(Token.SEMICOLON)
            self.update()
            return PrintStatement(state, value)

    def parse_statement(self) -> Statement:
        if self.curr_token == ILLEGAL:
            raise SyntaxError(f"invalid input: {self.curr_token.value}")
        return self.parse_let_statement() or \
                self.parse_assign_statement() or \
                self.print_statement() or \
                self.parse_expression_statement()

    def parse_expression_statement(self):
        expression = self.parse_expression(Priority.LOWEST)
        if self.next_token == Token.SEMICOLON:
            self.update()
            self.update()
        return expression