from lexer import Lexer 
from tokens import *
from parser import Parser

def repl():
    i = 0
    while True:
        try: 
            data:str = input(f"[{i}]:")
            for tok in Lexer(data):
                if tok.name == EOF:
                    break
                print(tok)
        except EOFError:
            print()
        except KeyboardInterrupt:
            print("bye")
            break
        finally:
            i+=1

def read_file(file_name:str) -> None:
    with open(file_name, "r") as f:
        data = f.read()
        for tok in Lexer(data):
            if tok.name == EOF:
                break
            print(tok)

def interpreter(data: str):
    for p in Parser(Lexer(data)):
        print("P", p)
        if p == EOF:
            break
        p.eval()

if __name__ == "__main__":
    import sys
    from os.path import exists

    if len(sys.argv) < 2:
        print("file not given.")
        repl()
    else:
        file_name = sys.argv[1]
        if not exists(file_name):
            print(f"{file_name} not found")
            sys.exit(0)
        with open(file_name) as f:
            data = f.read()
        interpreter(data) 
        # Assume one expression per a line 
        # or do we allow line continuation?