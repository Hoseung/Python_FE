from lexer import Lexer 
from tokens import *

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
        read_file(file_name)