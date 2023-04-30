
from tokenize import tokenize
from io import BytesIO
import tkinter as tk
from tkinter import filedialog
from pyswip import Prolog
import sys
import os

keywords = ["main", "const", "var", "int", "bool",
            "str", "if", "else", "for", "while", "print", "true", "false", "not", "and", "or", "elseif"]
operators = ["+", "-", "*", "/", "=", ">", "<", "!", "?", ":"]
arithmetic_assignment = ["+=", "-=", "/=", "*=", "=="]
separators = ["(", ")", "{", "}", ","]


def lexical_analyzer(filename):
    assert filename[-5:] == "pixel", "Please select a file with .pixel extension"
    tokens = ""
    readFile = open(filename, 'r').read()
    line = BytesIO(readFile.encode('utf-8')).readline
    tokened_line = tokenize(line)
    values = []
    for number, value, v1, v2, v3 in tokened_line:
        if len(value) == 0:
            continue
        else:
            if value == "\n" or value == " " or value == "\t":
                continue
            else:
                if value == "[":
                    values.append(value)
                elif value == "]":
                    values.append(value)
                    for val in values:
                        tokens += val
                        tokens += ", "
                    values = []
                else:
                    if value in keywords or value in operators or value == ";":
                        if value == "elseif":
                            tokens += "else, if, "
                            continue
                        tokens += value
                        tokens += ", "
                    elif value in separators:
                        tokens += "'"
                        tokens += value
                        tokens += "', "
                    elif value.startswith("'"):
                        temp = value[1:-1]
                        tokens += ("'" + temp + "', ")
                    elif value.startswith('"'):
                        temp = value[1:-1]
                        tokens += ('"' + temp + '", ')
                    elif value.isdigit() or value.isalpha():
                        tokens += value
                        tokens += ", "
                    elif value in arithmetic_assignment:
                        tokens += value[0]
                        tokens += ", "
                        tokens += value[1]
                        tokens += ", "
    tokens = tokens[:-2]
    tokens = tokens.split(", ")
    tokensFile = filename[:-5] + "pixeltok"
    with open(tokensFile, "w") as file:
        for token in tokens:
            token = token.replace("'", '')
            file.write('{}\n'.format(token))
        print("Writing Tokens in " + tokensFile)
    os.system("swipl -g \"main('" + tokensFile + "')\" ../runtime/main.pl")


if __name__ == "__main__":
    root = tk.Tk()
    root.withdraw()
    file_path = filedialog.askopenfilename()
    lexical_analyzer(file_path)
