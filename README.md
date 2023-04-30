# [![N|Solid](https://www.creativefabrica.com/wp-content/uploads/2022/05/21/Pixel-Logo-Graphics-30905280-1-1-580x387.jpg)](https://www.creativefabrica.com/wp-content/uploads/2022/05/21/Pixel-Logo-Graphics-30905280-1-1-580x387.jpg) 
# Programming Language
# SER502-Spring2023-Team16
### Pixel is a  programming language developed to compute trivial arithmetic operations, conditions, ternerary expressions and loops.

#### Team Members:
1. Swapnil Mukeshbhai Chadotra
2. Sudheer Reddy Kunduru
3. Jay Yogeshkumar Patel
4. Manan Hashmukbhai Patel
5. Jashmin Mineshkumar Patel
## ⚙ Tools Used
- SWI-Prolog
- Python3
- Bash

## Project Video Link
- Youtube Video Link : 

## Installation and Execution Steps
- SWI-Prolog Download ([Link 🚀](https://www.swi-prolog.org/Download.html))
- Python3 Download ([Link 🚀](https://www.python.org/downloads/))
- Sample test programs are saved in the 'data' folder with .pixel extension
### Installing Prolog
#### Arch Linux
```
$ apt-get install swi-prolog
```
#### MacOS (Requires brew)
```
brew install swi-prolog
```
### Execution Steps
#### Type 1
Go to the Compiler Folder
```
bash run.sh
```
#### Type 2
```
$ swipl
```
- Enter the path to the .pl file
```
?- ['/Users/user/Desktop/SER502-Spring2023-Team16/src/compiler/run.pl'].  
```
- Run the sample program by parsing token
```
?- pixel('/Users/user/Desktop/SER502-Spring2023-Team16/src/compiler/lexical_analyer_1.py').
```
#### Type 3
Go to the Compiler Folder
```
python lexical_analyzer_2.py
```