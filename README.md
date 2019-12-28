Please use the following command to test this program:
dune runtest

If you want to build and run the program without the tests
then please comment out the tests and use the follwing commands:
ocamlc bc.ml
./a.out

Printing strings is done by doing 

Print("this is the string")

or 

Printf(number)         ex. Printf(2.36)

For tests, pass a list of statements to runCode like 
runCode [this ; is; a ; list]

For functions, when a new layer is added to the scope, all variables declared will be in that scope. 
Not just the parameters (a stupid bc thing). 

Return, Break and Continue are all statements. Return has an expressions parameter. 