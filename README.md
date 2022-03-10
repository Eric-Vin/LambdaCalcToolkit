# LambdaToolkit
A compiler from a simple programming language down to lambda calculus and an associated lambda calculus interpreter.

## Building and Testing
In the root directory, run `make` to automatically build the project and run all tests. For a more verbose output, you can run `make test`.

## Usage
To use the compiler, run the binary with the first parameter `compile` followed by the input and output paths.
To use the interpreter, run the binary with the first parameter `interpret` followed by the input path and as many space delimited arguments to be passed into the program as desired.

## Example 
Several sample programs are provided in the examples folder. To compute the GCD of two natural numbers, say 6 and 9, run the following commands:

`stack exec LambdaCalcToolkit-exe compile examples/gcd.while gcd.lc`

`stack exec LambdaCalcToolkit-exe interpret gcd.lc 6 9`
