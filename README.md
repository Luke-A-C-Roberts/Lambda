# Lambda interpreter written in haskell

To run make sure you have the Glasgow Haskell Compiler `ghc` is installed. Once installed, navigate to the cloned folder and execute the build script with `./build`. Then run with `./lambda`.

## Features
This lambda interpreter supports:
  - UTF-8 characters (both `λ` and `ł` work as the lambda sigil)
  - nested (curried) lambda functions, eg. `λx.λy.xy`, which do not require braces on input.
  - free variables in lambda function, eg. `λx.xy`
  - beta reduction of single variables, eg. `(λx.x)y → y`
  - beta reduction of multiple variables, eg. `(λx.x)(ab) → ab`
  - beta reduction of other lambdas, eg. `(λx.x)(λy.y) → (λy.y)`
  - alpha conversion of single nested bound variables, eg. `(λf.(λx.f(fx)))x` → `(λf.(λa.f(fa)))x`
  - alpha conversion of multiple nested bound variables, eg. `(λa.(λb.(λc.abc)))(bc) → (λa.(λd.(λe.ade)))(bc)`

## Demo
![demo](https://github.com/Luke-A-C-Roberts/Lambda/blob/master/demo.png?raw=true)

## References
for comparison: https://lambster.dev/

details on specific reductions of lambda calculus: https://ligerlearn.com/what-is-the-lambda-%ce%bb-calculus/

alpha conversion Alpha Conversion and Alpha Equivalence: https://home.ttic.edu/~pl/classes/CMSC336-Winter08/lectures/lec3.pdf
