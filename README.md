# Lambda interpreter written in haskell

To run make sure you have the Glasgow Haskell Compiler `ghc` is installed. Once installed, navigate to the cloned folder and execute the build script with `./build`. Then run with `./lambda`.

This lambda interpreter supports:
  - nested (curried) lambda functions, eg. `λx.λy.xy`
  - free variables in lambda function, eg. `λx.xy`
  - alpha conversion of single nested bound variables, eg. `(λf.(λx.f(fx)))x` → `(λf.(λa.f(fa)))x`
  - alpha conversion of multiple nested bound variables, eg. `(λa.(λb.(λc.abc)))(bc) → (λa.(λd.(λe.ade)))(bc)`
  - beta reduction of single variables, eg. `(λx.x)y → y`
  - beta reduction of multiple variables, eg. `(λx.x)(ab) → ab`
  - beta reduction of other lambdas, eg. `(λx.x)(λy.y) → (λy.y)`
