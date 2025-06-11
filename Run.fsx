#load "Garnet_i9.fsx"
// time.fsx пример работы интерпретатора
open Garnet_i9
//let result = Garnet_i4.run "let x = 5 in x + 2" работал ок на 4 версии .
let testCode = """
letrec fact = fun n ->
  if n = 0 then 1 else n * fact (n - 1)
in fact 5
"""

let result = run testCode

printfn "Result: %A" result
