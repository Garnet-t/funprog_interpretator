// внимание тут пару примеров
let code = """
letrec fact = fun n ->
  if n = 0 then 1 else n * fact (n - 1)
in fact 5
"""
#load "Garnet_i9.fsx"

open Garnet_i9
//let result = Garnet_i4.run "let x = 5 in x + 2" была 4 версия
let testCode = """
letrec fact = fun n ->
  if n = 0 then 1 else n * fact (n - 1)
in fact 5
"""

let result = run testCode

printfn "Result: %A" result
