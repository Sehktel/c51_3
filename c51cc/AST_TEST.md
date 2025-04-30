 $env:TEST_FILE = ".\test\c51code\abc.c"
 lein repl
(require '[c51cc.ast :as ast])
(ast/view-ast-from-file)
