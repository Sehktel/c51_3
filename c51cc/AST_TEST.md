 $env:TEST_FILE = ".\test\c51code\abc.c"
 lein repl
(require '[c51cc.ast :as ast])

(ast/set-debug-tokens! true)  ;; включить отображение токенов
(ast/set-debug-tokens! false) ;; выключить отображение токенов

(ast/view-ast-from-file)
(ast/view-ast-from-file "./test/c51code/test_24_csumab.c")
