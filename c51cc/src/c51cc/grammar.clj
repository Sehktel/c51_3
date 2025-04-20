(ns c51cc.grammar
  "Грамматика для языка C51"
  (:require [c51cc.lexer :as lexer]))

(def c51-grammar
  "Расширенная PEG-грамматика для языка C51 с максимальной гибкостью"
  "program = <whitespace*> (declaration <whitespace*>)* <whitespace*>
   
   declaration = function-declaration / variable-declaration
   
   function-declaration = type-keyword <whitespace+> identifier <whitespace*> 
                          <'('> <whitespace*> parameters? <whitespace*> <')'> 
                          <whitespace*> function-body
   
   function-body = <'{'> <whitespace*> statement* <whitespace*> <'}'>
   
   parameters = parameter (<whitespace*> ',' <whitespace*> parameter)*
   parameter = type-keyword <whitespace+> identifier
   
   variable-declaration = type-keyword <whitespace+> variable-list <';'>
   
   variable-list = variable-init (<whitespace*> ',' <whitespace*> variable-init)*
   variable-init = identifier (<whitespace*> '=' <whitespace*> expression)?
   
   statement = variable-declaration 
             / assignment-statement
             / function-call-statement
             / control-flow 
             / return-statement
             / <whitespace*>
   
   assignment-statement = identifier <whitespace*> '=' <whitespace*> expression <';'>
   function-call-statement = function-call <';'>
   
   return-statement = 'return' <whitespace+> expression <whitespace*> ';'
   
   function-call = identifier <whitespace*> <'('> <whitespace*> arguments? <whitespace*> <')'>
   arguments = expression (<whitespace*> ',' <whitespace*> expression)*
   
   control-flow = ('if' / 'while') <whitespace*> <'('> <whitespace*> expression <whitespace*> <')'> 
                 <whitespace*> <'{'> <whitespace*> statement* <whitespace*> <'}'>
   
   expression = arithmetic-expression 
              / parenthesized-expression
              / identifier 
              / number
   
   parenthesized-expression = <'('> <whitespace*> expression <whitespace*> <')'>
   
   arithmetic-expression = expression <whitespace*> operator <whitespace*> expression
   
   operator = '+' / '-' / '*' / '/' / '==' / '!=' / '<' / '>' / '='
   
   type-keyword = 'int' / 'void' / 'char' / 'void*' / 'unsigned int'
   identifier = #'[a-zA-Z_][a-zA-Z0-9_]*'
   number = #'[0-9]+'
   
   whitespace = #'\\s+'") 