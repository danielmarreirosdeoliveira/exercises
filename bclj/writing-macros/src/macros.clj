(ns macros)

; user=> (infix (1 + 1))
; 2
; user=> (macroexpand '(infix (1 + 1)))
; (+ 1 1)
(defmacro infix
  [infixed]
  (list (second infixed) (first infixed) (last infixed)))

; with argument destructuring
(defmacro infix-2
  [[operand1 op operand2]]
  (list op operand1 operand2))

; user=> (my-print-0 (+ 1 1))
; (let [result 2] (println result) result)
(defmacro my-print-0
  [expression]
  (list 'let ['result expression]
        (list 'println 'result)
        'result))

; user=> (my-print (+ 1 1))
; 2
; 2
(defmacro my-print
  [expression]
  (list 'let ['result expression]
        (list 'println 'result)
        'result))

; user=> (quote (+ 1 3))
; (+ 1 3)
; user=> +
; #object[clojure.core$_PLUS_ 0x62ef27a8 "clojure.core$_PLUS_@62ef27a8"]
; user=> (quote +)
; +
; user=> abc
; CompilerException java.lang.RuntimeException: Unable to resolve symbol: abc in this context, compiling:(NO_SOURCE_PATH:0:0)

(defmacro when-2
  [test & body]
  (list 'if test (cons 'do body)))

; user=> (when-2 (< 1 3) (prn "!") (prn "!!"))
; "!"
; "!!"
; nil
; user=> (when-2 (< 1 1) (prn "!") (prn "!!"))
; nil
; user=> (macroexpand '(when-2 (< 1 3) (prn "!") (prn "!!")))
; (if (< 1 3) (do (prn "!") (prn "!!")))





