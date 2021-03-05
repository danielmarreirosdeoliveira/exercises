(ns syntax-quoting)

; user=> '+
; +
; user=> 'clojure.core/+
; clojure.core/+
; user=> `+
; clojure.core/+

; user=> `(+ 1 ~(inc 1))
; (clojure.core/+ 1 2)

