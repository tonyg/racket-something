#lang racket/base

(require something/base)

(def-operator \|\| 20 right or)
(def-operator && 30 right and)
(def-operator = 40 nonassoc =)
(def-operator < 40 nonassoc <)
(def-operator <= 40 nonassoc <=)
(def-operator > 40 nonassoc >)
(def-operator >= 40 nonassoc >=)
(def-operator + 60 left +)
(def-operator - 60 left -)
(def-operator * 70 left *)
(def-operator / 70 left /)
(def-operator ∘ 80 right compose)
;; (def-operator - 500 prefix -)
;; (def-operator + 500 prefix +)

(def-operator |`| 1100 prefix quasiquote)
(def-operator |,| 1100 prefix unquote)
(def-operator |,@| 1100 prefix unquote-splicing)
