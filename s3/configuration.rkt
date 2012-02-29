#lang typed/racket/base

(provide
 nss s3-namespace)

(: s3-namespace String)
(define s3-namespace "http://s3.amazonaws.com/doc/2006-03-01/")

(: nss (List (Pair Symbol String)))
(define nss `((s3 . ,s3-namespace)))
