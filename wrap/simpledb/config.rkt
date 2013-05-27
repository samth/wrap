#lang typed/racket/base

(provide
 sdb-api-version sdb-host sdb-ns)

(: sdb-host String)
(define sdb-host "sdb.amazonaws.com")

(: sdb-api-version String)
(define sdb-api-version "2009-04-15")

(: sdb-ns (Pairof Symbol String))
(define sdb-ns (cons 'sdb "http://sdb.amazonaws.com/doc/2009-04-15/"))
