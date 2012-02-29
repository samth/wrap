#lang typed/racket/base

(provide 
 sts-host sts-api-version sts-ns
 get-session-token-action)

(define sts-host "sts.amazonaws.com")

(define sts-api-version "2011-06-15")

(define sts-ns "https://sts.amazonaws.com/doc/2011-06-15/")

(define get-session-token-action "GetSessionToken")
