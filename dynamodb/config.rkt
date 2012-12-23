#lang typed/racket/base

(provide
 swf-host ;swf-version
 ddb-host ddb-version)

(: ddb-version String)
(define ddb-version "DynamoDB_20111205")

(: ddb-host String)
(define ddb-host "dynamodb.us-east-1.amazonaws.com")

(: swf-host String)
(define swf-host "swf.us-east-1.amazonaws.com")