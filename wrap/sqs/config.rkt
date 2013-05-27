#lang typed/racket/base

(provide 
 sqs-host sqs-auth-version sqs-api-version signature-method)

(define LIST-QUEUE-ACTION "ListQueues")

(: sqs-host String)
(define sqs-host "sqs.us-east-1.amazonaws.com")

(define sqs-auth-version "2")

(define sqs-api-version "2011-10-01")

(define signature-method "HmacSHA256")

(: sqs-ns String)
(define sqs-ns (string-append "http://queue.amazonaws.com/doc/" sqs-api-version "/"))
