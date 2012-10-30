#lang typed/racket/base

(provide
 receive-message)

(require 
 (only-in "../../../httpclient/http/header.rkt"
          Headers make-header)
 (only-in "../../../httpclient/uri/url/encode.rkt"
          url-encode-string)
 (only-in "invoke.rkt"
          SQSError sqs-invoke))

(define-type AttributeName (U 'All 'SenderId 'SentTimestamp 'ApproximateReceiveCount 'ApproximateFirstReceiveTimestamp))

(: make-attribute-headers ((Listof AttributeName) -> Headers))
(define (make-attribute-headers attrs)
  (let ((key-base "AttributeName.")
	(len (length attrs)))
    (let: loop : Headers ((attrs : (Listof AttributeName) attrs) 
			  (idx : Integer 1) (headers : Headers '()))
      (if (null? attrs)
	  headers
	  (loop (cdr attrs) 
		(add1 idx) 
		(cons (cons (string-append key-base (number->string idx))			    
			    (symbol->string (car attrs))) headers))))))

(: receive-message-request ((Listof AttributeName) 
			   Exact-Nonnegative-Integer Exact-Nonnegative-Integer -> Headers))
(define (receive-message-request attributes visibility-timeout max-msgs)
  (let ((max (make-header "MaxNumberOfMessage" (number->string max-msgs)))
	(visto (make-header "VisibilityTimeout" (number->string visibility-timeout))))
    (cons max (cons visto (make-attribute-headers attributes)))))
  
(: receive-message (String (Listof AttributeName) 
			   Exact-Nonnegative-Integer Exact-Nonnegative-Integer 
			   -> (U SQSError Void)))
(define (receive-message queue-path attributes visibility-timeout max-msgs)
  (sqs-invoke queue-path 'ReceiveMessage (receive-message-request attributes visibility-timeout max-msgs)))
