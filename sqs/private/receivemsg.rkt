#lang typed/racket/base

(provide
 receive-message)

(require
 (only-in net/uri/url/url
	  QParam QParams add-qparam)
 (only-in net/http/header
	  Headers make-header)
 (only-in net/http/encode
	  url-encode-string)
 (only-in "invoke.rkt"
	  SQSError sqs-invoke))

(define-type AttributeName (U 'All 'SenderId 'SentTimestamp 'ApproximateReceiveCount 'ApproximateFirstReceiveTimestamp))

(: make-attribute-headers ((Listof AttributeName) -> QParams))
(define (make-attribute-headers attrs)
  (let ((key-base "AttributeName.")
	(len (length attrs)))
    (let: loop : QParams ((attrs : (Listof AttributeName) attrs)
			  (idx : Integer 1) (headers : QParams '()))
	  (if (null? attrs)
	      headers
	      (loop (cdr attrs)
		    (add1 idx)
		    (add-qparam (QParam (string-append key-base (number->string idx))
					(symbol->string (car attrs))) headers))))))

(: receive-message-request ((Listof AttributeName)
			    Exact-Nonnegative-Integer Exact-Nonnegative-Integer
			    -> QParams))
(define (receive-message-request attributes visibility-timeout max-msgs)
  (let ((max (QParam "MaxNumberOfMessage" (number->string max-msgs)))
	(visto (QParam "VisibilityTimeout" (number->string visibility-timeout))))
    (add-qparam max (add-qparam visto (make-attribute-headers attributes)))))

(: receive-message (String (Listof AttributeName)
			   Exact-Nonnegative-Integer Exact-Nonnegative-Integer
			   -> (U SQSError Void)))
(define (receive-message queue-path attributes visibility-timeout max-msgs)
  (sqs-invoke queue-path 'ReceiveMessage (receive-message-request attributes visibility-timeout max-msgs)))
