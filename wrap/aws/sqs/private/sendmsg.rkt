#lang typed/racket/base

(provide
 send-message)

(require
 (only-in gut/uri/url/url
	  QParam QParams)
 (only-in gut/http/header
	  Headers make-header)
 (only-in gut/http/encode
	  url-encode-string)
 (only-in "invoke.rkt"
	  SQSError sqs-invoke))

(: send-message-request (String -> QParams))
(define (send-message-request msg)
  (list (QParam "MessageBody" (url-encode-string msg #t))))

(: send-message (String String -> (U SQSError Void)))
(define (send-message queue-path msg)
  (sqs-invoke queue-path 'SendMessage (send-message-request msg)))
