#lang typed/racket/base

(provide
 send-message)

(require
 (only-in net/uri/url/url
	  QParam QParams)
 (only-in net/http/header
	  Headers make-header)
 (only-in net/http/encode
	  url-encode-string)
 (only-in "invoke.rkt"
	  SQSError sqs-invoke))

(: send-message-request (String -> QParams))
(define (send-message-request msg)
  (list (QParam "MessageBody" (url-encode-string msg #t))))

(: send-message (String String -> (U SQSError Void)))
(define (send-message queue-path msg)
  (sqs-invoke queue-path 'SendMessage (send-message-request msg)))
