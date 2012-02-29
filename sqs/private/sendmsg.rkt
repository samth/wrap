#lang typed/racket/base

(provide
 send-message)

(require 
 (only-in (planet knozama/webkit:1/web/http/header)
          Headers make-header)
 (only-in (planet knozama/webkit:1/web/uri)
	  url-encode-string)
 (only-in "invoke.rkt"
	  SQSError sqs-invoke))

(: send-message-request (String -> Headers))
(define (send-message-request msg)
  (list (make-header "MessageBody" (url-encode-string msg #t))))

(: send-message (String String -> (U SQSError Void)))
(define (send-message queue-path msg)
  (sqs-invoke queue-path 'SendMessage (send-message-request msg)))
