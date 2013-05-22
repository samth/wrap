#lang typed/racket/base

(provide
 delete-message)

(require
 (only-in httpclient/header
	  Headers make-header)
 (only-in httpclient/encode
	  url-encode-string)
 (only-in "invoke.rkt"
	  SQSError sqs-invoke))

(: delete-message-request (String -> Headers))
(define (delete-message-request msg)
  '())
;;  (list (make-header "MessageBody" (url-encode-string msg #t))))

(: delete-message (String String -> (U SQSError Void)))
(define (delete-message queue-path msg)
  (void))
;;  (sqs-invoke queue-path 'SendMessage (send-message-request msg)))
