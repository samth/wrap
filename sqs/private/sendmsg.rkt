#lang typed/racket/base

(provide
 send-message)

(require 
 (only-in "../../../httpclient/http/header.rkt"
          Headers make-header)
 (only-in "../../../httpclient/uri/url/encode.rkt"
          url-encode-string)
 (only-in "invoke.rkt"
          SQSError sqs-invoke))

(: send-message-request (String -> Headers))
(define (send-message-request msg)
  (list (make-header "MessageBody" (url-encode-string msg #t))))

(: send-message (String String -> (U SQSError Void)))
(define (send-message queue-path msg)
  (sqs-invoke queue-path 'SendMessage (send-message-request msg)))
