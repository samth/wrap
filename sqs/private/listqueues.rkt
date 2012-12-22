#lang typed/racket/base

(provide 
 list-queues
 ListQueuesResp ListQueuesResp?
 ListQueuesResp-req-id
 ListQueuesResp-queues
 ListQueuesResp-result)

(require
 (only-in "../../../format/xml/sxml.rkt"
          Sxml SXPath 
          sxpath xml->sxml select-single-node-text)
 (only-in "../../../httpclient/http/http11.rkt"
          StatusLine)
 (only-in "../../../httpclient/http/header.rkt"
          Headers make-header)
 (only-in "invoke.rkt"
          SQSError sqs-invoke))

(struct: ListQueuesResp ([req-id : String]
                         [queues : (Listof String)]
                         [result : StatusLine]) #:transparent)

(: sqs-list-request ((Option String) -> Headers))
(define (sqs-list-request name-prefix)
  (if name-prefix
      (list (make-header "QueueNamePrefix" name-prefix))
      '()))

(: list-queues ((Option String) -> (U SQSError Void)))
(define (list-queues prefix)
  (sqs-invoke "/" 'ListQueues (sqs-list-request prefix)))


