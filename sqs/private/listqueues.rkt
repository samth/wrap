#lang typed/racket/base

(provide 
 list-queues
 ListQueuesResp ListQueuesResp?
 ListQueuesResp-req-id
 ListQueuesResp-queues
 ListQueuesResp-result)

(require
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath 
	  sxpath xml->sxml select-single-node-text)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  Result)
 (only-in (planet knozama/webkit:1/web/http/header)
          Headers make-header)
 (only-in "invoke.rkt"
	  SQSError sqs-invoke))

(struct: ListQueuesResp ([req-id : String]
			 [queues : (Listof String)]
			 [result : Result]) #:transparent)

(: sqs-list-request ((Option String) -> Headers))
(define (sqs-list-request name-prefix)
  (if name-prefix
      (list (make-header "QueueNamePrefix" name-prefix))
      '()))

(: list-queues ((Option String) -> (U SQSError Void)))
(define (list-queues prefix)
  (sqs-invoke "/" 'ListQueues (sqs-list-request prefix)))
    
  
