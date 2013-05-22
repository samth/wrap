#lang typed/racket/base

(provide
 list-queues
 ListQueuesResp ListQueuesResp?
 ListQueuesResp-req-id
 ListQueuesResp-queues
 ListQueuesResp-result)

(require
 (only-in type/opt
	  opt-map-orelse-value)
 (only-in net/uri/url/url
	  QParam QParams
	  Url Url-path url->string)
 (only-in format/xml/sxml
	  Sxml SXPath
	  sxpath xml->sxml select-single-node-text)
 (only-in httpclient/http11
	  StatusLine)
 (only-in httpclient/header
	  Headers make-header)
 (only-in "invoke.rkt"
	  SQSError sqs-invoke))

(struct: ListQueuesResp ([req-id : String]
			 [queues : (Listof String)]
			 [result : StatusLine]) #:transparent)

(: sqs-list-request ((Option String) -> QParams))
(define (sqs-list-request name-prefix)
  (opt-map-orelse-value name-prefix (λ: ((prefix : String))
					(list (QParam "QueueNamePrefix" prefix)))
			'()))

(: list-queues ((Option String) -> (U SQSError Void)))
(define (list-queues prefix)
  (sqs-invoke "/" 'ListQueues (sqs-list-request prefix)))
