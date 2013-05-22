#lang typed/racket/base

(provide
 SQSError
 sqs-invoke)

(require
 racket/pretty
 (only-in type/opt
	  opt-get-orelse-value)
 (only-in httpclient/mimetype-const
	  X-WWW-FORM-URLENCODED)
 (only-in net/uri/url/url
	  Url Url-path Authority
	  qparams->string)
 (only-in httpclient/header
	  Headers make-header header->string content-type)
 (only-in net/uri/url/url
	  QParam QParams
	  Url Url-path url->string)
 (only-in httpclient/http11
	  HTTPConnection-in
	  HTTPPayload http-invoke)
 (only-in format/xml/sxml
	  Sxml SXPath sxpath html->sxml xml->sxml extract-text extract-integer)
 (only-in "../../auth/authv2.rkt"
	  authv2-signature)
 (only-in "../config.rkt"
	  sqs-host sqs-auth-version sqs-api-version signature-method))

(struct: SQSError () #:transparent)

(define-type Action (U 'ListQueues 'SendMessage 'ReceiveMessage))

(: invoke-uri (String -> Url))
(define (invoke-uri path)
  (Url 'HTTPS (Authority #f sqs-host 80) path '() #f))

(: action->string (Action -> String))
(define (action->string action)
  (symbol->string action))

(: signed-values (Url Action QParams -> QParams))
(define (signed-values url sqs-action qparams)
  (authv2-signature sqs-api-version "POST" sqs-host
		    (action->string sqs-action) (Url-path url) qparams))

(: form-payload (QParams -> HTTPPayload))
(define (form-payload qparams)
  (let ((payload-string (qparams->string qparams)))
    (HTTPPayload X-WWW-FORM-URLENCODED
		 #f
		 (string-length (opt-get-orelse-value payload-string ""))
		 (open-input-string (opt-get-orelse-value payload-string "")))))

(: sqs-http-invoke (All (a) (Url HTTPPayload (Sxml -> (U SQSError a)) -> (U SQSError a))))
(define (sqs-http-invoke url payload resp-parser)
  (with-handlers ([exn:fail?
		   (lambda (ex) (SQSError))])
		 (let ((http-headers '()))
		   (let ((conn (http-invoke 'POST url http-headers payload)))
		     (pretty-print conn)
		     (let ((page (xml->sxml (HTTPConnection-in conn) '())))
		       (resp-parser page))))))


(: sqs-invoke (String Action QParams -> (U SQSError Void)))
(define (sqs-invoke queue-path action qparams)
  (let ((url (invoke-uri queue-path)))
    (let ((sig-headers (signed-values url action qparams)))
      (let ((payload (form-payload (append sig-headers qparams))))
	((inst sqs-http-invoke Void)
	 url
	 payload
	 (lambda: ((sxml : Sxml))
		  (pretty-print  sxml)
		  (SQSError)
		  (void)))))))
