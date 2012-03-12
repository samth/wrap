#lang typed/racket/base

(provide 
 SQSError
 sqs-invoke)

(require
 racket/pretty
 (only-in (planet rpr/httpclient:1/http/mimetype-const)
	  X-WWW-FORM-URLENCODED)
 (only-in (planet rpr/httpclient:1/uri/url/param)
	  Params Param params->query)
 (only-in (planet rpr/httpclient:1/http/header)
          Headers make-header header->string content-type)
 (only-in (planet rpr/httpclient:1/uri)
	  Uri Uri-path make-uri uri->string)
 (only-in (planet rpr/httpclient:1/http/http11)
	  HTTPConnection-in
	  HTTPPayload http-invoke)
 (only-in (planet rpr/format:1/xml/sxml)
	  Sxml SXPath sxpath html->sxml xml->sxml extract-text extract-integer)
 (only-in "../../auth/authv2.rkt"
	  authv2-signature)
 (only-in "../config.rkt"
	  sqs-host sqs-auth-version sqs-api-version signature-method))

(struct: SQSError () #:transparent)

(define-type Action (U 'ListQueues 'SendMessage 'ReceiveMessage))

(: invoke-uri (String -> Uri))
(define (invoke-uri path)
  (make-uri "http" #f sqs-host 80 path #f ""))

(: action->string (Action -> String))
(define (action->string action)
  (symbol->string action))

 (: signed-values (Uri Action Headers -> Headers))
 (define (signed-values uri sqs-action headers)
   (authv2-signature sqs-api-version "POST" sqs-host 
		     (action->string sqs-action) (Uri-path uri) headers))

(: form-payload (Headers -> HTTPPayload))
(define (form-payload headers)
  (let ((payload-string (params->query headers)))
    (pretty-print payload-string)
    (HTTPPayload X-WWW-FORM-URLENCODED 
		 #f
		 (string-length payload-string)
		 (open-input-string payload-string))))

(: sqs-http-invoke (All (a) (Uri HTTPPayload (Sxml -> (U SQSError a)) -> (U SQSError a))))
(define (sqs-http-invoke url payload resp-parser)
  (with-handlers ([exn:fail? 
		   (lambda (ex) (SQSError))])
    (let ((http-headers '()))
      (let ((conn (http-invoke 'POST url http-headers payload)))
	(pretty-print conn)
	(let ((page (xml->sxml (HTTPConnection-in conn) '())))
	  (resp-parser page))))))


(: sqs-invoke (String Action Headers -> (U SQSError Void)))
(define (sqs-invoke queue-path action headers) 
  (let ((url (invoke-uri queue-path)))
    (pretty-print (uri->string url))
    (let ((sig-headers (signed-values url action headers)))
      (let ((payload (form-payload (append sig-headers headers))))
	((inst sqs-http-invoke Void)
	 url
	 payload
	 (lambda: ((sxml : Sxml))
	   (pretty-print  sxml)
	   (SQSError)
	   (void)))))))
  
