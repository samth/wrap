#lang typed/racket/base

(provide
 main)

(require
 racket/pretty
 (only-in grip/data/either
	  Either Left Left? Right left right)
 (only-in gut/http/http11
	  HTTPPayload HTTPConnection-in
	  http-status StatusLine-code StatusLine-msg
	  http-successful? http-has-content?
	  http-close-connection http-invoke)
 (only-in gut/uri/url/url
	  Url
	  url->string
	  parse-url)
 (only-in gut/format/json/tjson
	  Json JsObject jsobject
	  json->jsobject json->string
	  read-json write-json))

(define-type Credentials String)

(struct: InvokeFailure ([status : String]
			[msg    : String]
			[url    : Url]
			[json   : Json]) #:transparent)

(define test-url
  "http://www.mapquestapi.com/geocoding/v1/address?location=lancaster%20pa&key=~a")

(: invoke (Url -> (Either InvokeFailure JsObject)))
(define (invoke url)
  (let ((conn (http-invoke 'GET url '() #f)))
    (with-handlers ([exn:fail? (λ (ex)
				 (begin
				   (http-close-connection conn)
				   (Left (InvokeFailure "xxx"
							(format "~s" ex)
							url (jsobject '())))))])
		   (let ((json (read-json (HTTPConnection-in conn))))
		     (http-close-connection conn)
		     (if (hash? json)
			 (let: ((json : JsObject (cast json JsObject)))
			       (Right json))
			 (Left (InvokeFailure "x200" "Malformed Json"
					      url json)))))))

(: geocode (Credentials String -> Void))
(define (geocode cred address)
  (let ((url (right (parse-url (format test-url cred)))))
    (pretty-print (invoke url))))

(define key "MY API KEY")

(define (main)
  (geocode key "TEST ME"))
