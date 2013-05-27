;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007-2011  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide dynamodb workflow)

(require
 (only-in racket/pretty
	  pretty-print)
 (only-in type/control
	  aif)
 (only-in net/http/encode
	  url-encode-string)
 (only-in net/uri/url/url
	  Authority
	  Url Url-query parse-url url->string)
 (only-in net/http/http11
	  HTTPPayload HTTPConnection-in
	  http-status StatusLine-code StatusLine-msg
	  http-successful? http-has-content?
	  http-close-connection http-invoke)
 (only-in net/http/param
	  param Param Params encode-param)
 (only-in net/http/header
	  Header Headers make-header)
 (only-in type/date
	  current-date-string-rfc-2822
	  current-date-string-iso-8601)
 (only-in format/json/tjson
	  Json JsObject jsobject
	  json->jsobject json->string
	  read-json write-json)
 (only-in "error.rkt"
	  AWSFailure
	  aws-failure
	  is-exception-response?)
 (only-in "../sts/session.rkt"
	  ensure-session)
 (only-in "../auth/authv3.rkt"
	  auth-signature)
 (only-in "config.rkt"
	  ddb-host)
 (only-in "../credential.rkt"
	  SessionCredential SessionCredential?
	  AwsCredential-session AwsCredential? BaseCredential-secret-key BaseCredential-access-key
	  SessionCredential-token current-aws-credential))

(struct: DynamoDBFailure () #:transparent)

(: request-headers Headers)
(define request-headers
  (list
   ;; (make-header-string "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.2 Safari/535.11")
   (make-header "User-Agent" "Googlebot/2.1 (+http://www.google.com/bot.html)")
   (make-header "Accept-Encoding" "gzip")

   ;; Below for Dynamodb.
   (make-header "Content-Type" "application/x-amz-json-1.0")
   ;; For Workflow
					;(make-header "Content-Type" "application/json; charset=UTF-8")
   (make-header "Connection" "Close")))

(: date-header (-> Header))
(define (date-header)
  (cons "x-amz-date" (current-date-string-rfc-2822)))

(: auth-headers (String String -> Params))
(define (auth-headers cmd tok)
  (list
   ;;(cons "host" ddb-host)
   (cons "x-amz-security-token" tok)
   (date-header)
   (cons "x-amz-target" cmd)))

(: dynamodb-invoke (Url Headers String -> JsObject))
(define (dynamodb-invoke url headers payload)
					; (with-handlers ([exn:fail?
					;                  (lambda (ex)
					;                    (pretty-print ex)
					;                    (raise ex #t))])
  (let ((conn (http-invoke 'POST url headers
			   (HTTPPayload "application/x-amz-json-1.0"
					#f #f (open-input-string payload)))))
    (if (http-successful? conn)
	(if (http-has-content? conn)
	    (let ((json (read-json (HTTPConnection-in conn))))
	      (http-close-connection conn)
	      ;;(pretty-print json)
	      (if (hash? json)
		  (let: ((json : JsObject (cast json JsObject)))
			(if (is-exception-response? json)
			    (aws-failure json)
			    json))
		  (error "Invalid DynamoDB response: not a Json Object")))
	    (jsobject '()))
	(let ((status (http-status conn)))
	  (error 'dynamodb-invoke "HTTP failure ~s: ~s"
		 (StatusLine-code status)
		 (StatusLine-msg status))))))


(: sign-request (String Params String -> String))
(define (sign-request host params body)
  (string-append "Signature=" (auth-signature host params body)))

(: authorization-header (String Params String SessionCredential -> Param))
(define (authorization-header host headers body session-cred)
  (param "x-amzn-authorization"
	 (string-append "AWS3 AWSAccessKeyId="
			(BaseCredential-access-key session-cred)
			",Algorithm=HmacSHA256,"
			;; "SignedHeaders=host;x-amz-date;x-amz-target;x-amz-security-token,"
			(sign-request host headers body))))

(: dynamodb (String String -> Json))
(define (dynamodb cmd cmd-body)
  ;;(pretty-print cmd-body)
  (if (ensure-session)
      (let* ((scred (let ((scred (AwsCredential-session (current-aws-credential))))
		      (if scred scred (error "Failure to obtain session credentials"))))
	     (stok (SessionCredential-token scred)))
	(let ((url (Url 'HTTP (Authority #f ddb-host 80) "/" '() #f))
	      (auth-hdrs (auth-headers cmd stok)))
	  (let* ((auth (authorization-header ddb-host auth-hdrs cmd-body scred))
		 (hdrs (cons auth auth-hdrs))
		 (shdrs (append hdrs request-headers)))
	    (dynamodb-invoke url shdrs cmd-body))))
      (error "DynamoDB failed to obtain a valid session token")))

(: make-service-invoker (String -> (String JsObject -> Json)))
(define (make-service-invoker host)
  (λ: ((target : String) (payload : JsObject))
      (if (ensure-session)
	  (let* ((scred (let ((scred (AwsCredential-session (current-aws-credential))))
			  (if scred scred (error "Failure to obtain session credentials"))))
		 (stok (SessionCredential-token scred)))
	    (let ((url (Url 'HTTP (Authority #f host 80) "/" '() #f))
		  (auth-hdrs (auth-headers target stok))
		  (payload (json->string payload)))
	      (let* ((auth (authorization-header host auth-hdrs payload scred))
		     (hdrs (cons auth auth-hdrs))
		     (shdrs (append hdrs request-headers)))
		(dynamodb-invoke url shdrs payload))))
	  (error "Failed to obtain a valid session token"))))


;; FIXME RPR - NEED TO VERIFY DYNAMODB ALWAYS RESPONDS WITH A JSOBJECT
(: workflow-invoker (String JsObject -> Json))
(define workflow-invoker (make-service-invoker "swf.us-east-1.amazonaws.com"))

(: workflow (String JsObject -> JsObject))
(define workflow (λ: ((target : String) (payload : JsObject))
		     (json->jsobject (workflow-invoker target payload))))
