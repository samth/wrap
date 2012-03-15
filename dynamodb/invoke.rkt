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

(provide dynamodb)

(require
 racket/pretty
 (only-in racket/port
	  call-with-input-bytes)
 (only-in (planet rpr/prelude:1/std/control)
	  aif)
 (only-in (planet rpr/httpclient:1/uri/url/encode)
	  url-encode-string)
 (only-in (planet rpr/httpclient:1/uri)
	  Uri Uri-query make-uri parse-uri uri->string)
 (only-in (planet rpr/httpclient:1/http/http11)
	  HTTPPayload HTTPConnection-in 
	  http-successful? http-close-connection http-invoke)
 (only-in (planet rpr/httpclient:1/uri/url/param)
	  param Param Params encode-param)
 (only-in (planet rpr/httpclient:1/http/header)
          Header Headers make-header)
 (only-in (planet rpr/prelude:1/type/date)
	  current-date-string-rfc-2822
	  current-date-string-iso-8601)
 (only-in (planet rpr/format:1/json/tjson)
	  Json JsObject JsObject? read-json write-json)
 (only-in "error.rkt"
	  DDBFailure DDBFailure? ddb-failure
	  is-exception-response? throw)
 (only-in "../sts/session.rkt"
	  ensure-session)
 (only-in "../auth/authv3.rkt"
	  auth-signature)
 (only-in "config.rkt"
	  ddb-host)
 (only-in (planet rpr/aws:1/credential)
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
   (make-header "Content-Type" "application/x-amz-json-1.0")
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

(: dynamodb-invoke (Uri Headers String -> (U DDBFailure JsObject)))
(define (dynamodb-invoke url headers payload)
  (with-handlers ([exn:fail?
		   (lambda (ex) 
		     (pretty-print ex)
		     (raise ex #t))])
    (let ((conn (http-invoke 'POST url headers 
			     (HTTPPayload "application/x-amz-json-1.0"
					  #f #f (open-input-string payload)))))
      ;;(pretty-print headers)
      (let ((json (read-json (HTTPConnection-in conn))))
	(http-close-connection conn)
	;;(pretty-print json)
	(if (JsObject? json)
	    (if (is-exception-response? json)
		(ddb-failure json)
		json)
	    (error "Invalid DynamoDB response: not a Json Object"))))))

(: sign-request (Params String -> String))
(define (sign-request params body)
  (string-append "Signature=" (auth-signature ddb-host params body)))

(: authorization-header (Params String SessionCredential -> Param))
(define (authorization-header headers body session-cred)
  (param "x-amzn-authorization"
	 (string-append "AWS3 AWSAccessKeyId=" 
			(BaseCredential-access-key session-cred)
			",Algorithm=HmacSHA256,"
			;; "SignedHeaders=host;x-amz-date;x-amz-target;x-amz-security-token,"
			(sign-request headers body))))

(: dynamodb (String String -> (U DDBFailure Json)))
(define (dynamodb cmd cmd-body)
  ;;(pretty-print cmd-body)
  (if (ensure-session)
      (let* ((scred (let ((scred (AwsCredential-session (current-aws-credential))))
		      (if scred scred (error "Failure to obtain session credentials"))))
	     (stok (SessionCredential-token scred)))
	(let ((url (make-uri "http" #f ddb-host 80 "/" #f #f))
	      (auth-hdrs (auth-headers cmd stok)))
	  (let* ((auth (authorization-header auth-hdrs cmd-body scred))
		 (hdrs (cons auth auth-hdrs))
		 (shdrs (append hdrs request-headers)))
	    (dynamodb-invoke url shdrs cmd-body))))
      (error "DynamoDB failed to obtain a valid session token")))
