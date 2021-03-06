;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's AWS API Library
;; Copyright (C) 2007-2013  Raymond Paul Racine
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

(provide
 S3Payload S3Payload?
 S3Response S3Response? S3Response-http S3Response-sxml
 empty-response make-empty-error-response
 make-base-uri
 s3-invoke s3-get-object s3-get-object-pipe-to-file)

(require
 (only-in racket/port
	  port->bytes)
 (only-in gut/uri/url/url
	  QParams qparams->string
	  Url Authority Url-path)
 (only-in grip/data/date
	  current-date-string-rfc-2822)
 (only-in gut/http/heading
	  DATE HOST)
 (only-in gut/http/header
	  Header Headers
	  header->string
	  make-header
	  date-header
	  content-length
	  content-type
	  content-md5)
 (only-in gut/http/http11
	  Method HTTPPayload HTTPPayload-md5 HTTPPayload-mime
	  http-method->string http-status-code http-has-content?
	  ResponseHeader-status StatusLine
	  HTTPConnection-in HTTPConnection-header
	  http-invoke http-close-connection )
 (only-in gut/format/xml/sxml
	  Sxml
	  xml->sxml)
 (only-in "../credential.rkt"
	  current-aws-credential
	  AwsCredential
	  BaseCredential-access-key
	  BaseCredential-secret-key)
 (only-in "../auth.rkt"
	  aws-auth-str
	  aws-auth-mac)
 (only-in "../configuration.rkt"
	  s3-host)
 (only-in "types.rkt"
	  Range Range-from Range-to))

(struct: S3Response ([http : StatusLine]
		     [sxml : Sxml]) #:transparent)

(struct: S3Payload ([length : Index]
		    [mime : String]
		    [md5  : String]
		    [inport  : Input-Port]) #:transparent)

(: empty-response (List Symbol))
(define empty-response '(*TOP*))

(: make-empty-error-response (Integer String -> S3Response))
(define (make-empty-error-response status-code message)
  (S3Response (StatusLine 'HTTP/1.1 status-code message) empty-response))

(: make-base-uri ((Option String) String QParams -> Url))
(define (make-base-uri bucket path qparams)

  (: make-bucket-host (-> String))
  (define (make-bucket-host)
    (if bucket
	(string-append bucket "." s3-host)
	s3-host))

  (Url 'HTTP (Authority #f (make-bucket-host) 80) path qparams #f))

(: authorization-header (AwsCredential String -> Header))
(define (authorization-header credential auth-str)
  (make-header "Authorization"
	       (string-append  "AWS "
			       (BaseCredential-access-key credential)
			       ":"
			       (aws-auth-mac (BaseCredential-secret-key credential)
					     auth-str))))

(: range-header (Range -> Header))
(define (range-header range)
  (make-header "range" (format "bytes=~s-~s"
			       (Range-from range)
			       (Range-to range))))

(: s3-get-object-pipe-to-file (String String Path (Option Range)-> S3Response))
(define (s3-get-object-pipe-to-file bucket path file-path range)
  (define: buff-sz : Integer (* 10 1024))
  (define: zero : Integer 0)
  (let ((url (make-base-uri bucket path '())))
    (if url
	(let* ((datetime (current-date-string-rfc-2822))
	       (canonical-resource (string-append "/" bucket (Url-path url)))
	       (core-headers  (list (make-header DATE datetime)
				    (authorization-header (current-aws-credential)
							  (aws-auth-str (http-method->string 'GET)
									"" ""
									datetime '()
									canonical-resource))))
	       (headers (if range (cons (range-header range) core-headers) core-headers)))
	  (let ((connection (http-invoke 'GET url headers #f)))
	    (with-handlers [(exn:fail? (λ (ex)
					 ((error-display-handler) "ERROR in S3 Object GET" ex)
					 (http-close-connection connection)
					 (S3Response (StatusLine 'HTTP/1.1 500
								 (exn-message ex))
						     empty-response)))]
			   (if (http-has-content? connection)
			       (call-with-output-file
				   file-path
				 (λ: ((outp : Output-Port))
				     (let* ((inp (HTTPConnection-in connection))
					    (buffer (make-bytes buff-sz)))
				       (let: loop : S3Response ((bs : (U EOF Integer) (read-bytes! buffer inp)))
					     (if (eof-object? bs)
						 (begin
						   (http-close-connection connection)
						   (S3Response (StatusLine 'HTTP/1.1 200 "OK") empty-response))
						 (begin
						   (write-bytes buffer outp zero bs)
						   (loop (read-bytes! buffer inp)))))))
				 #:mode 'binary
				 #:exists 'error)
			       (S3Response (StatusLine 'HTTP/1.1 500
						       "S3 GET of object returned no content")
					   empty-response)))))
	(S3Response (StatusLine 'HTTP/1.1 400
				(string-append "Bad Request - Malformed URL"))
		    empty-response))))

(: s3-get-object (String String (Option Range) -> (U S3Response Bytes)))
(define (s3-get-object bucket path range)
  (let ((url (make-base-uri bucket path '())))
    (if url
	(let* ((datetime (current-date-string-rfc-2822))
	       (canonical-resource (string-append "/" bucket (Url-path url)))
	       (core-headers (list (make-header DATE datetime)
				   (authorization-header (current-aws-credential)
							 (aws-auth-str (http-method->string 'GET)
								       "" ""
								       datetime '()
								       canonical-resource))))
	       (headers (if range (cons (range-header range) core-headers) core-headers)))
	  (let ((connection (http-invoke 'GET url headers #f)))
	    (with-handlers [(exn:fail? (λ (ex)
					 ((error-display-handler) "ERROR in S3 Object GET" ex)
					 (http-close-connection connection)
					 (S3Response (StatusLine 'HTTP/1.1 500
								 (exn-message ex))
						     empty-response)))]
			   (if (http-has-content? connection)
			       (let ((bytes (port->bytes (HTTPConnection-in connection))))
				 (http-close-connection connection)
				 bytes)
			       (bytes)))))
	(S3Response (StatusLine 'HTTP/1.1 400
				(string-append "Bad Request - Malformed URL"))
		    empty-response))))


;; S3 RPC Req/Resp API invocation
(: s3-invoke (Method (Option String) String QParams Headers (Option HTTPPayload) -> S3Response))
(define (s3-invoke action bucket path query-params headers payload)
  (let ((url (make-base-uri bucket path query-params)))
    (if url
	(let* ((datetime (current-date-string-rfc-2822))
	       (canonical-resource (if bucket
				       (string-append "/" bucket (Url-path url))
				       (Url-path url)))
	       (md5 (if payload
			(let ((md5 (HTTPPayload-md5 payload)))
			  (if md5 md5 ""))
			""))
	       (mime (if payload
			 (HTTPPayload-mime payload)
			 ""))
	       (core-headers  (list (make-header DATE datetime)
				    (authorization-header (current-aws-credential)
							  (aws-auth-str (http-method->string action)
									md5 mime
									datetime '()
									canonical-resource)))))
	  (let ((connection (http-invoke action
					 url
					 (append core-headers headers)
					 payload)))
	    (with-handlers [(exn:fail? (lambda (ex)
					 ((error-display-handler) "ERROR in S3 invocation." ex)
					 (displayln ex)
					 (http-close-connection connection)
					 (S3Response (StatusLine 'HTTP/1.1 440
								 (string-append "Bad Request - "
										(exn-message ex)))
						     empty-response)))]

			   (if (http-has-content? connection)
			       (cond
				((eq? action 'HEAD)
				 (http-close-connection connection)
				 (S3Response (ResponseHeader-status (HTTPConnection-header connection))
					     empty-response))
				(else
				 (let ((results (xml->sxml (HTTPConnection-in connection) '())))
				   (http-close-connection connection)
				   (S3Response (ResponseHeader-status (HTTPConnection-header connection))
					       results))))
			       (S3Response (ResponseHeader-status (HTTPConnection-header connection))
					   empty-response)))))
	(S3Response (StatusLine 'HTTP/1.1 400
				(string-append "Bad Request - Malformed URL"))
		    empty-response))))
