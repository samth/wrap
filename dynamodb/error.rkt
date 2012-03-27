;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2012  Raymond Paul Racine
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

;; "{\"message\": \"Supplied AttributebValue is empty, must contain exactly one of the supported datatypes\", \"__type\": \"com.amazon.coral.validate#ValidationException\"}"

(provide
 throw is-exception-response? ddb-failure
 DDBFailure DDBFailure? 
 ConditionalCheckFailed ConditionalCheckFailed?
 ValidationException ValidationException?
 ResourceNotFound ResourceNotFound?
 InUseException InUseException?
 InvokeConditionsNotMet InvokeConditionsNotMet?)

(require 
 racket/pretty
 (only-in (planet rpr/format:1/json/tjson)
 	  Json JsObject JsObject? json->string string->json jsobject))
 	
(struct: DDBFailure exn:fail () #:transparent)

(struct: InvokeConditionsNotMet DDBFailure () #:transparent)

(struct: IllFormedResponse DDBFailure ([json : String]) #:transparent)

(struct: ValidationException DDBFailure () #:transparent)

(struct: InUseException DDBFailure () #:transparent)

(struct: ResourceNotFound DDBFailure () #:transparent)

(struct: ConditionalCheckFailed DDBFailure () #:transparent)

(define unknown-msg "Unknown error message type: ")
(define ill-formed-msg "Unparsable error response")

(define-syntax throw
  (syntax-rules ()
    ((throw excn)
     (raise (ddb-failure excn #t)))))

(: is-exception-response? (JsObject -> Boolean))
(define (is-exception-response? jsobj)
  (and (hash-has-key? jsobj 'message)
       (hash-has-key? jsobj '__type)))

(: ddb-failure (JsObject -> DDBFailure))
(define (ddb-failure jsobj)
  (if (is-exception-response? jsobj)
      (let ((type (hash-ref jsobj '__type))
	    (msg  (hash-ref jsobj 'message)))
	(if (and (string? type)
		 (string? msg))
	    (cond
	     ((string=? type "com.amazon.coral.validate#ValidationException")
	      (ValidationException msg (current-continuation-marks)))
	     ((string=? type "com.amazonaws.dynamodb.v20111205#ResourceInUseException")
	      (InUseException msg (current-continuation-marks)))
	     ((string=? type "com.amazonaws.dynamodb.v20111205#ResourceNotFoundException")
	      (ResourceNotFound msg (current-continuation-marks)))
	     ((string=? type "com.amazonaws.dynamodb.v20111205#ConditionalCheckFailedException")
	      (ConditionalCheckFailed msg (current-continuation-marks)))			
	     (else (raise (IllFormedResponse (string-append unknown-msg type) 
					     (current-continuation-marks) (json->string jsobj)))))
	    (raise (IllFormedResponse ill-formed-msg 
				      (current-continuation-marks) 
				      (json->string jsobj)))))
      (raise (IllFormedResponse ill-formed-msg 
				(current-continuation-marks) 
				(json->string jsobj)))))
