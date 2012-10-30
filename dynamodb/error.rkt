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
 illformed-response
 ConditionalCheckFailed ConditionalCheckFailed?
 ValidationException ValidationException?
 ResourceNotFound ResourceNotFound?
 InUseException InUseException?
 InvokeConditionsNotMet InvokeConditionsNotMet?)

(require 
 (only-in "../../format/json/tjson.rkt"
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


(: illformed-response (String -> Nothing))
(define (illformed-response resp)
  (raise (IllFormedResponse unknown-msg (current-continuation-marks) resp)))

(: ddb-failure (JsObject -> Nothing))
(define (ddb-failure jsobj)
  
  (: raise-illformed (String -> Nothing))
  (define (raise-illformed msg)
    (raise (IllFormedResponse ill-formed-msg 
                              (current-continuation-marks) 
                              msg)))
  
  (if (is-exception-response? jsobj)
      (let ((type (hash-ref jsobj '__type))
            (msg  (hash-ref jsobj 'message)))
        (if (and (string? type)
                 (string? msg))
            (cond
              ((string=? type "com.amazon.coral.validate#ValidationException")
               (raise (ValidationException msg (current-continuation-marks))))
              ((string=? type "com.amazonaws.dynamodb.v20111205#ResourceInUseException")
               (raise (InUseException msg (current-continuation-marks))))
              ((string=? type "com.amazonaws.dynamodb.v20111205#ResourceNotFoundException")
               (raise (ResourceNotFound msg (current-continuation-marks))))
              ((string=? type "com.amazonaws.dynamodb.v20111205#ConditionalCheckFailedException")
               (raise (ConditionalCheckFailed msg (current-continuation-marks))))
              (else (raise-illformed (json->string jsobj))))
            (raise-illformed (json->string jsobj))))
      (raise-illformed (json->string jsobj))))

