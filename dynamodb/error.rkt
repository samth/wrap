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
 AWSFailure
 ;throw 
 is-exception-response? aws-failure)
 
(require 
 (only-in "../../format/json/tjson.rkt"
          Json JsObject json->string string->json jsobject))

(struct: AWSFailure exn:fail ([type : String]) #:transparent)

(define unknown-msg "Unknown error message type: ")
(define ill-formed-msg "Unparsable error response")

;(define-syntax throw
;  (syntax-rules ()
;    ((throw excn)
;     (raise (ddb-failure excn #t)))))

(: is-exception-response? (JsObject -> Boolean))
(define (is-exception-response? jsobj)
  (and (hash-has-key? jsobj 'message)
       (hash-has-key? jsobj '__type)))

(: malformed-response (String -> Nothing))
(define (malformed-response resp)
  (raise (AWSFailure resp (current-continuation-marks) "MalformedJSON")))

(: aws-failure (JsObject -> Nothing))
(define (aws-failure jsobj)      
  (if (is-exception-response? jsobj)
      (let ((type (hash-ref jsobj '__type))
            (msg  (hash-ref jsobj 'message)))
        (if (and (string? type)
                 (string? msg))
            (raise (AWSFailure msg (current-continuation-marks) type))
            (malformed-response (json->string jsobj))))
      (malformed-response (json->string jsobj))))

