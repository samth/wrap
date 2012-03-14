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

(provide
 put-item
 PutItemResp PutItemResp?)

(require 
 racket/pretty
 (only-in (planet rpr/format:1/json/tjson)
	  Json JsObject JsObject? json->string 
	  jsobject jsobject-add-attribute)
 (only-in "action.rkt"
	  PUT-ITEM)
 (only-in "types.rkt"
	  ReturnValues
	  Exists Exists? Exists-name Exists-exists
	  Item Item?
	  DDBType ddbtype-symbol)
 (only-in "error.rkt" 
	  DDBFailure DDBFailure?)
 (only-in "invoke.rkt"
	  dynamodb)
 (only-in "request.rkt"
	  items-json item-json
	  expected/exists-json
	  return-values-json))

;; {"TableName":"Table1",
;; 	"Item":{
;; 		"AttributeName1":{"AttributeValue1":"S"},
;; 		"AttributeName2":{"AttributeValue2":"N"},
;; 	},
;; 	"Expect":{"AttributeName3":{"Value": {"S":"AttributeValue"},{"Exists":Boolean}},
;; 	"ReturnValues":"ReturnValuesConstant"}

(struct: PutItemResp ([items : (Listof Item)]
		      [consumed : Float]) #:transparent)

(: put-item-request (String (Listof Item) (Option (U Exists Item)) ReturnValues -> String))
(define (put-item-request name items expected return-values)
  (let: ((req : JsObject (jsobject `((TableName . ,name)
				     (Item . ,(items-json items))
				     (ReturnValues . ,(return-values-json return-values))))))
    (when expected
      (jsobject-add-attribute req 'Expected (expected/exists-json expected)))
    ;;(pretty-print (json->string req))
    (json->string req)))

(: put-item (String (Listof Item) (Option (U Exists Item)) ReturnValues -> (U DDBFailure PutItemResp)))
(define (put-item name items expected return-values)
  (let ((resp (dynamodb PUT-ITEM (put-item-request name items expected return-values))))
    (cond
     ((JsObject? resp) (parse-put-item-resp resp))
     ((DDBFailure? resp) resp)
     (else
      (error (string-append "Invalid response: " (json->string resp)))))))

(: parse-put-item-resp (JsObject -> (U DDBFailure PutItemResp)))
(define (parse-put-item-resp jsobj)
  
  (: invalid-error (Symbol Json -> Nothing))
  (define (invalid-error symbol json)
    (error (string-append "Invalid attribute in response: " (symbol->string symbol) " -> " (json->string jsobj))))

  (: extract-string-value (JsObject Symbol -> String))
  (define (extract-string-value jsobj key)
    (let ((value (hash-ref jsobj key)))
      (if (string? value)
	  value
	  (invalid-error key jsobj))))

  (: parse-item (Symbol Json -> Item))
  (define (parse-item name json-value)
    (if (JsObject? json-value)
	(cond
	 ((hash-has-key? json-value 'N)
	  (Item (symbol->string name) (extract-string-value json-value 'N) 'Number))
	 ((hash-has-key? json-value 'S)
	  (Item (symbol->string name) (extract-string-value json-value 'S) 'String))
	 (else 	(invalid-error name json-value)))
	(invalid-error name json-value)))

  (: consumed (JsObject -> Float))
  (define (consumed jsobj)
    (let ((consumed (hash-ref jsobj 'ConsumedCapacityUnits)))
      (if (flonum? consumed)
	  consumed
	  0.00)))

  (let ((attrs (hash-ref jsobj 'Attributes (lambda () ((inst make-hasheq Symbol Json))))))
    (if (JsObject? attrs)
	(let ((attrs ((inst hash->list Symbol Json) attrs)))
	  (PutItemResp (map (lambda: ((attr : (Pair Symbol Json)))
			      (parse-item (car attr) (cdr attr)))
			    attrs)
		       (consumed jsobj)))
	(error (string-append "Invalid response: " (json->string jsobj))))))

