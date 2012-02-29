#lang typed/racket/base

#|
Common routines for parsing DynamoDB responses.
|# 

(provide
 parse-fail
 parse-items parse-keyval
 parse-consumed-capacity
 parse-positive-integer)

(require
 (only-in "types.rkt"
	  ddbtype-symbol DDBType
	  Item ItemVal KeyVal)
 (only-in (planet knozama/webkit:1/formats/tjson)
	  JsObject-empty
 	  Json JsObject JsObject? json->string string->json 
	  jsobject jsobject-add-attribute))

(: parse-fail (Json -> Nothing))
(define (parse-fail json)
  (error "Invalid response: " (json->string json)))

(: parse-item-value (Symbol Json -> String))
(define (parse-item-value type json)    
  (if (JsObject? json)
      (let ((value (hash-ref json type)))
	(if (string? value)
	    value
	    (parse-fail json)))
      (parse-fail json)))	  

(: parse-item (String JsObject -> Item))
(define (parse-item name json)
  (cond 
   ((hash-has-key? json 'S) (Item name (parse-item-value 'S json) 'String))
   ((hash-has-key? json 'N) (Item name (parse-item-value 'N json) 'Number))	
   (else (parse-fail json))))

(: parse-keyval (JsObject -> KeyVal))
(define (parse-keyval json)
  (cond 
   ((hash-has-key? json 'S) (KeyVal (parse-item-value 'S json) 'String))
   ((hash-has-key? json 'N) (KeyVal (parse-item-value 'N json) 'Number))
   (else (parse-fail json))))

(: parse-consumed-capacity (JsObject -> Float))
(define (parse-consumed-capacity resp)
  (let ((consumed (hash-ref resp 'ConsumedCapacityUnits)))
    (if (flonum? consumed)
	consumed
	(parse-fail resp))))


(: parse-positive-integer (JsObject Symbol -> Exact-Nonnegative-Integer))
(define (parse-positive-integer resp symbol)
  (let ((int (hash-ref resp symbol)))
    (if (and (exact-integer? int)
	     (>= int 0))
	int
	(parse-fail resp))))

(: parse-items (JsObject -> (HashTable String Item)))
(define (parse-items jattrs)
  (let: ((items : (HashTable String Item) (make-hash)))
    (let: loop : (HashTable String Item) 
	  ((attrs : (Listof (Pair Symbol Json)) ((inst hash->list Symbol Json) jattrs)))
	  (if (null? attrs)
	      items
	      (let* ((jitem (car attrs))
		     (name (symbol->string (car jitem)))
		     (type-value (cdr jitem)))
		(if (JsObject? type-value)
		    (begin
		      (hash-set! items name (parse-item name type-value))
		      (loop (cdr attrs)))
		    (parse-fail jattrs)))))))
