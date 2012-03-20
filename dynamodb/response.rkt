#lang typed/racket/base

#|
Common routines for parsing DynamoDB responses.
|# 

(provide
 parse-fail parse-last-key
 parse-items parse-keyval
 parse-consumed-capacity
 parse-positive-integer)

(require
 racket/pretty
 (only-in "types.rkt"
	  ddbtype-symbol DDBType
	  Item ItemVal ItemKey KeyVal)
 (only-in (planet rpr/format:1/json/tjson)
	  JsObject-empty
 	  Json JsObject JsObject? json->string string->json 
	  jsobject jsobject-add-attribute))

(: parse-fail (Json -> Nothing))
(define (parse-fail json)
  (error "Invalid response: " (json->string json)))

(: parse-item-value (Symbol Json -> (U (Listof String) String)))
(define (parse-item-value type json)    
  (if (JsObject? json)
      (let ((value (hash-ref json type)))
	(if (or (string? value)
		(and (list? value)
		     (andmap string? value)))
	    value
	    (parse-fail json)))
      (parse-fail json)))

(: parse-item (String JsObject -> Item))
(define (parse-item name json)
  (cond 
   ((hash-has-key? json 'S) 
    (Item name (parse-item-value 'S json) 'String))
   ((hash-has-key? json 'N) 
    (Item name (parse-item-value 'N json) 'Number))
   ((hash-has-key? json 'SS) 
    (Item name (parse-item-value 'SS json) 'StringSet))
   ((hash-has-key? json 'SN) 
    (Item name (parse-item-value 'SN json) 'NumberSet))
   (else (parse-fail json))))

(: parse-keyval (JsObject -> KeyVal))
(define (parse-keyval json)
  (cond 
   ((hash-has-key? json 'S) 
    (let ((skey (parse-item-value 'S json)))
      (if (string? skey)
	  (KeyVal skey 'String)
	  (parse-fail json))))
   ((hash-has-key? json 'N) 
    (let ((skey (parse-item-value 'N json)))
      (if (string? skey)
	  (KeyVal skey 'Number)
	  (parse-fail json))))
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

(: parse-last-key (JsObject -> (Option ItemKey)))
(define (parse-last-key resp)
  
  (: item-val (JsObject Symbol -> (Option KeyVal)))
  (define (item-val resp key)
    (if (hash-has-key? resp key)
	(let ((key (hash-ref resp key)))
	  (if (JsObject? key)
	      (parse-keyval key)
	      #f))
	#f))
  
  (if (hash-has-key? resp 'LastEvaluatedKey)
      (let ((attrs (hash-ref resp 'LastEvaluatedKey)))
	(if (JsObject? attrs)
	    (let ((hash-key (item-val attrs 'HashKeyElement))
		  (range-key (item-val attrs 'RangeKeyElement)))
	      (if hash-key
		  (ItemKey hash-key range-key)
		  #f))
	    #f))
      #f))
