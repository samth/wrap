#lang typed/racket/base

(provide
 invalid-error attr-value
 parse-capacity parse-key-schema)

(require  
 (only-in (planet knozama/webkit:1/formats/tjson)
	  Json JsObject JsObject? json->string 
	  jsobject)
 (only-in "types.rkt"
	  string->DDBType
	  Key KeySchema Throughput))

(define-syntax attr-of-type
  (syntax-rules ()
    ((is-type of-type? jsobj attr)
     (let ((json (attr-value jsobj attr)))
       (if (of-type? json)
	   json
	   (invalid-error attr jsobj))))))

(: invalid-error (Symbol Json -> Nothing))
(define (invalid-error symbol json)
  (error (string-append "Invalid attribute in response: " (symbol->string symbol) " -> " (json->string json))))

(: attr-value (All (a) JsObject Symbol (Any -> Boolean : a)-> a))
(define (attr-value jsobj attr type-of?)
  (if (hash-has-key? jsobj attr)
      (let ((val (hash-ref jsobj attr)))
	(if (type-of? val)
	    val
	    (invalid-error attr jsobj)))
      (invalid-error attr jsobj)))

(: parse-capacity (JsObject -> Throughput))
(define (parse-capacity jsobj)
  (let ((write (attr-value jsobj 'WriteCapacityUnits exact-integer?))
	(read  (attr-value jsobj 'ReadCapacityUnits exact-integer?)))
    (if (and (> write 0)
	     (> read 0))
	(Throughput read write)
	(invalid-error 'ProvisionedThroughput jsobj))))


(: parse-key (JsObject Symbol -> Key))
(define (parse-key jsobj key-symbol)
  (let ((name (attr-value jsobj 'AttributeName string?))
	(type (string->DDBType (attr-value jsobj 'AttributeType string?))))
    (if type
	(Key name type)
	(invalid-error 'AttributeType jsobj))))

(: parse-key-schema (JsObject -> KeySchema))
(define (parse-key-schema jsobj)
  (let ((hash-key (parse-key (attr-value jsobj 'HashKeyElement JsObject?) 'HashKeyElement))
	(range-key (if (hash-has-key? jsobj 'RangeKeyElement)
		       (parse-key (attr-value jsobj 'RangeKeyElement JsObject?) 'RangeKeyElement)
		       #f)))
    (KeySchema hash-key range-key)))
