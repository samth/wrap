#lang typed/racket/base

(provide
 invalid-error 
 attr-value attr-value-string attr-value-jsobject attr-value-jslist
 parse-capacity parse-key-schema)

(require  
 (only-in "../../format/json/tjson.rkt"
          Json JsObject JsList
          json->string 
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

(: attr-value (All (A) JsObject Symbol (Json -> Boolean : A) -> A))
(define (attr-value jsobj attr type-of?)
  (if (hash-has-key? jsobj attr)
      (let ((json (hash-ref jsobj attr)))
        (if (type-of? json)
            json
            (invalid-error attr json)))
      (invalid-error attr jsobj)))

(: attr-value-string (JsObject Symbol -> String))
(define (attr-value-string jsobj attr)
  (attr-value jsobj attr string?))

(: attr-value-jsobject (JsObject Symbol -> JsObject))
(define (attr-value-jsobject jsobj attr)
  (let ((json (attr-value jsobj attr hash?)))
    (cast json JsObject)))    

(: attr-value-jslist (JsObject Symbol -> JsList))
(define (attr-value-jslist jsobj attr)
  (let ((json (attr-value jsobj attr list?)))
    (cast json JsList)))
   
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
  (let ((hash-key (parse-key (attr-value-jsobject jsobj 'HashKeyElement) 'HashKeyElement))
        (range-key (if (hash-has-key? jsobj 'RangeKeyElement)
                       (parse-key (attr-value-jsobject jsobj 'RangeKeyElement) 'RangeKeyElement)
                       #f)))
    (KeySchema hash-key range-key)))
