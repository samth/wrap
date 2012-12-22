#lang typed/racket/base

(provide 
 query
 (struct-out QueryResp))

(require
 racket/pretty
 (only-in "../../format/json/tjson.rkt"
          Json JsObject jsobject json->string)
 (only-in "error.rkt"
          illformed-response)
 (only-in "types.rkt"
          Range Range-values Range-operator
          ddbtype-symbol
          Operator operator->string 
          Item ItemVal ItemVal-type ItemVal-value
          ItemKey KeyVal)
 (only-in "action.rkt"
          QUERY)
 (only-in "invoke.rkt"
          dynamodb)
 (only-in "request.rkt"
          itemkey-json keyvalue-json)
 (only-in "response.rkt"
          parse-last-key
          parse-positive-integer
          parse-consumed-capacity
          parse-fail
          parse-keyval
          parse-items))

(struct: QueryResp ([lastkey : (Option ItemKey)]
                    [consumed : Float]
                    [count : Exact-Nonnegative-Integer]
                    [items : (Listof (HashTable String Item))]) #:transparent)

(: item-val-jsobject (ItemVal -> JsObject))
(define (item-val-jsobject item-val)
  (jsobject `((,(ddbtype-symbol (ItemVal-type item-val)) . ,(ItemVal-value item-val)))))

(: query-range-values ((Listof ItemVal) -> (Listof JsObject)))
(define (query-range-values item-vals)  
  (map item-val-jsobject item-vals))

(: query-range (Range -> Json))
(define (query-range range)
  (jsobject `((AttributeValueList . ,(query-range-values (Range-values range)))
              (ComparisonOperator . ,(operator->string (Range-operator range))))))

(: query-request (String (Listof String) Exact-Positive-Integer Boolean Boolean
                         KeyVal (Option Range) Boolean (Option ItemKey) -> String))
(define (query-request table fields limit consistent? count? hash-key range forward? exclusive-start-key)
  (let ((attrs
         `((TableName . ,table)
           (HashKeyValue . ,(keyvalue-json hash-key))
           (ConsistentRead . ,consistent?)
           (AttributesToGet . ,fields)
           (Limit . ,limit)
           (Count . ,count?))))
    (let ((attrs (if exclusive-start-key
                     (cons `(ExclusiveStartKey . ,(itemkey-json exclusive-start-key)) attrs)
                     attrs)))
      (let ((attrs (if range
                       (cons `(RangeKeyCondition . ,(query-range range)) attrs)
                       attrs)))
        (let: ((req : JsObject (jsobject attrs)))
          (when (null? fields)
            (hash-remove! req 'AttributesToGet))
          (json->string req))))))


(: query (String (Listof String) Exact-Positive-Integer Boolean Boolean
                 KeyVal (Option Range) Boolean (Option ItemKey) -> QueryResp))
(define (query table fields limit consistent? count? hash-key range forward? exclusive-start-key)
  (let ((resp (dynamodb QUERY (query-request table fields limit consistent? count? hash-key range forward? exclusive-start-key))))
    (if (hash? resp)
        (let*: ((resp : JsObject (cast resp JsObject))
                (items-js : Json (hash-ref resp 'Items)))
          (if (and (list? items-js)
                   (andmap hash? items-js))
              (let* ((items-js (cast items-js (Listof JsObject)))
                     (items (map parse-items items-js)))
                (let ((last-key (parse-last-key resp))
                      (consumed (parse-consumed-capacity resp))
                      (count (parse-positive-integer resp 'Count)))
                  (QueryResp last-key consumed count items)))
              (parse-fail resp)))
        (illformed-response (string-append "Unparsable response from query of " table)))))

