#lang typed/racket/base

(provide
 scan Filter
 ScanResp ScanResp? ScanResp-lastkey 
 ScanResp-consumed ScanResp-count
 ScanResp-scanned ScanResp-items)

(require 
 racket/pretty
 (only-in "../../format/json/tjson.rkt"
          Json JsObject JsObject? JsObject-empty
          json->string jsobject jsobject-add-attribute)
 (only-in "error.rkt"
          illformed-response)
 (only-in "action.rkt"
          SCAN)
 (only-in "invoke.rkt"
          dynamodb)
 (only-in "request.rkt"
          itemkey-json)
 (only-in "types.rkt"
          Filter Filter-values Filter-operator Filter-item
          ddbtype-symbol
          Operator operator->string 
          Item ItemVal ItemVal-type ItemVal-value
          ItemKey KeyVal)
 (only-in "response.rkt"
          parse-last-key
          parse-positive-integer
          parse-consumed-capacity
          parse-fail
          parse-keyval
          parse-items))

(struct: ScanResp ([lastkey : ItemKey]
                   [consumed : Float]
                   [count : Exact-Nonnegative-Integer]
                   [scanned : Exact-Nonnegative-Integer]
                   [items : (Listof (HashTable String Item))]) #:transparent)

(: item-val-jsobject (ItemVal -> JsObject))
(define (item-val-jsobject item-val)
  (jsobject `((,(ddbtype-symbol (ItemVal-type item-val)) . ,(ItemVal-value item-val)))))

(: scan-filter-values ((Listof ItemVal) -> (Listof JsObject)))
(define (scan-filter-values item-vals)  
  (map item-val-jsobject item-vals))

(: scan-filter (Filter -> (Pair Symbol Json)))
(define (scan-filter filter)
  (let ((condition (jsobject `((AttributeValueList . ,(scan-filter-values (Filter-values filter)))
                               (ComparisonOperator . ,(operator->string (Filter-operator filter)))))))
    `(,(string->symbol (Filter-item filter)) . ,condition)))

(: scan-request (String (Listof String) Exact-Positive-Integer Boolean (Listof Filter) (Option ItemKey) -> String))
(define (scan-request table attrs limit count? filters exclusive-start-key)
  (let: ((req-attrs : (Listof (Pair Symbol Json))
                    (let ((base-attrs
                           `((TableName . ,table)
                             (AttributesToGet . ,attrs)
                             (ScanFilter . ,(jsobject (map scan-filter filters)))
                             (Count . ,count?)
                             (Limit . ,limit))))
                      (if exclusive-start-key
                          (cons `(ExclusiveStartKey . ,(itemkey-json exclusive-start-key)) base-attrs)
                          base-attrs))))
    (let: ((req : JsObject (jsobject req-attrs)))
      (when (null? attrs)
        (hash-remove! req 'AttributesToGet))
      (json->string req))))

(: scan (String (Listof String) Exact-Positive-Integer Boolean (Listof Filter) (Option ItemKey) -> ScanResp))
(define (scan table attrs limit count? filters exclusive-start-key)
  (let ((resp (dynamodb SCAN (scan-request table attrs limit count? filters exclusive-start-key))))
    (if (JsObject? resp)
        (let: ((items-js : Json (hash-ref resp 'Items)))
          (if (and (list? items-js)
                   (andmap JsObject? items-js))
              (let ((items (map parse-items items-js)))
                (let ((last-key (parse-last-key resp))
                      (consumed (parse-consumed-capacity resp))
                      (count (parse-positive-integer resp 'Count))
                      (scanned (parse-positive-integer resp 'ScannedCount)))
                  (if last-key
                      (ScanResp last-key consumed count scanned items)
                      (parse-fail resp))))
              (parse-fail resp)))
        (illformed-response (string-append "Unparsable response from in scanning " table)))))

