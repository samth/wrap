#lang typed/racket/base

(provide
 delete-item DeleteItemResult)

(require
 racket/pretty
 (only-in "types.rkt" KeyVal
	  ItemKey Exists Item ReturnValues)
 (only-in "action.rkt"
	  DELETE-ITEM)
 (only-in "invoke.rkt"
	  dynamodb)
 (only-in (planet knozama/webkit:1/formats/tjson)
	  Json JsObject jsobject json->string)
 (only-in "request.rkt"
	  return-values-json itemkey-json))

(struct: DeleteItemResult () #:transparent)

(: delete-item-request (String ItemKey (Option (U Exists Item)) ReturnValues -> String))
(define (delete-item-request table key expected return-values)
  (let ((req (jsobject `((TableName . ,table)
			 (Key . ,(itemkey-json key))
			 (ReturnValues . ,(return-values-json return-values))))))
    (json->string req)))

(: delete-item (String ItemKey (Option (U Exists Item)) ReturnValues -> DeleteItemResult))
(define (delete-item table item-key expected return-values)
  (let ((req (delete-item-request table item-key expected return-values)))
    (let ((resp (dynamodb DELETE-ITEM req)))
      (pretty-print resp)))
  (DeleteItemResult))


;; (define (test)
;;  (delete-item-request "mytable" (ItemKey (KeyVal "315515" 'String) (KeyVal "20120101" 'String)) #f 'None))

;; {"TableName":"Table1",
;;     "Key":
;;         {"HashKeyElement":{"S":"AttributeValue1"},"RangeKeyElement":{"N":"AttributeValue2"}},
;;     "Expected":{"AttributeName3":{"Value":{"S":"AttributeValue3"}}},
;;     "ReturnValues":"ALL_OLD"}
;; }
