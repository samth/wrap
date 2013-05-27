#lang typed/racket/base

(provide 
 update-item UpdateItemResp)

(require
 racket/pretty
 (only-in "../../format/json/tjson.rkt"
          Json JsObject jsobject json->string)
 (only-in "action.rkt"
          UPDATE-ITEM)
 (only-in "types.rkt"
          Action action->string ddbtype-symbol
          KeyVal Item 
          ItemVal ItemVal-value ItemVal-type
          ItemUpdate ItemUpdate-name ItemUpdate-action ItemUpdate-value 
          ItemKey Exists ReturnValues)
 (only-in "invoke.rkt"
          dynamodb)
 (only-in "request.rkt"
          return-values-json item-json itemkey-json))

(struct: UpdateItemResp ())

(: itemupdate-json (ItemUpdate -> Json))
(define (itemupdate-json item)
  (let ((attrs `((Action . ,(action->string (ItemUpdate-action item))))))
    (let ((value (ItemUpdate-value item)))
      (if value
          (jsobject (cons `(Value . ,(jsobject `((,(ddbtype-symbol (ItemVal-type value)) . ,(ItemVal-value value))))) attrs))
          (jsobject attrs)))))

(: itemupdates-json ((Listof ItemUpdate) -> JsObject))
(define (itemupdates-json updates)
  (jsobject ((inst map (Pairof Symbol Json) ItemUpdate)
             (lambda: ((item : ItemUpdate))
               `(,(string->symbol (ItemUpdate-name item)) . ,(itemupdate-json item)))
             updates)))

(: update-item-request (String ItemKey (Option (U Exists Item)) (Listof ItemUpdate) ReturnValues -> String))
(define (update-item-request table item-key expected attrs return-values)
  (let ((req (jsobject `((TableName . , table)
                         (Key . ,(itemkey-json item-key))
                         (AttributeUpdates . ,(itemupdates-json attrs))
                         (ReturnValues . ,(return-values-json return-values))))))
    (json->string req)))

(: update-item (String ItemKey (Option (U Exists Item)) (Listof ItemUpdate) ReturnValues -> UpdateItemResp))
(define (update-item table item-key expected attrs return-values)
  (let ((req (update-item-request table item-key expected attrs return-values)))
    (let ((resp (dynamodb UPDATE-ITEM req)))
      (pretty-print resp)))
  (UpdateItemResp))

;; {"TableName":"Table1",
;;     "Key":
;;         {"HashKeyElement":{"S":"AttributeValue1"},
;;         "RangeKeyElement":{"N":"AttributeValue2"}},
;;     "AttributeUpdates":{"AttributeName3":{"Value":{"S":"AttributeValue3_New"},"Action":"PUT"}},
;;     "Expected":{"AttributeName3":{"Value":{"S":"AttributeValue3_Current"}}},
;;     "ReturnValues":"ReturnValuesConstant"
;; }