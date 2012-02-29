#lang typed/racket/base

;; Common functions to build a request

(provide 
 item-json items-json
 expected/exists-json
 ;;expected-json exists-json
 return-values-json
 itemkey-json)

(require
 (only-in (planet knozama/webkit:1/formats/tjson)
 	  Json JsObject JsObject? json->string string->json 
	  jsobject jsobject-add-attribute)
 (only-in "types.rkt"
	  Exists Exists? Exists-name Exists-exists
	  Item Item? Item-name Item-type Item-value
	  ddbtype-symbol
	  ReturnValues
	  KeyVal KeyVal-type KeyVal-value
	  ItemKey ItemKey-hashkey ItemKey-rangekey))

(: keyvalue-json (KeyVal -> JsObject))
(define (keyvalue-json keyval)
  (jsobject `((,(ddbtype-symbol (KeyVal-type keyval)) . ,(KeyVal-value keyval)))))

(: itemkey-json (ItemKey -> JsObject))
(define (itemkey-json item-key)
  (let ((jsobj (jsobject `((HashKeyElement . ,(keyvalue-json (ItemKey-hashkey item-key)))))))
    (let ((rnge-key (ItemKey-rangekey item-key)))
      (when rnge-key 
	(jsobject-add-attribute jsobj 'RangeKeyElement (keyvalue-json rnge-key))))
    jsobj))

(: return-values-json (ReturnValues -> String))
(define (return-values-json rtnval)
  (case rtnval
    ((None)       "NONE")
    ((AllOld)     "ALL_OLD")
    ((AllNew)     "ALL_NEW")
    ((UpdatedNew) "UPDATED_NEW")
    ((UpdatedOld) "UPDATED_OLD")))

(: expected-json (Item -> JsObject))
(define (expected-json expected)
  (jsobject (list (item-json expected))))

(: exists-json (Exists -> JsObject))
(define (exists-json exists)
  (jsobject `((,(string->symbol (Exists-name exists)) . ,(jsobject `((Exists . ,(Exists-exists exists))))))))

(: item-json (Item -> (Pair Symbol JsObject)))
(define (item-json item)
  (cons (string->symbol (Item-name item))
	(jsobject `((,(ddbtype-symbol (Item-type item)) . ,(Item-value item))))))

(: expected/exists-json ((U Exists Item) -> JsObject))
(define (expected/exists-json expected)
  (cond 
   ((Item? expected)   (expected-json expected))
   ((Exists? expected) (exists-json expected))))

(: items-json ((Listof Item) -> JsObject))
(define (items-json items)
  (jsobject (map item-json items)))
