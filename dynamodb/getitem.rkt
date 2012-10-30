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
 get-item GetItemResp GetItemResp? GetItemResp-items GetItemResp-consumed)

(require
 (only-in "../../format/json/tjson.rkt"
          JsObject-empty
          Json JsObject JsObject? json->string string->json jsobject
          jsobject-add-attribute jsobject-remove-attribute)
 (only-in "../../prelude/std/opt.rkt"
          opt-orelse)
 (only-in "types.rkt"
          ddbtype-symbol DDBType
          Item
          Key Key? Key-name Key-type
          KeyVal KeyVal? KeyVal-value KeyVal-type
          ItemKey)
 (only-in "action.rkt"
          GET-ITEM)
 (only-in "invoke.rkt"
          dynamodb)
 (only-in "request.rkt"
          itemkey-json)
 (only-in "response.rkt"
          parse-consumed-capacity
          parse-fail
          parse-items))

(struct: GetItemResp ([items : (HashTable String Item)] [consumed : Float]) #:transparent)

(: get-item-request (String ItemKey (Listof String) Boolean -> String))
(define (get-item-request table key attrs consistent?)
  (let ((req (jsobject `((TableName . ,table)
			 (Key . ,(itemkey-json key))
			 (AttributesToGet . ,attrs)
			 (ConsistentRead . ,(if consistent? "true" "false"))))))
    (when (null? attrs)
      (jsobject-remove-attribute req 'AttributesToGet))
    (json->string req)))

(: get-item  (String ItemKey (Listof String) Boolean -> GetItemResp))
(define (get-item table item-key attrs consistent?)
  (let ((req (get-item-request table item-key attrs consistent?)))
    (let ((resp (dynamodb GET-ITEM req)))
      (if (JsObject? resp)
	  (parse-get-item-resp resp)
	  (error "Invalid response ~a" resp)))))

;;; Parse Response

(: parse-get-item-resp (JsObject -> GetItemResp))
(define (parse-get-item-resp resp)
  (if (hash-has-key? resp 'ConsumedCapacityUnits)
      (let ((jconsumed (parse-consumed-capacity resp)))
	(let ((items (hash-ref resp 'Item (lambda: () JsObject-empty))))
	  (if (JsObject? items)
	      (GetItemResp (parse-items items) jconsumed)
	      (parse-fail resp))))
      (parse-fail resp)))

