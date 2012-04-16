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
 delete-table
 DeleteTableResp DeleteTableResp?)

(require
 racket/pretty
 (only-in (planet rpr/format:1/json/tjson)
	  Json JsObject JsObject? json->string jsobject)
 (only-in "types.rkt"
	  Throughput
	  TableStatus string->TableStatus)
 (only-in "error.rkt"
	  DDBFailure DDBFailure?)
 (only-in "action.rkt"
	  DELETE-TABLE)
 (only-in "invoke.rkt"
	  dynamodb)
 (only-in "parse.rkt"
	  invalid-error attr-value parse-capacity))

(struct: DeleteTableResp ([name : String]
			  [status : TableStatus]
			  [capacity : Throughput]) #:transparent)

(: delete-table (String -> DeleteTableResp))
(define (delete-table name)
  (let ((result (dynamodb DELETE-TABLE (format "{\"TableName\": ~s}" name))))    
    (parse-delete-table-resp result)))

(: parse-delete-table-resp (Json -> DeleteTableResp))
(define (parse-delete-table-resp resp)
  (if (JsObject? resp)
      (let ((desc (attr-value resp 'TableDescription JsObject?)))
        (let ((status (let ((status (string->TableStatus (attr-value desc 'TableStatus string?))))
                        (if status status (invalid-error 'TableStatus resp))))
              (name (attr-value desc 'TableName string?))
              (capacity (parse-capacity (attr-value desc 'ProvisionedThroughput JsObject?))))
          (DeleteTableResp name status capacity)))
      (raise (invalid-error 'DeleteTable resp))))
	  
