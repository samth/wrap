;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Knozama's Amazon API Library
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
 describe-table
 DescribeTableResp DescribeTableResp?)

(require
 racket/pretty
 (only-in (planet rpr/format:1/json/tjson)
	  Json JsObject JsObject? json->string jsobject)
 (only-in "action.rkt"
	  DESCRIBE-TABLE)
 (only-in "invoke.rkt"
	  dynamodb)
 (only-in "parse.rkt"
	  parse-capacity parse-key-schema
	  invalid-error attr-value)
 (only-in "types.rkt"
	  TableStatus TableStatus? string->TableStatus
	  KeySchema Throughput Throughput? DDBType? Key)
 (only-in "error.rkt"
	  DDBFailure DDBFailure?))

;; Some values are optional and/or set to 0 to support when a table is state transitioning i.e. deleting.
(struct: DescribeTableResp ([name : String]
			    [schema : (Option KeySchema)]
			    [size : Integer]
			    [item-cnt : Integer]
			    [creation : Float]
			    [status : TableStatus]
			    [capacity : Throughput]) #:transparent)

(: describe-table (String -> (U DDBFailure DescribeTableResp)))
(define (describe-table name)
   (let ((result (dynamodb DESCRIBE-TABLE (format "{\"TableName\": ~s}" name))))
     (if (DDBFailure? result)
	 result
	 (parse-describe-table-resp result))))

(: parse-describe-table-resp (Json -> DescribeTableResp))
(define (parse-describe-table-resp resp)
  (if (JsObject? resp)      
      (let ((table (attr-value resp 'Table JsObject?)))
	(let ((size (if (hash-has-key? table 'TableSizeBytes)
			(attr-value table 'TableSizeBytes exact-integer?)
			0))
	      (name (attr-value table 'TableName string?))
	      (item-cnt (if (hash-has-key? table 'ItemCount)			    
			    (attr-value  table 'ItemCount exact-integer?)
			    0))
	      (creation (if (hash-has-key? table 'CreationDateTime)
			    (attr-value table 'CreationDateTime flonum?)
			    0.0))
	      (status (let ((status (string->TableStatus (attr-value table 'TableStatus string?))))
			(if status status (invalid-error 'TablesStatus resp))))
	      (capacity (parse-capacity (attr-value table 'ProvisionedThroughput JsObject?)))
	      (schema (if (hash-has-key? table 'KeySchema)
			  (parse-key-schema (attr-value table 'KeySchema JsObject?))
			  #f)))
	  (DescribeTableResp name schema size item-cnt creation status capacity)))
      (invalid-error 'DescribeTableResp resp)))
