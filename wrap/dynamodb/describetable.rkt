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
 (only-in "../../format/json/tjson.rkt"
          Json JsObject json->string jsobject)
 (only-in "action.rkt"
          DESCRIBE-TABLE)
 (only-in "invoke.rkt"
          dynamodb)
 (only-in "parse.rkt"
          parse-capacity parse-key-schema
          invalid-error attr-value attr-value-jsobject)
 (only-in "types.rkt"
          TableStatus TableStatus? string->TableStatus
          KeySchema Throughput Throughput? DDBType? Key)
 (only-in "error.rkt"
          AWSFailure))          

;; Some values are optional and/or set to 0 to support when a table is state transitioning i.e. deleting.
(struct: DescribeTableResp ([name : String]
                            [schema : (Option KeySchema)]
                            [size : Integer]
                            [item-cnt : Integer]
                            [creation : Float]
                            [status : TableStatus]
                            [capacity : Throughput]) #:transparent)

(: describe-table (String -> DescribeTableResp))
(define (describe-table name)
  (parse-describe-table-resp (dynamodb DESCRIBE-TABLE (format "{\"TableName\": ~s}" name))))

(: parse-describe-table-resp (Json -> DescribeTableResp))
(define (parse-describe-table-resp resp)
  (if (hash? resp)
      (let*: ((resp : JsObject (cast resp JsObject))
              (table (attr-value-jsobject resp 'Table)))
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
              (capacity (parse-capacity (attr-value-jsobject table 'ProvisionedThroughput)))
              (schema (if (hash-has-key? table 'KeySchema)
                          (parse-key-schema (attr-value-jsobject table 'KeySchema))
                          #f)))
          (DescribeTableResp name schema size item-cnt creation status capacity)))
      (invalid-error 'DescribeTableResp resp)))
