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

(provide create-table CreateTableResp)

(require 
 racket/pretty
 (only-in "../../format/json/tjson.rkt"
          Json JsObject jsobject json->string) 
 (only-in "types.rkt"
          Throughput-write Throughput-read Throughput Throughput?
          DDBType ddbtype-code)
 (only-in "error.rkt"
          AWSFailure)          
 (only-in "action.rkt"
          CREATE-TABLE)
 (only-in "types.rkt"
          TableStatus string->TableStatus
          Throughput KeySchema
          Key Key? Key-name Key-type)
 (only-in "invoke.rkt"
          dynamodb)
 (only-in "parse.rkt"
          invalid-error parse-capacity
          attr-value attr-value-jsobject parse-key-schema))

(struct: CreateTableResp ([name : String]
                          [status : TableStatus]
                          [creation : Float]
                          [capacity : Throughput]
                          [schema : KeySchema]) #:transparent)

(: create-request (String Key (Option Key) Throughput -> String))
(define (create-request name hash-key range-key throughput)
  
  (: keys-json (Key (Option Key) -> JsObject))
  (define (keys-json hash-key range-key)
    
    (: key-json (Key -> JsObject))
    (define (key-json key)
      (jsobject `((AttributeName . ,(Key-name key))
                  (AttributeType . ,(ddbtype-code (Key-type key))))))
    
    (let: ((keys : JsObject (jsobject '())))
      (hash-set! keys 'HashKeyElement (key-json hash-key))
      (when range-key
        (hash-set! keys 'RangeKeyElement (key-json range-key)))
      keys))
  
  (: throughput-json (Throughput -> JsObject))
  (define (throughput-json throughput)
    (jsobject `((ReadCapacityUnits . ,(Throughput-read throughput))
                (WriteCapacityUnits . ,(Throughput-write throughput)))))
  
  (json->string (jsobject `((TableName . ,name)
                            (KeySchema . ,(keys-json hash-key range-key))
                            (ProvisionedThroughput . ,(throughput-json throughput))))))

(: create-table (String Key (Option Key) Throughput -> CreateTableResp))
(define (create-table name hash-key range-key throughput) 
  (let ((result (dynamodb CREATE-TABLE (create-request name hash-key range-key throughput))))    
    (parse-create-table-resp result)))

(: parse-create-table-resp (Json -> CreateTableResp))
(define (parse-create-table-resp resp)
  (if (hash? resp)
      (let: ((resp : JsObject (cast resp JsObject)))
        (let ((desc (attr-value-jsobject resp 'TableDescription)))
          (let ((name (attr-value desc 'TableName string?))
                (schema (parse-key-schema (attr-value-jsobject desc 'KeySchema)))
                (creation (attr-value desc 'CreationDateTime flonum?))
                (capacity (parse-capacity (attr-value-jsobject desc 'ProvisionedThroughput)))
                (status (let ((status (string->TableStatus (attr-value desc 'TableStatus string?))))
                          (if status status (invalid-error 'TablesStatus desc)))))
            (CreateTableResp name status creation capacity schema))))
      (raise (invalid-error 'TableDescription resp))))
