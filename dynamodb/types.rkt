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
 Operator operator->string
 Throughput Throughput? Throughput-read Throughput-write
 Action action->string
 Exists Exists? Exists-name Exists-exists
 Key Key? Key-name Key-type
 KeyVal KeyVal? KeyVal-value KeyVal-type
 KeySchema KeySchema? KeySchema-hash-key KeySchema-range-key
 Item Item? Item-name Item-value Item-type
 ItemUpdate ItemUpdate? ItemUpdate-name ItemUpdate-action ItemUpdate-value
 ItemKey ItemKey? ItemKey-hashkey ItemKey-rangekey
 ItemVal ItemVal? ItemVal-value ItemVal-type
 ddbtype-code ddbtype-symbol string->DDBType DDBType DDBType? 
 TableStatus TableStatus? string->TableStatus
 ReturnValues)

(define-type DDBType (U 'String 'Number 'StringSet 'NumberSet))

(define-type Operator (U 'EQ 'LE 'LT 'GE 'GT 'BEGINS_WITH 'BETWEEN 'NULL 'NOT_NULL))

(define-predicate DDBType? DDBType)

(define-type ReturnValues (U 'None 'AllOld 'AllNew 'UpdatedOld 'UpdatedNew))

(: operator->string (Operator -> String))
(define (operator->string op)
  (symbol->string op))

(: ddbtype-code (DDBType -> String))
(define (ddbtype-code type)
  (case type
    ((String) "S")
    ((Number) "N")
    ((StringSet) "SS")
    ((NumberSet) "NS")))

(: ddbtype-symbol (DDBType -> Symbol))
(define (ddbtype-symbol type)
  (case type
    ((String)    'S)
    ((Number)    'N)
    ((StringSet) 'SS)
    ((NumberSet) 'NS)))

(: string->DDBType (String -> (Option DDBType)))
(define (string->DDBType str)
  (cond
   ((string=? "S" str)
    'String)
   ((string=? "N" str)
    'Number)
   ((string=? "SS" str)
    'StringSet)
   ((string=? "NS" str)
    'NumberSet)
   (else #f)))

(define-type TableStatus (U 'Active 'Deleting 'Creating))

(define-predicate TableStatus? TableStatus)

(: string->TableStatus (String -> (Option TableStatus)))
(define (string->TableStatus str)
  (cond 
   ((string=? str "ACTIVE") 'Active)
   ((string=? str "DELETING") 'Deleting)
   ((string=? str "CREATING") 'Creating)
   (else #f)))

(struct: KeySchema ([hash-key : Key]
		    [range-key : (Option Key)]) #:transparent)

(struct: Throughput ([read : Natural] 
		     [write : Natural]) #:transparent)

(define-type Action (U 'PUT 'ADD 'DELETE))

(: action->string (Action -> String))
(define (action->string action)
  (symbol->string action))

(struct: Exists ([name : String] [exists : Boolean]) #:transparent)

(struct: Key ([name : String]
	      [type : DDBType]) #:transparent)

(struct: KeyVal ([value : String] [type : DDBType]) #:transparent)

(struct: ItemVal ([value : String] [type : DDBType]) #:transparent)

(struct: Item ([name : String] [value : String] [type : DDBType]) #:transparent)

(struct: ItemKey ([hashkey : KeyVal]
		  [rangekey : (Option KeyVal)]) #:transparent)

(struct: ItemUpdate  ([name : String] [value : (Option ItemVal)] [action : Action]) #:transparent)

