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
 (struct-out Filter)
 (struct-out Range)
 (struct-out Item)
 (struct-out ItemUpdate)
 (struct-out ItemKey)
 (struct-out ItemVal)
 (struct-out Key)
 (struct-out KeyVal)
 (struct-out KeySchema)
 (struct-out Exists)
 (struct-out Throughput)
 Operator operator->string
 Action action->string
 ddbtype-code ddbtype-symbol string->DDBType DDBType DDBType? 
 TableStatus TableStatus? string->TableStatus
 ReturnValues)

(define-type DDBType (U 'String 'Number 'StringSet 'NumberSet))

(define-type Operator (U 'EQ 'LE 'LT 'GE 'GT 'BEGINS_WITH 'BETWEEN 'NULL 'NOT-NULL))

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

(struct: Filter ([item : String]
                 [values : (Listof ItemVal)]
                 [operator : Operator]) #:transparent)

(struct: Range ([values : (Listof ItemVal)]
                [operator : Operator]) #:transparent)

(struct: Exists ([name : String] [exists : Boolean]) #:transparent)

(struct: Key ([name : String]
              [type : DDBType]) #:transparent)

(struct: KeyVal ([value : String] [type : DDBType]) #:transparent)

(struct: ItemVal ([value : (U (Listof String ) String)] [type : DDBType]) #:transparent)

(struct: Item ([name : String] [value : (U (Listof String) String)] [type : DDBType]) #:transparent)

(struct: ItemKey ([hashkey : KeyVal]
                  [rangekey : (Option KeyVal)]) #:transparent)

(struct: ItemUpdate  ([name : String] [value : (Option ItemVal)] [action : Action]) #:transparent)

