;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007-2011  Raymond Paul Racine
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
 list-tables ListTablesResp ListTablesResp?)

(require
 racket/pretty
 (only-in "action.rkt"
	  LIST-TABLES)
 (only-in "invoke.rkt"
	  dynamodb))

(struct: ListTablesResp ([names : (Listof String)]
			 [last : String]) #:transparent)

(: list-tables ((Option String) Natural -> ListTablesResp))
(define (list-tables start-from cnt)

  ;;(define cmd "DynamoDB_20111205.ListTables")
  (define cmd-body (format "{\"Limit\": ~s}" cnt))

  (pretty-print (dynamodb LIST-TABLES cmd-body))
  (ListTablesResp '() ""))
