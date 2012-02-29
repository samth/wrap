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
 LIST-TABLES CREATE-TABLE DESCRIBE-TABLE DELETE-TABLE 
 PUT-ITEM GET-ITEM DELETE-ITEM UPDATE-ITEM SCAN)

(require 
 (only-in "config.rkt"
	  ddb-version))

(: action (String -> String))
(define (action cmd)
  (string-append ddb-version "." cmd))

(: SCAN String)
(define SCAN
  (action "Scan"))

(: CREATE-TABLE String)
(define CREATE-TABLE
  (action "CreateTable"))

(: LIST-TABLES String)
(define LIST-TABLES
  (action "ListTables"))

(: DELETE-TABLE String)
(define DELETE-TABLE
  (action "DeleteTable"))

(: DESCRIBE-TABLE String)
(define DESCRIBE-TABLE
  (action "DescribeTable"))

(: PUT-ITEM String)
(define PUT-ITEM
  (action "PutItem"))

(: GET-ITEM String)
(define GET-ITEM 
  (action "GetItem"))

(: DELETE-ITEM String)
(define DELETE-ITEM
  (action "DeleteItem"))

(: UPDATE-ITEM String)
(define UPDATE-ITEM
  (action "UpdateItem"))
