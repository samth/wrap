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

(require
 (only-in "dynamodb.rkt"
	  create-table delete-table describe-table list-tables 
	  get-item put-item 
	  Item Key ItemKey KeyVal
	  ReturnValues Throughput))

(define table "prodtest")

(define (create)
  (create-table table (Key "sku" 'String) #f (Throughput 3 5)))

(define (describe)
  (describe-table table))

(define (add)
  (put-item table (list (Item "color" "red" 'String)
			(Item "price" "1.99" 'Number)
			(Item "sku" "315515" 'String))
	    #f 'AllOld))

(define (add0)
  (put-item table (list (Item "color" "blue" 'String)
			(Item "price" "4.99" 'Number)
			(Item "sku" "315515" 'String))
	    #f 'AllOld))

(define (add2)
  (put-item table (list (Item "color" "blue" 'String)
			(Item "price" "5.00" 'Number)
			(Item "sku" "123456" 'String))
	    #f 'AllOld))

(define (get)
  (get-item table (ItemKey (KeyVal "315515" 'String) #f) '("sku" "price" "color") #f))

(define (get2)
  (get-item table (ItemKey (KeyVal "123456" 'String) #f) '("sku" "price") #f))

(define (delete)
  (delete-table table))

(define (dir)
  (list-tables #f 10))

