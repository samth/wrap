;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007,2008,2009,2010  Raymond Paul Racine
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
  
#lang racket/base

(provide string->hex weave)  

(define (string->hex s)
  (let ((len (string-length s)))
    (let loop ((i 0) (accum ""))
      (if (< i len)
	 (let ((hex (format "~x" (char->integer (string-ref s i)))))            
	   (loop (+ 1 i) (string-append hex " " accum)))
	 accum))))

(define (weave lst sep)    
  (if (null?  (cdr lst))
     (car lst)
     (string-append (car lst) sep (weave (cdr lst) sep))))

;  (define (bytes->hexstring bstr)
;    (let ((hex #(48 49 50 51 52 53 54 55 56 57 97 98 99 100 101 102))
;          (umask #b11110000)
;          (lmask #b00001111))      
;      (let loop ((hexbytes '()) (i (bytes-length bstr)))
;        (if (zero? i)
;            (bytes->string/utf-8 (apply bytes hexbytes))
;            (let ((b (bytes-ref bstr (- i 1))))
;              (let ((unibble (vector-ref hex (arithmetic-shift (bitwise-and b umask) -4)))
;                    (lnibble (vector-ref hex (bitwise-and b lmask))))                
;                (loop (cons unibble (cons lnibble hexbytes)) (- i 1))))))))
