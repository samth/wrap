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

(require net/head
         net/url
         xml/xml)

(provide s3-response? 
         s3-response-close
         s3-response-from-port  
         s3-response-debug-dump
         s3-http-header
         s3-http-code
         s3-http-version
         s3-http-message
         s3-http-response-fields
         s3-http-response-field)

(define-struct s3-response (http port))

(define (s3-response-from-port ip)
  (make-s3-response (purify-port ip) ip))

(define (s3-response-close s3-response)
  (close-input-port (s3-response-port s3-response)))

(define (s3-http-response-fields s3-response)
  (extract-all-fields (s3-response-http s3-response)))

(define (s3-http-response-field field-name-str s3-response)
  (extract-field field-name-str (s3-response-http s3-response)))

(define (header-extract s3-response regexp)
  (cadr (regexp-match regexp (s3-response-http s3-response))))

;;(define response-test "HTTP/1.1 403 Forbidden\r\n\r\n")

(define http-response-header-regex #rx"(HTTP)/([0-9][.][0-9]) ([0-9]+) (.+$)")

(define (s3-response-headers response)
  (s3-response-http response))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s3-response -> string
;; response -> "HTTP/1.1 403 Forbidden"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (s3-http-header s3-response)
  (header-extract s3-response #rx"(^HTTP/.*?)\r\n\r\n|\n\n|\r\r"))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Give a standard HTTP response string extract the code
;; "HTTP/1.1 403 Forbidden" -> 403
;; string -> number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (s3-http-code s3-response)
  (string->number (header-extract s3-response #rx"HTTP/[0-9][.][0-9] ([0-9]+) ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "HTTP/1.1 403 Forbidden" -> "1.1"
;; s3-response? -> string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (s3-http-version s3-response)
  (header-extract s3-response #rx"HTTP/([0-9][.][0-9]) "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "HTTP/1.1 403 Forbidden" -> "Forbidden"
;; s3-response? -> string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (s3-http-message s3-response)
  (header-extract s3-response #rx"HTTP/[0-9][.][0-9] [0-9]+ (.+$)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s3-response -> void
;; Dump out a response to the console
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (s3-response-debug-dump s3-response)
  (display "---------S3 Response---------")(newline)
  (display (s3-response-http s3-response))
  (display "---------Payload-------------")(newline)
  (let ([inport (s3-response-port s3-response)])
    (if (equal? (s3-http-response-field "Content-Type" s3-response) "application/xml")
        (display-xml (read-xml inport))
        (display-pure-port inport))
    (close-input-port inport)
    (newline)(display "---------End Payload---------")(newline)))
