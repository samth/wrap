;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Amazon API Library
;; Copyright (C) 2007-2013  Raymond Paul Racine
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
 aws-auth-str aws-auth-mac aws-auth-mac-encode
 ddb-request-signature)

(require/typed srfi/13
               (string-trim-both (String -> String)))

(require
 (only-in racket/pretty
          pretty-print)
 (only-in "../prelude/text/util.rkt"
          weave-string-separator)
 (only-in "../crypto/base64.rkt"
          base64-encode)
 (only-in "../crypto/hmac.rkt"
          hmac-sha1 hmac-sha256)
 (only-in "../httpclient/uri/url/encode.rkt"
          url-encode-string)
 (only-in "../httpclient/uri/url/param.rkt"
          params->query Param Params))

(: ddb-base String)
(define ddb-base "POST\n/\n\n")

(: ddb-merge-value (String String -> String))
(define (ddb-merge-value new-value curr-values)
  (if (string=? curr-values "")
      new-value
      (string-append curr-values "," new-value)))

(: ddb-merge-params (Params -> Params))
(define (ddb-merge-params params)
  (let: ((merged : (HashTable String String) (make-hash)))
    (let: loop : Params ((params : Params params))
      (if (null? params)
          (hash->list merged)
          (let ((param (car params)))
            (hash-update! merged (car param) 
                          (lambda: ((curr-value : String))
                            (ddb-merge-value (cdr param) curr-value))
                          (lambda () ""))
            (loop (cdr params)))))))

(: ddb-canonicalize-headers (Params -> String))
(define (ddb-canonicalize-headers params)
  
  (: lower-case-key (Param -> Param))
  (define (lower-case-key param)
    (cons (string-downcase (car param)) (cdr param)))
  
  (let ((lparams (map lower-case-key params)))
    (params->query lparams)))

;; DynamoDB Auth String to Sign
(: ddb-auth-str ((Listof (Pair String String)) String -> String))
(define (ddb-auth-str params body)
  (string-append ddb-base (ddb-canonicalize-headers params) body))

(: ddb-auth-mac-encode (String String -> String))
(define (ddb-auth-mac-encode key str)
  (url-encode-string (string-trim-both (base64-encode (hmac-sha256 (string->bytes/utf-8 key)
                                                                   (string->bytes/utf-8 str))))
                     #f))

(: ddb-request-signature (String (Listof (Pair String String)) String -> String))
(define (ddb-request-signature key params body)
  (ddb-auth-mac-encode key (ddb-auth-str params body)))

(: aws-auth-str (String String String String (Listof String) String -> String))
(define (aws-auth-str verb md5 mime expiration amz-headers resource)
  (let ((sep "\n"))
    (if (null? amz-headers)
        (weave-string-separator sep (list verb md5 mime expiration resource))
        (weave-string-separator sep (list verb md5 mime expiration 
                                          (weave-string-separator sep amz-headers) resource)))))

(: aws-auth-mac (String String -> String))
(define (aws-auth-mac key str)
  (string-trim-both (base64-encode (hmac-sha1 (string->bytes/utf-8 key)
                                              (string->bytes/utf-8 str)))))

(: aws-auth-mac-encode (String String -> String))
(define (aws-auth-mac-encode key str)
  (url-encode-string (aws-auth-mac key str) #f))
