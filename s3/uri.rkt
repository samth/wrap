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

#| Helpers for S3 URI's, extracting buckets etc.|#

(provide
 s3-path->prefix
 s3-uri-bucket-and-path)

(require 
 (only-in httpclient/uri
          Uri Uri-scheme Uri-path)
 (only-in httpclient/uri/path
          uri-path-split uri-build-path))

(: s3-path->prefix (String -> String))
(define (s3-path->prefix path)
  (if (string=? path "")
      ""
      (if (string=? (substring path 0 1) "/")
          (substring path 1)
          path)))

(: s3-uri-bucket-and-path (Uri -> (Values String String)))
(define (s3-uri-bucket-and-path s3-uri)  
  (let ((scheme (Uri-scheme s3-uri)))
    (if (string=? scheme "s3")
        (let ((path-segs (uri-path-split (Uri-path s3-uri))))          
          (if (pair? path-segs)
              (if (string=? (car path-segs) "")
                  (let ((rootless-segs (cdr path-segs)))
                    (if (pair? rootless-segs)                        
                        (values (car rootless-segs)
                                (uri-build-path (cons "" (cdr rootless-segs))))
                        (values "" "")))
                  (values "" ""))
              (values "" "")))
        (values "" ""))))


