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
 s3-uri-path->prefix)

(require 
 (only-in httpclient/uri
          Uri Uri-scheme Uri-path)
 (only-in httpclient/uri/path
          uri-path-split uri-build-path))

(: s3-uri-path->prefix (String -> String))
(define (s3-uri-path->prefix path)
  (if (string=? path "")
      ""
      (if (string=? (substring path 0 1) "/")
          (substring path 1)
          path)))
