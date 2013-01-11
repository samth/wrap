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

#lang typed/racket/base

(provide 
 (struct-out Range)
 (struct-out Buckets)
 (struct-out Owner)
 (struct-out Bucket)
 (struct-out Key)
 (struct-out Keys)
 (struct-out Prefix))

(struct: Buckets
	 ([owner : Owner]
	  [buckets : (Listof Bucket)]) #:transparent)

(struct: Owner 
	 ([id : String]
	  [name : String]) #:transparent)

(struct: Bucket
	 ([name : String]
	  [creation-date : String]) #:transparent)

(struct: Key
	 ([key : String]
	  [last-modified : String]
	  ;; [storage-class : String]
	  [etag : String]
	  [size : Integer]
	  [owner : Owner]) #:transparent)

(struct: Prefix ([prefix : String]) #:transparent)

(struct: Keys
  ([name : String]
   [prefix : String]
   [marker : String]
   [max-keys : Integer]
   [truncated? : Boolean]
   [prefixes : (Listof Prefix)]
   [objects : (Listof Key)]) #:transparent)

(struct: Range ([from : Natural] [to : Natural]))
