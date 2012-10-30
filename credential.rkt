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

;; Load AWS credentials from a protected file.

#lang typed/racket/base

(provide
 add-session-credential
 current-aws-credential
 set-aws-credential!
 init-aws-credential
 AwsCredential
 AwsCredential?
 AwsCredential-account-id
 BaseCredential-access-key
 BaseCredential-secret-key
 AwsCredential-session
 SessionCredential
 SessionCredential?
 SessionCredential-token
 SessionCredential-expiration
 AwsCredential-associate-tag)

(require/typed racket/base
	       ((read read-creds) (Input-Port -> (Listof (Pair Symbol String)))))
 
(require 
 (only-in "../prelude/std/opt.rkt"
          opt-apply-orelse)
 (only-in "../prelude/type/date.rkt"
          Time))

(struct: BaseCredential ((access-key : String)
			 (secret-key : String)) #:transparent)

(struct: SessionCredential BaseCredential
  ([token      : String]
   [expiration : Time]) #:transparent)

(struct: AwsCredential BaseCredential ([account-id    : String]
                                       [associate-tag : String]
                                       [session       : (Option SessionCredential)]) #:mutable #:transparent)

;; struct-copy is broken in TR
(: add-session-credential (SessionCredential -> AwsCredential))
(define (add-session-credential session-cred)
  (struct-copy AwsCredential (current-aws-credential) [session session-cred])) ;; BROKEN IN TypedRacket.

(: default-cred-path Path)
(define default-cred-path
  (build-path (find-system-path 'home-dir) ".awscreds.sexp"))

(: set-aws-credential! (AwsCredential -> Void))
(define (set-aws-credential! cred)
  (set! current-aws-credential (make-parameter cred)))

(: init-aws-credential (Path -> AwsCredential))
(define (init-aws-credential path)
  (load-credential path))

(: load-credential (Path -> AwsCredential))
(define (load-credential fpath)
  
  (define lookup (inst assoc Symbol String))
  (define value  (inst cdr Symbol String))

  (: cred-value (Symbol (Listof (Pair Symbol String)) -> String))
  (define (cred-value sym props)
    (opt-apply-orelse  (lookup sym props) value  ""))
  
  (call-with-input-file fpath
    (lambda: ((ip : Input-Port))
      (let: ((props : (Listof (Pair Symbol String))(read-creds ip)))
	(AwsCredential (cred-value 'access-key props)
		       (cred-value 'secret-key props)
		       (cred-value 'account-id props)
		       (cred-value 'associate-tag props)
		       #f)))))

(: current-aws-credential (Parameterof AwsCredential))
(define current-aws-credential (make-parameter (init-aws-credential default-cred-path)))
