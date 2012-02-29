#lang typed/racket/base

(require
 (only-in "types.rkt"
	  Operator ItemKey))

;; This is wrong, not the full ItemKey, just the Hash-Key ??/
;;(: query (String (Listof String) Exact-Postive-Integer Boolean Boolean ItemKey 
(define (query table attrs limit consistent? count? item-key conditions scan-forward? exclusive-start-key)
  (void))
