#lang typed/racket/base

(provide
 ensure-session)

(require
 racket/pretty
 (only-in (planet rpr/prelude:1/type/date)
	  time< current-time)
 (only-in "../credential.rkt"
	  set-aws-credential! add-session-credential
	  current-aws-credential AwsCredential AwsCredential? AwsCredential-session 
	  SessionCredential SessionCredential? SessionCredential-expiration)
 (only-in "sts.rkt"
	  get-session-token))

(: expired-token? (SessionCredential -> Boolean))
(define (expired-token? creds)
  (let ((expiry (SessionCredential-expiration creds)))
    (time< expiry (current-time))))	

(: refresh-token (-> Boolean))
(define (refresh-token)
  (let ((tok (get-session-token 100)))
    (if (SessionCredential? tok)
	(begin
	  (set-aws-credential! (add-session-credential tok))
	  #t)
	#f)))

;; consider chaperone-procedure or make-derived-parameter
(: ensure-session (-> Boolean))
(define (ensure-session)
  (let ((stok (AwsCredential-session (current-aws-credential))))
    (if stok
	(if (expired-token? stok)
	    (refresh-token)
	    #t)
	(refresh-token))))
	    
