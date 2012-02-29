#lang typed/racket/base

(provide
 parse-session-response)

(require
 racket/pretty 
 (only-in (planet rpr/prelude:1/type/date)
	  date->time-utc iso-8601-date-string->date)
 (only-in (planet rpr/format:1/xml/sxml)
	  Sxml SXPath sxpath extract-text extract-integer)
 (only-in "../credential.rkt"
	  SessionCredential current-aws-credential)
 (only-in "config.rkt"
	  sts-ns))

(: mk-sxpath (String -> SXPath))
(define mk-sxpath
  (let ((sts-nss  `((sts . ,sts-ns))))
    (lambda (path)
      (sxpath path sts-nss))))

(define sx-session-result-creds 
  (mk-sxpath "/sts:GetSessionTokenResponse/sts:GetSessionTokenResult/sts:Credentials"))
(define sx-session-token        (mk-sxpath "/sts:SessionToken/text()"))
(define sx-session-access-key   (mk-sxpath "/sts:AccessKeyId/text()"))
(define sx-session-secret-key   (mk-sxpath "/sts:SecretAccessKey/text()"))
(define sx-session-expiration   (mk-sxpath "/sts:Expiration/text()"))

(: parse-session-response (Sxml -> SessionCredential))
(define (parse-session-response sxml)
  (let ((result (sx-session-result-creds sxml)))
    (if (pair? result)
	(let ((token (extract-text (sx-session-token result)))
	      (access (extract-text (sx-session-access-key result)))
	      (secret (extract-text (sx-session-secret-key result)))
	      (expiration (date->time-utc (iso-8601-date-string->date (extract-text (sx-session-expiration result))))))
	  (SessionCredential access secret token expiration))
	(error "Failed to obtains session token"))))
