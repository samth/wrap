#lang typed/racket/base

(provide
 authv2-signature)

(require
 (only-in type/opt
	  opt-get-orelse-value)
 (only-in type/date
	  current-date-string-iso-8601)
 (only-in type/text
	  weave-string-separator)
 (only-in net/uri/url/url
	  QParam QParams QParam-name
	  add-qparam merge-qparams qparams->string)
 (only-in httpclient/encode
	  url-encode-string)
 (only-in crypto/base64
	  base64-encode)
 (only-in crypto/hmac
	  hmac-sha256)
 (only-in "../credential.rkt"
	  BaseCredential-secret-key BaseCredential-access-key current-aws-credential))

(: sep String)
(define sep "\n")

(: sig-version String)
(define sig-version "2")

(: sig-method String)
(define sig-method "HmacSHA256")

(: access-key String)
(define access-key "AWSAccessKeyId")

(: sig-version-parm QParam)
(define sig-version-parm (QParam "SignatureVersion" sig-version))

(: sig-method-parm QParam)
(define sig-method-parm (QParam "SignatureMethod" sig-method))

(: access-key-parm (String -> QParam))
(define (access-key-parm key)
  (QParam access-key key))

(: param-sort (QParam QParam -> Boolean))
(define (param-sort p1 p2)
  (string<=? (QParam-name p1) (QParam-name p2)))

(: fixed-params QParams)
(define fixed-params
  (list sig-version-parm
	sig-method-parm))

(: authv2-params (String String String -> QParams))
(define (authv2-params access-key cmd api-version)
  (cons (access-key-parm access-key)
	(cons (QParam "Action" cmd)
	      (cons (QParam "Version" api-version)
		    (cons (QParam "Timestamp" (url-encode-string (current-date-string-iso-8601 #t) #f))
			  fixed-params)))))

(: auth-str (String String String QParams -> String))
(define (auth-str http-action host path qparams)
  (let ((qstr (opt-get-orelse-value  (qparams->string (sort qparams param-sort)) "")))
    (string-append (weave-string-separator sep  (list http-action host path qstr)))))

(: signature (String String -> String))
(define (signature secret-key signee)
  (url-encode-string (base64-encode (hmac-sha256 secret-key signee)) #f))

(: authv2-signature (String String String String String QParams -> QParams))
(define (authv2-signature api-version action host cmd path qparams)
  (let ((access-key (BaseCredential-access-key (current-aws-credential)))
	(secret-key (BaseCredential-secret-key (current-aws-credential))))
    (let ((params (merge-qparams qparams (authv2-params access-key cmd api-version))))
      (add-qparam (QParam "Signature" (signature secret-key (auth-str action host path qparams)))
		  qparams))))
