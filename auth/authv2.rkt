#lang typed/racket/base

(provide
 authv2-signature)

(require
 (only-in "../../prelude/type/date.rkt"
          current-date-string-iso-8601)
 (only-in "../../prelude/text/util.rkt"
          weave-string-separator)
 (only-in "../../httpclient/uri/url/param.rkt"
          param Param Params params->query)
 (only-in "../../httpclient/uri/url/encode.rkt"
          url-encode-string)
 (only-in "../../crypto/base64.rkt"
          base64-encode)
 (only-in "../../crypto/hmac.rkt"
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

(: sig-version-parm (Pairof String String))
(define sig-version-parm (param "SignatureVersion" sig-version))

(: sig-method-parm (Pairof String String))
(define sig-method-parm (param "SignatureMethod" sig-method))

(: access-key-parm (String -> (Pairof String String)))
(define (access-key-parm key)
  (cons access-key key))

(: param-sort ((Pair String String) (Pair String String) -> Boolean))
(define (param-sort p1 p2)
  (string<=? (car p1) (car p2)))

(: fixed-params Params)
(define fixed-params 
  (list sig-version-parm 
	sig-method-parm))

(: authv2-params (String String String -> Params))
(define (authv2-params access-key cmd api-version)
  (cons (access-key-parm access-key)
	(cons (param "Action" cmd)
	      (cons (param "Version" api-version)
		    (cons (param "Timestamp" (url-encode-string (current-date-string-iso-8601 #t) #f))
			  fixed-params)))))

(: auth-str (String String String Params -> String))
(define (auth-str http-action host path params)
  (let ((qstr (params->query (sort params param-sort))))
    (string-append (weave-string-separator sep  (list http-action host path qstr)))))

(: signature (String String -> String))
(define (signature secret-key signee)
  (url-encode-string (base64-encode (hmac-sha256 secret-key signee)) #f))

(: authv2-signature (String String String String String Params -> Params))
(define (authv2-signature api-version action host cmd path params)
  (let ((access-key (BaseCredential-access-key (current-aws-credential)))
	(secret-key (BaseCredential-secret-key (current-aws-credential))))
    (let ((params (append params (authv2-params access-key cmd api-version))))
      (cons (param "Signature" (signature secret-key (auth-str action host path params)))
	    params))))
