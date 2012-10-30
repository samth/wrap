#lang typed/racket/base

(provide 
 auth-signature)

(require
 racket/pretty
 (only-in "../../prelude/text/util.rkt"
          weave-string-separator)
 (only-in "../../httpclient/http/header.rkt"
          header->string)
 (only-in "../../httpclient/uri/url/encode.rkt"
          url-encode-string)
 (only-in "../../crypto/hash/sha256.rkt"
          sha256)
 (only-in "../../crypto/base64.rkt"
          base64-encode)
 (only-in "../../crypto/hmac.rkt"
          hmac-sha256)
 (only-in "../../httpclient/uri/url/param.rkt"
          param Param Params)
 (only-in "../credential.rkt"
	  AwsCredential-session BaseCredential-secret-key 
	  current-aws-credential))

(: filter-canonicalize-headers (Params -> Params))
(define (filter-canonicalize-headers params)

  (: lower-case-key (Param -> Param))
  (define (lower-case-key param)
    (cons (string-downcase (car param)) (cdr param)))

  (: x-amz-header? (Param -> Boolean))
  (define (x-amz-header? param)
    (let ((k (car param)))
      (and (>= (string-length k) 6)
	   (string=? (substring (car param) 0 6) "x-amz-"))))

  (: merge-value (String String -> String))
  (define (merge-value new-value curr-values)
    (if (string=? curr-values "")
	new-value
	(string-append curr-values "," new-value)))
  
  ;; one-pass loop as opposed to merge-filter-map 3 pass
  (let: ((merged : (HashTable String String) (make-hash)))
    (let: loop : Params ((params : Params params))
	  (if (null? params)
	      (hash->list merged)
	      (let ((lparam (lower-case-key (car params))))
		(when (x-amz-header? lparam)
		  (let ((k (car lparam))
			(v (cdr lparam)))
		    (hash-update! merged k 
				  (lambda: ((curr-value : String))
				    (merge-value v curr-value))
				  (lambda () ""))))
		(loop (cdr params)))))))

(: auth-signee (String Params String -> String))
(define (auth-signee host params body)
  (let ((hdrs (apply string-append (sort (map (lambda: ((param : Param))
						(string-append (header->string param) "\n"))
					      (cons (param "host" host) params))  string<=?))))
    (string-append "POST\n/\n\n" hdrs "\n" body)))

(: auth-signature (String Params String -> String))
(define (auth-signature host params body)
  (let ((scred (let ((scred (AwsCredential-session (current-aws-credential))))
		 (if scred scred (error "Missing session credentials")))))
    (let ((secret-key (BaseCredential-secret-key scred))
	  (signee (auth-signee host params body)))
      ;;(pretty-print signee)
      (base64-encode (hmac-sha256 secret-key (sha256 signee))))))
  
