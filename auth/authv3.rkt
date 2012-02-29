#lang typed/racket/base

(provide 
 auth-signature)

(require
 racket/pretty
 (only-in (planet knozama/common:1/text/util)
	  weave-string-separator)
 (only-in (planet knozama/webkit:1/web/http/header)
	  header->string)
 (only-in (planet knozama/webkit:1/web/uri)
	  url-encode-string)
 (only-in (planet knozama/webkit:1/crypto/hash/sha256)
	  sha256)
 (only-in (planet knozama/webkit:1/crypto/base64)
	  base64-encode)
 (only-in (planet knozama/webkit:1/crypto/hmac)
	  hmac-sha256)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  param Param Params)
 (only-in (planet knozama/aws:1/credential)
	  AwsCredential-session BaseCredential-secret-key current-aws-credential))

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
  
;; (require
;;  (only-in (planet knozama/common:1/type/date)
;; 	  current-time-rfc-2822
;; 	  current-time-iso-8601))

;; (: auth-headers (String -> Params))
;; (define (auth-headers tok)
;;   (list 
;;    (cons "host" "someawshost")
;;    (cons "x-amZ-security-token" tok)
;;    (cons "x-amZ-security-token" "tokabc")
;;    (cons "x-amz-date" (current-time-rfc-2822))
;;    (cons "x-amz-target" "DynamoDB_20111205.ListTables")))

;; (define (test)
;;   (pretty-print (auth-signee "rayhost" (filter-canonicalize-headers (auth-headers "tok123"))))
;;   (pretty-print (auth-signature "rayhost" (filter-canonicalize-headers (auth-headers "tok123")))))

