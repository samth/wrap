#lang typed/racket/base

(provide
 auth-signature)

(require
 racket/pretty
 racket/match
 (only-in type/text
	  weave-string-separator)
 (only-in httpclient/header
	  Header Headers
	  Header-name
	  make-header header-lowercase-name
	  header->string)
 (only-in httpclient/encode
	  url-encode-string)
 (only-in crypto/hash/sha256
	  sha256)
 (only-in crypto/base64
	  base64-encode)
 (only-in crypto/hmac
	  hmac-sha256)
 (only-in "../credential.rkt"
	  AwsCredential-session BaseCredential-secret-key
	  current-aws-credential))

(: filter-canonicalize-headers (Headers -> Headers))
(define (filter-canonicalize-headers headers)

  (: x-amz-header? (Header -> Boolean))
  (define (x-amz-header? header)
    (let ((name (Header-name header)))
      (and (>= (string-length name) 6)
	   (string=? (substring name 0 6) "x-amz-"))))

  (: merge-value (String String -> String))
  (define (merge-value new-value curr-values)
    (if (string=? curr-values "")
	new-value
	(string-append curr-values "," new-value)))

  ;; one-pass loop as opposed to merge-filter-map 3 pass
  (let: ((merged : (HashTable String String) (make-hash)))
	(let: loop : Headers ((headers : Headers headers))
	      (if (null? headers)
		  (hash->list merged)
		  (let ((lparam (header-lowercase-name (car headers))))
		    (when (x-amz-header? lparam)
			  (match lparam
				 ((cons name value)
				  (hash-update! merged name
						(lambda: ((curr-value : String))
							 (merge-value value curr-value))
						(lambda () "")))))
		    (loop (cdr headers)))))))

(: auth-signee (String Headers String -> String))
(define (auth-signee host headers body)
  (let ((hdrs (apply string-append
		     (sort (map (lambda: ((header : Header))
					 (string-append (header->string header) "\n"))
				(cons (make-header "host" host) headers))  string<=?))))
    (string-append "POST\n/\n\n" hdrs "\n" body)))

(: auth-signature (String Headers String -> String))
(define (auth-signature host params body)
  (let ((scred (let ((scred (AwsCredential-session (current-aws-credential))))
		 (if scred scred (error "Missing session credentials")))))
    (let ((secret-key (BaseCredential-secret-key scred))
	  (signee (auth-signee host params body)))
      ;;(pretty-print signee)
      (base64-encode (hmac-sha256 secret-key (sha256 signee))))))
