#lang typed/racket/base

(provide
 a2s-invoke
 ;;fetch-parse
 browse-node service-parms
 empty-response
 sign-request
 item-lookup similarity-lookup)

(require/typed racket/base
	       (error-display-handler (-> (String Any -> Any))))

(require/typed
 (planet lizorkin/ssax:2:0/ssax)
 (ssax:xml->sxml (Input-Port (Listof String) -> (Listof Any))))

(require
 racket/pretty
 (only-in type/opt
	  opt-get-orelse-value)
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html)
 (only-in type/text
	  weave-string-separator)
 (only-in net/uri/url/url
	  QParam QParams
	  merge-qparams qparams->string add-qparam
	  Url Authority)
 (only-in net/uri/url/show
	  url->path-query-fragment-string)
 (only-in httpclient/encode
	  url-encode-string)
 (only-in httpclient/header
	  Header Headers host-header header->string)
 (only-in httpclient/http11
	  http-invoke HTTPConnection-in)
 (only-in type/date
	  current-date-string-iso-8601)
 (only-in crypto/hmac
	  hmac-sha256)
 (only-in crypto/base64
	  base64-encode)
 (only-in "../configuration.rkt"
	  a2s-ns a2s-host a2s-path)
 (only-in "../credential.rkt"
	  current-aws-credential
	  BaseCredential-secret-key
	  BaseCredential-access-key))

(: empty-response (List Symbol))
(define empty-response '(*TOP*))

(: a2s-host-header String)
(define a2s-host-header (header->string (host-header a2s-host)))

(: itemlookup-parms QParams)
(define itemlookup-parms
  (list (QParam "Operation" "ItemLookup")))

(: browse-parms QParams)
(define browse-parms
  (list (QParam "Operation"     "BrowseNodeLookup")
	(QParam "ResponseGroup" "BrowseNodeInfo")))

(: service-parms QParams)
(define service-parms
  (list (QParam "Service" "AWSECommerceService")
	(QParam "Version" "2010-11-01")))

(: core-parms (-> QParams))
(define (core-parms)
  (merge-qparams (list  (QParam "Timestamp"
				(url-encode-string (current-date-string-iso-8601 #f) #f))
			(QParam "AWSAccessKeyId"
				(BaseCredential-access-key (current-aws-credential))))
		 service-parms))

(: browse-node (Integer -> (Listof Any)))
(define (browse-node node-id)
  (let ((parms (add-qparam (QParam "BrowseNodeId" (number->string node-id))
			   browse-parms)))
    (a2s-invoke parms)))

(: similarity-lookup ((Listof String) -> (Listof Any)))
(define (similarity-lookup asins)
  (let ((qparms (list (QParam "Operation" "SimilarityLookup")
		      (QParam "IdType" "ASIN")
		      (QParam "ItemId" (weave-string-separator "," asins))
		      (QParam "ResponseGroup" (url-encode-string "SalesRank,ItemAttributes,Images,EditorialReview" #f)))))
    (a2s-invoke qparms)))

(: item-lookup (String -> (Listof Any)))
(define (item-lookup asin)
  (let ((parms (append itemlookup-parms
		       (list (QParam "IdType" "ASIN")
			     (QParam "ItemId" asin)
			     (QParam "ResponseGroup" (url-encode-string "BrowseNodes" #f))))))
    ;;  ("ResponseGroup" . ,(url-encode-string "Small,Reviews" #f))))))
    ;;("ResponseGroup" . ,(url-encode-string "SalesRank,Small,ItemAttributes,EditorialReview,Images,Reviews,Offers,Similarities" #f))
    ;; (displayln "ITEM LOOKUP")
    (a2s-invoke parms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A2S required sorted param string ready for signing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: params->string (Headers -> String))
(define (params->string params)
  (define str-sort (inst sort String String))
  (weave-string-separator "&" (str-sort (map (lambda: ((pair : Header))
						      (string-append (car pair) "=" (cdr pair)))
					     params) string<?)))

(: sign-request (String QParams -> QParams))
(define (sign-request action qparams)
  (let* ((param-str (opt-get-orelse-value (qparams->string qparams) ""))
	 (auth-str (weave-string-separator "\n" (list action
						      a2s-host
						      a2s-path
						      param-str)))
	 (sig (url-encode-string (base64-encode
				  (hmac-sha256
				   (BaseCredential-secret-key (current-aws-credential))
				   auth-str)) #f)))
    (add-qparam (QParam "Signature=" sig) qparams)))


;; Generic call procedure to the REST A2S API
(: a2s-invoke (QParams -> (Listof Any)))
(define (a2s-invoke params)
  (let* ((qparams (sign-request "GET" (merge-qparams (core-parms) params)))
	 (uri (Url 'HTTP (Authority #f a2s-host 80) a2s-path qparams #f)))
    (if uri
	(let ((connection (http-invoke 'GET uri '() #f)))
	  (with-handlers [(exn:fail? (lambda (ex)
				       ((error-display-handler) "ERROR in a2s invocation." ex)
				       (displayln ex)
				       (close-input-port (HTTPConnection-in connection))
				       empty-response))]
			 (let ((ip (HTTPConnection-in connection)))
			   (let ((results (ssax:xml->sxml ip '())))
			     (close-input-port ip)
			     results))))
	empty-response)))
