#lang typed/racket/base

(provide keyword-search
	 browse-node-search)

(require
 racket/pretty
 (only-in (planet rpr/httpclient:1/uri)
	  make-uri
	  url-encode-string)
 (only-in (planet rpr/httpclient:1/http/http11)
	  http-invoke)
 (only-in (planet rpr/httpclient:1/http/header)
	  Header
	  Headers)
 (only-in "../credential.rkt"
	  AwsCredential-associate-tag
	  BaseCredential-secret-key
	  BaseCredential-access-key)
 (only-in "../configuration.rkt"
	  a2s-ns
	  a2s-host)
 "a2s.rkt")

(: search-op-parm Header)
(define search-op-parm
  '("Operation" . "ItemSearch"))

(: index-parm (Symbol -> Header))
(define index-parm
  (lambda (sym)
    (case sym
      ((KINDLE)
       '("SearchIndex" . "KindleStore"))
      ((BOOKS)
       '("SearchIndex" . "Books"))
      (else '("SearchIndex" . "All")))))

;;       '("SearchIndex" . "Books&Power=binding:Kindle Edition"))

(: group (Symbol -> String))
(define (group sym)
  (case sym
    ((Attributes)  "ItemAttributes")
    ((Nodes)       "BrowseNodes")
    ((Offer)       "OfferSummary")
    ((Rank)	   "SalesRank")
    ((Small)	   "Small")
    ((Large)       "Large")
    ((Review)	   "EditorialReview")
    ((Ids)	   "ItemIds")
    (else          "Small")))  

;;      ((Images)		"Images")

(: rank (Symbol -> String))
(define (rank sym)
  (case sym
    ((PriceAsc)	 "price")
    ((PriceDesc) "-price")
    ((Review)	 "reviewrank")
    ((Date)	 "daterank")
    ((Sales)	 "salesrank")
    (else        "daterank")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIven a list of group symbols form the ResponseGroup kv param and url-encode it.
;; listof (symbol?) -> string?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: response-group-parm ((Listof Symbol) -> Header))
(define (response-group-parm groups)
  (let loop ((groups groups)(param ""))
    (if (null? groups)
       `("ResponseGroup" . ,(url-encode-string param #f))
       (loop (cdr groups)
	     (let ((g (group (car groups))))
	       (if (void? g)
		  param
		  (if (equal? param "")
		     g
		     (string-append param "," g))))))))


(: browse-node-search (Symbol (Listof Symbol) Integer (Option Symbol) Symbol Integer -> (Listof Any)))
(define (browse-node-search index groups node power by page)

  (: add-sort (Headers -> Headers))
  (define (add-sort params)
    (if by
       (cons `("Sort" . ,(rank by)) params)
       params))
  
  (: add-power (Headers -> Headers))
  (define (add-power params)
    (if power
       (cons `("Power" . ,(symbol->string power)) params)
       params))
  
  (let* ((base
	(cons `("BrowseNode" . ,(number->string node))
	      (cons search-op-parm 
		    (cons `("ItemPage" . ,(number->string page))
			  (cons (response-group-parm groups) 
				(list (index-parm index)))))))
       (parms (add-power (add-sort base))))
    
    (displayln parms)
    (a2s-invoke parms)))

(: keyword-search (Symbol (Listof Symbol) String -> (Listof Any)))
(define (keyword-search index groups words)
  (let ((parms 
       (cons `("Keywords" . ,(url-encode-string words #f))
	     (cons search-op-parm
		   (cons (response-group-parm groups) (list (index-parm index)))))))
    (a2s-invoke parms)))
