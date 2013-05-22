#lang typed/racket/base

(provide keyword-search
	 browse-node-search)

(require
 (only-in net/uri/url/url
	  QParam QParams add-qparam merge-qparams)
 (only-in httpclient/encode
	  url-encode-string)
 (only-in httpclient/http11
	  http-invoke)
 (only-in httpclient/header
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

(: search-op-parm QParam)
(define search-op-parm
  (QParam "Operation" "ItemSearch"))

(: index-parm (Symbol -> QParam))
(define index-parm
  (lambda (sym)
    (case sym
      ((KINDLE)
       (QParam "SearchIndex" "KindleStore"))
      ((BOOKS)
       (QParam "SearchIndex" "Books"))
      (else (QParam "SearchIndex" "All")))))

;;       '("SearchIndex" . "Books&Power=binding:Kindle Edition"))

(: group (Symbol -> String))
(define (group sym)
  (case sym
    ((Attributes)	"ItemAttributes")
    ((Nodes)		"BrowseNodes")
    ((Offer)		"OfferSummary")
    ((Rank)		"SalesRank")
    ((Small)		"Small")
    ((Large)		"Large")
    ((Review)		"EditorialReview")
    ((Ids)		"ItemIds")
    (else		"Small")))

;;      ((Images)		"Images")

(: rank (Symbol -> String))
(define (rank sym)
  (case sym
    ((PriceAsc)		"price")
    ((PriceDesc)	"-price")
    ((Review)		"reviewrank")
    ((Date)		"daterank")
    ((Sales)		"salesrank")
    (else		"daterank")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIven a list of group symbols form the ResponseGroup kv param.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: response-group-parm ((Listof Symbol) -> QParam))
(define (response-group-parm groups)
  (let loop ((groups groups)(param ""))
    (if (null? groups)
	(QParam "ResponseGroup" param)
	(loop (cdr groups)
	      (let ((g (group (car groups))))
		(if (void? g)
		    param
		    (if (equal? param "")
			g
			(string-append param "," g))))))))

(: browse-node-search (Symbol (Listof Symbol) Integer (Option Symbol) Symbol Integer -> (Listof Any)))
(define (browse-node-search index groups node power by page)

  (: add-sort (QParams -> QParams))
  (define (add-sort params)
    (if by
	(add-qparam (QParam "Sort" (rank by)) params)
	params))

  (: add-power (QParams -> QParams))
  (define (add-power params)
    (if power
	(add-qparam (QParam "Power" (symbol->string power)) params)
	params))

  (let* ((base (list (QParam "BrowseNode" (number->string node))
		     search-op-parm
		     (QParam "ItemPage" (number->string page))
		     (response-group-parm groups)
		     (index-parm index)))
	 (parms (add-power (add-sort base))))
    (a2s-invoke parms)))

(: keyword-search (Symbol (Listof Symbol) String -> (Listof Any)))
(define (keyword-search index groups words)
  (let ((parms (list (QParam "Keywords" (url-encode-string words #f))
		     search-op-parm
		     (response-group-parm groups)
		     (index-parm index))))
    (a2s-invoke parms)))
