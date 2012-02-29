#lang typed/racket/base

(provide)
;; create-domain list-domains delete-domain)

(require
 racket/pretty
 (only-in (planet knozama/common:1/std/control)
	  aif)
 (only-in (planet knozama/aws:1/credential)
	  BaseCredential-secret-key BaseCredential-access-key current-aws-credential)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  params->query parse-params param Param Params)
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri Uri-query make-uri parse-uri
	  url-encode-string uri->string)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  HTTPConnection-in http-successful? http-close-connection http-invoke)
 (only-in (planet knozama/webkit:1/web/http/header)
          Headers make-header)
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath sxpath html->sxml xml->sxml extract-text extract-integer)
 (only-in "../auth/authv2.rkt"
	  authv2-signature)
 (only-in "config.rkt"
	  sdb-host sdb-ns sdb-api-version))

(struct: SDBError () #:transparent)

(struct: ListDomains ([domains  : (Listof String)]
		      [nextToken : (Option String)]) #:transparent)
(struct: MetaDomain ([item-count : Integer]
		     [item-size : Integer]
		     [name-count : Integer]
		     [name-size : Integer]		     
		     [value-count : Integer]
		     [value-size : Integer]
		     [timestamp : Integer]) #:transparent)

(struct: Expect ([name : String] [value : String]
		 [exist : Boolean]) #:transparent)

;; The value of replace is examined iff value is a string.
(struct: Attr ([name : String] [value : String]
	       [replace : Boolean]) #:transparent)

(define-type AEParam (U Expect Attr))

(: CREATE-ACTION String)
(define CREATE-ACTION "CreateDomain")

(: DELETE-ACTION String)
(define DELETE-ACTION "DeleteDomain")

(: LIST-ACTION String)
(define LIST-ACTION "ListDomains")

(: META-ACTION String)
(define META-ACTION "DomainMetadata")

(: GET-ACTION String)
(define GET-ACTION "GetAttributes")

(: PUT-ACTION String)
(define PUT-ACTION "PutAttributes")

(: request-headers Headers)
(define request-headers  
  (list 
   ;; (make-header-string "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.2 Safari/535.11")
   (make-header "User-Agent" "Googlebot/2.1 (+http://www.google.com/bot.html)")
   (make-header "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
   (make-header "Accept-Charset" "ISO-8859-1,utf-8;q=0.7,*;q=0.3")
   (make-header "Accept-Encoding" "gzip")
   (make-header "Accept-Language" "en-US,en;q=0.8")
   (make-header "Cache-Control" "max-age=0")
   (make-header "Connection" "Close")))

(: domain-param (String -> Param))
(define (domain-param domain)
  (param "DomainName" domain))

(: invoke-uri (String String -> Uri))
(define (invoke-uri path query)
  (make-uri "https" #f sdb-host 443 path query ""))

(: signed-query (String String String (Listof (Pairof String String)) -> Params))
(define (signed-query http-action cmd path qparams)
  (authv2-signature sdb-api-version http-action sdb-host cmd path qparams))

(: invoke-sdb-get (All (a) (Uri Headers (Sxml -> (U SDBError a)) -> (U SDBError a))))
(define (invoke-sdb-get url headers resp-parser)
  (with-handlers ([exn:fail? 
		   (lambda (ex) (SDBError))])
    (let ((conn (http-invoke 'GET url headers #f)))
      (pretty-print conn)
      (let ((page (xml->sxml (HTTPConnection-in conn) '())))
	(pretty-print page)
	(resp-parser page)))))

(: parse-list-domains-resp (Sxml -> (U SDBError ListDomains)))
(define (parse-list-domains-resp sxml)
  (ListDomains '() ""))

(: list-domains ((Option Integer) -> (U SDBError ListDomains)))
(define (list-domains maxList)
  (let ((url (invoke-uri "/" (params->query (signed-query "GET" LIST-ACTION "/" '())))))
    (invoke-sdb-get url request-headers parse-list-domains-resp)))

(: parse-create-domain-resp (Sxml -> (U SDBError True)))
(define (parse-create-domain-resp sxml)
  #t)

(: create-domain (String -> (U SDBError True)))
(define (create-domain domain)
    (let ((url (invoke-uri "/" (params->query (signed-query "GET" CREATE-ACTION "/" (list (domain-param domain)))))))
      (invoke-sdb-get url request-headers parse-create-domain-resp)))

(: parse-delete-domain-resp (Sxml -> (U SDBError True)))
(define (parse-delete-domain-resp sxml)
  #t)

(: delete-domain (String -> (U SDBError True)))
(define (delete-domain domain)
  (let ((url (invoke-uri "/" (params->query (signed-query "GET" DELETE-ACTION "/" (list (domain-param domain)))))))
    (invoke-sdb-get url request-headers parse-delete-domain-resp)))

(: mk-sxpath (String -> SXPath))
(define (mk-sxpath path)
  (sxpath path `(,sdb-ns)))

(define sx-meta-result		(mk-sxpath "/sdb:DomainMetadataResponse/sdb:DomainMetadataResult"))
(define sx-meta-item-count		(mk-sxpath "/sdb:ItemCount/text()"))
(define sx-meta-item-names-size	(mk-sxpath "/sdb:ItemNamesSizeBytes/text()"))
(define sx-meta-attr-names-count	(mk-sxpath "/sdb:AttributeNameCount/text()"))
(define sx-meta-attr-names-size	(mk-sxpath "/sdb:AttributeNamesSizeBytes/text()"))
(define sx-meta-attr-values-count	(mk-sxpath "/sdb:AttributeValueCount/text()"))
(define sx-meta-attr-values-size	(mk-sxpath "/sdb:AttributeValuesSizeBytes/text()"))
(define sx-meta-timestamp		(mk-sxpath "/sdb:Timestamp/text()"))

(: parse-meta-domain-resp (Sxml -> (U SDBError MetaDomain)))
(define (parse-meta-domain-resp sxml)
  
  (: extract-int-minus-error (Sxml -> Integer))
  (define (extract-int-minus-error sxml)
    (aif (extract-integer sxml) it -1))
  
  (let ((result (sx-meta-result sxml)))
    (let ((item-cnt (extract-int-minus-error (sx-meta-item-count result)))
	(item-names-sz (extract-int-minus-error (sx-meta-item-names-size result)))
	(attr-name-cnt (extract-int-minus-error (sx-meta-attr-names-count result)))
	(attr-names-sz (extract-int-minus-error (sx-meta-attr-names-size result)))
	(attr-values-cnt (extract-int-minus-error (sx-meta-attr-values-count result)))
	(attr-values-sz (extract-int-minus-error (sx-meta-attr-values-size result)))
	(ts (extract-int-minus-error (sx-meta-timestamp result))))
      (MetaDomain item-cnt item-names-sz 
		  attr-name-cnt attr-names-sz
		  attr-values-cnt attr-values-sz ts))))

(: meta-domain (String -> (U SDBError MetaDomain)))
(define (meta-domain domain)
  (let ((url (invoke-uri "/" (params->query (signed-query "GET" META-ACTION "/" (list (domain-param domain)))))))
    (invoke-sdb-get url request-headers parse-meta-domain-resp)))

(: attr-name-value (String Attr ->  (Listof Param)))
(define (attr-name-value sid attr)
  (let ((aparam (list 
	       (cons (string-append "Attribute." sid ".Value") (url-encode-string (Attr-value attr) #f))
	       (cons (string-append "Attribute." sid ".Name") (url-encode-string (Attr-name attr) #f)))))
    (if (Attr-replace attr)
       (cons  (cons (string-append  "Attribute." sid ".Replace") "true") aparam)
       aparam)))


(: attr-expected-name-value (String Expect -> (Listof Param)))
(define (attr-expected-name-value sid attr)
  (cond ((Expect-exist attr)
	 (list
	  (cons (string-append "Expected." sid ".Exist") "true")
	  (cons (string-append "Expected." sid ".Name") (url-encode-string (Expect-name attr) #f))))
	((Expect-value attr)
	 (list
	  (cons (string-append "Expected." sid ".Value") (Expect-value attr))
	  (cons (string-append "Expected." sid ".Name") (url-encode-string (Expect-name attr) #f))))
	(else
	 (list
	  (cons (string-append "Expected." sid ".Exist") "false")
	  (cons (string-append "Expected." sid ".Name") (url-encode-string (Expect-name attr) #f))))))

(: attr-params (Integer AEParam -> (Listof Param)))
(define (attr-params id attr)
  (let ((sid (number->string id)))
    (cond
     ((Attr? attr) (attr-name-value sid attr))
     ((Expect? attr) (attr-expected-name-value sid attr)))))

(: build-attribute-query ((Listof AEParam) -> (Listof Param)))
(define (build-attribute-query attrs)
  (let: loop : (Listof Param) ((attrs : (Listof AEParam) attrs) (id : Integer 1) (accum : (Listof Param) '()))
    (if (null? attrs)
       (reverse accum)
       (let ((attr-set (attr-params id (car attrs))))
	 (loop (cdr attrs) (+ 1 id) (append attr-set accum))))))

(: item-param (String -> Param))
(define (item-param item)
  (cons "ItemName" (url-encode-string item #f)))

(: parse-put-attrs-response (Sxml -> (U SDBError True)))
(define (parse-put-attrs-response sxml)
  ;;(pretty-print sxml)
  #t)

(: put-attributes (String String (Listof AEParam) -> (U SDBError True)))
(define (put-attributes domain item attrs)
  (let ((qparams (cons (item-param item)
		     (build-attribute-query attrs))))
    (pretty-print qparams)
    (let ((url (invoke-uri "/" (params->query (signed-query "GET" PUT-ACTION "/" qparams)))))
      (invoke-sdb-get url request-headers parse-put-attrs-response)
      (pretty-print (uri->string url))
      #t)))

(: consistent-param (Boolean -> Param))
(define (consistent-param flag)
  (if flag
     (cons "ConsistentRead" "true")
     (cons "ConsistentRead" "false")))

(: build-attribute-get-query ((Listof String) -> (Listof Param)))
(define (build-attribute-get-query attr-names)
  (let: loop : (Listof Param) ((names : (Listof String) attr-names) 
			     (id : Integer 1) 
			     (accum : (Listof Param) '()))
      (if (null? names)
	 (reverse accum)
	 (loop (cdr names) (+ 1 id) (cons (cons (string-append "AttributeName." (number->string id)) 
						(car names))
					  accum)))))

(: get-attributes (String String (Listof String) Boolean -> (U SDBError True)))
(define (get-attributes domain item attrs consistent?)
  (let ((qparams (cons (item-param item)
		     (cons (consistent-param consistent?) 
			   (build-attribute-get-query attrs)))))
    (pretty-print qparams)
    (let ((url (invoke-uri "/" (params->query (signed-query "GET" GET-ACTION "/" qparams)))))
      (invoke-sdb-get url request-headers parse-put-attrs-response)
      #t)))
