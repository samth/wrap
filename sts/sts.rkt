#lang typed/racket/base

(provide
 get-session-token)

(require
 racket/pretty
 (only-in net/uri/url/url
	  Url url->string)
 (only-in httpclient/http11
	  HTTPConnection-in http-successful? http-close-connection http-invoke)
 (only-in net/uri/url/url
	  Url Authority
	  QParam QParams qparams->string)
 (only-in format/xml/sxml
	  Sxml SXPath sxpath xml->sxml extract-text extract-integer)
 (only-in httpclient/header
	  Headers make-header)
 (only-in "../credential.rkt"
	  SessionCredential SessionCredential?)
 (only-in "../auth/authv2.rkt"
	  authv2-signature)
 (only-in "config.rkt"
	  get-session-token-action sts-host sts-api-version)
 (only-in "response.rkt"
	  parse-session-response))

(: request-headers Headers)
(define request-headers
  (list
   ;; (make-header-string "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.2 Safari/535.11")
   (make-header "User-Agent" "Googlebot/2.1 (+http://www.google.com/bot.html)")
   (make-header "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
   (make-header "Accept-Encoding" "gzip")
   (make-header "Accept-Language" "en-US,en;q=0.8")
   (make-header "Cache-Control" "max-age=0")
   (make-header "Connection" "Close")))

(: invoke-uri (String QParams -> Url))
(define (invoke-uri path qparams)
  (Url "https" (Authority #f sts-host 443) path qparams ""))

(: invoke-sts-get (All (a) (Url Headers (Sxml -> a) -> a)))
(define (invoke-sts-get url headers resp-parser)
  (let ((conn (http-invoke 'GET url headers #f)))
    (let ((page (xml->sxml (HTTPConnection-in conn) '())))
      (http-close-connection conn)
      (resp-parser page))))

(: signed-query (String QParams -> QParams))
(define (signed-query cmd qparams)
  (authv2-signature sts-api-version "GET" sts-host cmd "/" qparams))

(: duration-param (Natural -> QParam))
(define (duration-param duration-secs)
  ;; (assert (and (>= duration-secs 3600) (<= duration-secs 129600)))
  (QParam "DurationSeconds" (number->string duration-secs)))

;; 3600s (one hour) to 129600s (36 hours), with 43200s (12 hours) as default
(: get-session-token (Exact-Nonnegative-Integer -> SessionCredential))
(define (get-session-token duration-secs)
  (let ((url (invoke-uri "/" (signed-query get-session-token-action '()))))
    (invoke-sts-get url request-headers parse-session-response)))
