#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple storage of configuration values.
;; we could have multiple configuration maps 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 
 s3-host
 a2s-host a2s-path a2s-ns a2s-nss)

;; host for Amazon Associate Services
(define a2s-host "webservices.amazon.com")
(define a2s-path "/onca/xml")

;; S3 storage host
(define s3-host "s3.amazonaws.com")

;; Amazon Associate Services namespace
(define a2s-namespace "http://webservices.amazon.com/AWSECommerceService/2010-11-01")

(define a2s-ns
  (cons 'a2s a2s-namespace))

(define a2s-nss
  (list a2s-ns))
