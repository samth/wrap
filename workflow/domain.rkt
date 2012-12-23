#lang typed/racket/base

(require
 racket/pretty
 (only-in format/json/tjson
          Json JsObject json->string string->json jsobject)
 (only-in "../dynamodb/invoke.rkt"
          workflow)
 (only-in "../dynamodb/parse.rkt"
          attr-value-jsobject attr-value-jslist attr-value) 
 (only-in "types.rkt"          
          WFResponseCode))

(define register-domain-target  "SimpleWorkflowService.RegisterDomain")
(define deprecate-domain-target "SimpleWorkflowService.DeprecateDomain")
(define describe-domain-target  "SimpleWorkflowService.DescribeDomain")
(define list-domains-target     "SimpleWorkflowService.ListDomains")

(: register-domain (String String (U Natural 'NONE) -> WFResponseCode))
(define (register-domain name desc retention-days)
  (let ((sdays (if (number? retention-days)
                   (number->string retention-days)
                   (symbol->string retention-days))))
    (let: ((payload : JsObject (jsobject `((name . ,name)
                                           (description . ,desc)
                                           (workflowExecutionRetentionPeriodInDays . ,sdays)))))
      (pretty-print (workflow register-domain-target payload))
      'Success)))

;; Describe domain API call

(define-type DomainStatus (U 'REGISTERED 'DEPRECATED))
(define-predicate DomainStatus? DomainStatus)

(struct: DomainInfo ([name : String]
                     [description : String]
                     [status : DomainStatus]) #:transparent)

(struct: DomainDescription DomainInfo ([retention-period : (U 'NONE Natural)]) #:transparent)

(: parse-duration (String -> (U 'NONE Natural)))
(define (parse-duration s)
  (if (string=? s "NONE")
      (cast (string->symbol s) 'NONE)
      (let ((duration (string->number s)))
        (if (and (exact-integer? duration)
                 (>= duration 0))    
            duration
            (error 'parse-domain-info "Invalid duration ~s in describe-domain call" s)))))

(: parse-status (String -> DomainStatus))
(define (parse-status s)
  (let ((status (string->symbol s)))
    (if (DomainStatus? status)
        status
        (error "Invalid status ~s in describe-domain call" s))))     

(: parse-domain-description-response (JsObject -> DomainDescription))
(define (parse-domain-description-response jsobj)  
  (let ((info (attr-value-jsobject jsobj 'domainInfo ))
        (config (attr-value-jsobject jsobj 'configuration)))    
    (DomainDescription (attr-value info 'name string?)
                       (attr-value info 'description string?)
                       (parse-status (attr-value info 'status string?))
                       (parse-duration (attr-value config 'workflowExecutionRetentionPeriodInDays string?)))))

(: describe-domain (String -> DomainInfo))
(define (describe-domain name)  
  (let: ((payload : JsObject (jsobject `((name . ,name)))))                                             
    (parse-domain-info-response (cast (workflow describe-domain-target payload) JsObject))))

;; Deprecate Domain API call
(: deprecate-domain (String -> Void))
(define (deprecate-domain name)
  (workflow deprecate-domain-target (jsobject `((name . ,name))))
  (void))

;; List Domains API call

(: parse-domain-info-response (JsObject -> DomainInfo))
(define (parse-domain-info-response jsobj)  
  (DomainInfo (attr-value jsobj 'name string?)
              (attr-value jsobj 'description string?)
              (parse-status (attr-value jsobj 'status string?))))

(: parse-list-domains-response (JsObject -> (Listof DomainInfo)))
(define(parse-list-domains-response resp)
  (let ((infos (attr-value-jslist resp 'domainInfos)))    
    (map (λ: ((json : Json))
           (parse-domain-info-response (cast json JsObject)))
         infos)))

(: list-domains (case-> 
                 (DomainStatus -> (Listof DomainInfo))
                 (DomainStatus Natural String Boolean -> (Listof DomainInfo))))
(define (list-domains status [max-page-size 100] [next-page-token ""] [reverse-order? #f])
  (let ((payload (jsobject `((maximumPageSize . ,max-page-size)
                             (nextTokenPage . ,next-page-token)
                             (registrationStatus . ,(symbol->string status))
                             (reverseOrder . ,reverse-order?)))))
    (parse-list-domains-response (cast (workflow list-domains-target payload) JsObject))))
