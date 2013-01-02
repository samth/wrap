#lang typed/racket/base

(require
 racket/pretty
 (only-in format/json/tjson
          Json JsObject json->string string->json jsobject)
 (only-in "../dynamodb/invoke.rkt"
          workflow)
 (only-in "../dynamodb/parse.rkt"
          attr-value-jsobject attr-value-jslist attr-value-string) 
 (only-in "types.rkt"          
          Duration
          WFResponseCode)
 (only-in "attrs.rkt"
          duration->attr))

(define register-domain-target  "SimpleWorkflowService.RegisterDomain")
(define deprecate-domain-target "SimpleWorkflowService.DeprecateDomain")
(define describe-domain-target  "SimpleWorkflowService.DescribeDomain")
(define list-domains-target     "SimpleWorkflowService.ListDomains")

(: register-domain (String String Duration -> WFResponseCode))
(define (register-domain name desc retention-days)
  (let ((sdays (duration->attr retention-days)))                   
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
    (DomainDescription (attr-value-string info 'name)
                       (attr-value-string info 'description)
                       (parse-status (attr-value-string info 'status))
                       (parse-duration (attr-value-string config 'workflowExecutionRetentionPeriodInDays)))))

(: describe-domain (String -> DomainInfo))
(define (describe-domain name)  
  (let: ((payload : JsObject (jsobject `((name . ,name)))))                                             
    (parse-domain-description-response (cast (workflow describe-domain-target payload) JsObject))))

;; Deprecate Domain API call
(: deprecate-domain (String -> Void))
(define (deprecate-domain name)
  ;(workflow deprecate-domain-target (jsobject `((name . ,name))))
  (void))

;; List Domains API call

(: parse-domain-info-response (JsObject -> DomainInfo))
(define (parse-domain-info-response jsobj)  
  (DomainInfo (attr-value-string jsobj 'name)
              (attr-value-string jsobj 'description)
              (parse-status (attr-value-string jsobj 'status))))

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
