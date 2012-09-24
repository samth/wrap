#lang typed/racket/base

(provide 
 S3Payload S3Payload?
 S3Response S3Response? S3Response-http S3Response-sxml
 empty-response make-empty-error-response
 make-base-uri
 s3-invoke)

(require
 racket/pretty
 (only-in (planet rpr/httpclient:1/uri)
          make-uri Uri Uri-path)
 (only-in (planet rpr/prelude:1/type/date)
          current-date-string-rfc-2822)
 (only-in (planet rpr/httpclient:1/http/heading)
          DATE HOST)
 (only-in (planet rpr/httpclient:1/http/header)
          Header Headers
          header->string
          make-header
          date-header
          content-length
          content-type
          content-md5)
 (only-in (planet rpr/httpclient:1/uri/url/param)
          Params
          params->query)
 (only-in (planet rpr/httpclient:1/http/http11)
          Method HTTPPayload HTTPPayload-md5 HTTPPayload-mime
          http-method->string http-status-code http-has-content?
          ResponseHeader-status StatusLine
          HTTPConnection-in HTTPConnection-header
          http-invoke http-close-connection make-client-error-response)
 (only-in (planet rpr/format:1/xml/sxml)
          Sxml SXPath 
          sxpath xml->sxml select-single-node-text)
 (only-in "../credential.rkt"
          current-aws-credential
          AwsCredential
          BaseCredential-access-key
          BaseCredential-secret-key)
 (only-in "../auth.rkt"
          aws-auth-str
          aws-auth-mac)
 (only-in "../configuration.rkt"
          s3-host)
 (only-in "configuration.rkt"
          s3-namespace nss))

(struct: S3Response ([http : StatusLine]
                     [sxml : Sxml]) #:transparent)

(struct: S3Payload ([length : Index]
                    [mime : String]
                    [md5  : String]
                    [inport  : Input-Port]) #:transparent)

(: empty-response (List Symbol))
(define empty-response '(*TOP*))

(: make-empty-error-response (Integer String -> S3Response))
(define (make-empty-error-response status-code message)
  (S3Response (StatusLine 'HTTP/1.1 status-code message) empty-response))

(: make-base-uri ((Option String) String (Option Params) -> (Option Uri)))
(define (make-base-uri bucket path params)
  
  (: make-bucket-host (-> String))
  (define (make-bucket-host)
    (if bucket
        (string-append bucket "." s3-host)
        s3-host))
  
  (let ((query (if params 
                   (params->query params)
                   #f)))
    (make-uri "http" #f (make-bucket-host) 80 path query #f)))

(: authorization-header (AwsCredential String -> Header))
(define (authorization-header credential auth-str)
  (make-header "Authorization" 
               (string-append  "AWS " 
                               (BaseCredential-access-key credential) 
                               ":" 
                               (aws-auth-mac (BaseCredential-secret-key credential)
                                             auth-str))))

(: s3-invoke (Method (Option String) String (Option Params) Headers (Option HTTPPayload) -> S3Response))
(define (s3-invoke action bucket path query-params headers payload)
  (let ((url (make-base-uri bucket path query-params)))
    (if url
        (let* ((datetime (current-date-string-rfc-2822))
               (canonical-resource (if bucket
                                       (string-append "/" bucket (Uri-path url))
                                       (Uri-path url)))
               
               (md5 (if payload 
                        (let ((md5 (HTTPPayload-md5 payload)))
                          (if md5 md5 ""))
                        ""))
               (mime (if payload
                         (HTTPPayload-mime payload)
                         ""))
               (core-headers  (list (make-header DATE datetime)
                                    (authorization-header (current-aws-credential)
                                                          (aws-auth-str (http-method->string action)
                                                                        md5 mime
                                                                        datetime '()
                                                                        canonical-resource)))))
          (pretty-print url)
          (let ((connection (http-invoke action 
                                         url 
                                         (append core-headers headers)
                                         payload)))
            (with-handlers [(exn:fail? (lambda (ex)
                                         ((error-display-handler) "ERROR in S3 invocation." ex)
                                         (displayln ex) 
                                         (http-close-connection connection)
                                         (S3Response (StatusLine 'HTTP/1.1 440
                                                                 (string-append "Bad Request - "
                                                                                (exn-message ex)))
                                                     empty-response)))]
              
              (if (http-has-content? connection)
                  (let ((results (xml->sxml (HTTPConnection-in connection) '())))
                    (http-close-connection connection)
                    (S3Response (ResponseHeader-status (HTTPConnection-header connection))
                                results))
                  (S3Response (ResponseHeader-status (HTTPConnection-header connection))
                              empty-response)))))
        (S3Response (StatusLine 'HTTP/1.1 400 
                                (string-append "Bad Request - Malformed URL"))
                    empty-response))))

