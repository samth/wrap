;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's AWS API Library
;; Copyright (C) 2007-2011  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide
 Range
 s3-get-object s3-get-object-to-file s3-head-object s3-object-meta
 s3-put-object s3-delete-object
 s3-put-file-object)

(provide:
 [s3-list-bucket-objects (String String String String Natural -> Keys)])

(require
 racket/pretty
 (only-in racket/path
	  some-system-path->string
	  find-relative-path)
 (only-in type/opt
	  opt-car)
 (only-in type/date
	  current-date-string-rfc-2822)
 (only-in net/uri/url/url
	  QParam QParams
	  Url Url-path url->string)
 (only-in net/http/heading
	  DATE HOST)
 (only-in net/http/header
	  Header
	  header->string
	  make-header
	  make-headers
	  date-header
	  content-length
	  content-type
	  content-md5)
 (only-in net/http/http11
	  ResponseHeader HTTPPayload
	  HTTPConnection-in HTTPConnection-header
	  http-invoke http-close-connection make-client-error-response)
 (only-in crypto/base64
	  base64-encode)
 (only-in crypto/hash/md5
	  md5-bytes)
 (only-in format/xml/sxml
	  Sxml SXPath
	  sxpath xml->sxml select-single-node-text)
 (only-in "../configuration.rkt"
	  s3-host)
 (only-in "types.rkt"
	  Range Prefix Owner Bucket Buckets
	  Keys Keys-objects
	  Key Key-key)
 (only-in "configuration.rkt"
	  nss)
 (only-in "invoke.rkt"
	  make-base-uri make-empty-error-response
	  s3-invoke
	  [s3-get-object s3-get]
	  s3-get-object-pipe-to-file
	  S3Response S3Response-sxml
	  S3Payload))

;; FIXME Use opt-map to create the parameter query string

(: s3-list-bucket-objects (String String String String Natural -> Keys))
(define (s3-list-bucket-objects bucket prefix delimiter marker max)

  (: s->i (String -> (Option Integer)))
  (define (s->i s)
    (let ((i (string->number s)))
      (if (exact-integer? i)
	  i
	  #f)))

  (: s->b (String -> Boolean))
  (define (s->b s)
    (not (string=? s "false")))

  (: parse-owner (Sxml -> Owner))
  (define (parse-owner sxml)
    (define sx-id (select-single-node-text "/s3:Owner/s3:ID" nss))
    (define sx-name (select-single-node-text "/s3:Owner/s3:DisplayName" nss))
    (let ((id (sx-id sxml))
	  (name (sx-name sxml)))
      (Owner id name)))

  (: parse-object (Sxml -> Key))
  (define (parse-object sxml)
    (define sx-key (select-single-node-text "/s3:Key" nss))
    (define sx-last-modified (select-single-node-text "/s3:LastModified" nss))
    (define sx-etag (select-single-node-text "/s3:ETag" nss))
    (define sx-size (select-single-node-text "/s3:Size" nss))
    ;;(define sx-storage (select-single-node-text "/s3:StorageClass" nss))
    (let ((key (sx-key sxml))
	  (last-modified (sx-last-modified sxml))
	  (etag (sx-etag sxml))
	  (size (s->i (sx-size sxml)))
	  ;;(storage (sx-storage sxml))
	  (owner (parse-owner sxml)))
      (Key key last-modified ;; storage
	   etag (assert size) owner)))

  (: parse-objects (Sxml -> (Listof Key)))
  (define (parse-objects sxml)
    (define sx-objects (sxpath "/s3:Contents" nss))
    (let ((objs (sx-objects sxml)))
      (if (andmap list? objs)
	  (map parse-object objs)
	  '())))

  (: parse-prefixes (Sxml -> (Listof Prefix)))
  (define (parse-prefixes sxml)
    (define sx-prefixes (sxpath "/s3:CommonPrefixes" nss))
    (define sx-prefix    (select-single-node-text "/s3:Prefix" nss))
    (let ((pre-s (sx-prefixes sxml)))
      (if (andmap list? pre-s)
	  (map (λ: ((sxml : Sxml)) (Prefix (sx-prefix sxml))) pre-s)
	  '())))

  (: parse-response (Sxml -> Keys))
  (define (parse-response sxml)
    (define sx-name (select-single-node-text "/s3:Name" nss))
    (define sx-prefix (select-single-node-text "/s3:Prefix" nss))
    (define sx-marker (select-single-node-text "/s3:Marker" nss))
    (define sx-max-keys (select-single-node-text "/s3:MaxKeys" nss))
    (define sx-is-truncated (select-single-node-text "/s3:IsTruncated" nss))
    (let ((name (sx-name sxml))
	  (prefix (sx-prefix sxml))
	  (marker (sx-marker sxml))
	  (max-keys (s->i (sx-max-keys sxml)))
	  (is-truncated (s->b (sx-is-truncated sxml)))
	  (objs (parse-objects sxml))
	  (prefixes (parse-prefixes sxml)))
      (Keys name prefix marker (assert max-keys) is-truncated prefixes objs)))

  (define sx-result (sxpath "s3:ListBucketResult" nss))

  (let ((query (list (QParam "prefix" prefix)
		     (QParam "marker" marker)
		     (QParam "delimiter" delimiter)
		     (QParam "max-keys" (number->string max)))))
    (let ((resp (s3-invoke 'GET bucket "" query '() #f)))
      (parse-response (sx-result (S3Response-sxml resp))))))

(: s3-put-file-object (Path String (U Path String) -> S3Response))
(define (s3-put-file-object local-file-path bucket s3-path)
  (let ((s3-path (if (path? s3-path)
		     (path->string s3-path)
		     s3-path)))
    (if (file-exists? local-file-path)
	(let* ((length (assert (file-size local-file-path) index?))
	       (ip (open-input-file local-file-path))
	       (mime "binary/octet-stream")
	       (payload (HTTPPayload mime #f length ip)))
	  (with-handlers [(exn:fail? (lambda (ex)
				       (close-input-port ip)
				       (make-empty-error-response 500 (exn-message ex))))]
			 (s3-invoke 'PUT bucket s3-path '() '() payload)))
	(make-empty-error-response 404 (string-append "File "
						      (path->string local-file-path)
						      " does not exist to PUT")))))

(: s3-put-object (Bytes String String -> S3Response))
(define (s3-put-object bytes bucket path)
  (let* ((length (bytes-length bytes))
	 (md5 (base64-encode (md5-bytes bytes)))
	 (mime "binary/octet-stream")
	 (payload (HTTPPayload mime md5 length (open-input-bytes bytes))))
    (s3-invoke 'PUT bucket path '() '() payload)))

(: s3-delete-object (String String -> S3Response))
(define (s3-delete-object bucket path)
  (s3-invoke 'DELETE bucket path '() '() #f))

(: s3-head-object (String String -> S3Response))
(define (s3-head-object bucket path)
  (s3-invoke 'HEAD bucket path '() '() #f))

(: s3-get-object (case-> (String String -> (U S3Response Bytes))
			 (String String (Option Range) -> (U S3Response Bytes))))
(define (s3-get-object bucket path [range #f])
  (s3-get bucket path range))

(: s3-get-object-to-file (case-> (String String Path -> S3Response)
				 (String String Path (Option Range) -> S3Response)))
(define (s3-get-object-to-file bucket path file-path [range #f])
  (s3-get-object-pipe-to-file bucket path file-path range))

(: s3-object-meta (String String String -> (Option Key)))
(define (s3-object-meta bucket path fname)

  (define s3-rel-path
    (let ((path (string->path path)))
      (if (absolute-path? path)
	  (find-relative-path "/" path)
	  path)))

  (define s3-file (some-system-path->string (build-path s3-rel-path fname)))

  (let ((keys (s3-list-bucket-objects bucket
				      s3-file
				      "" "" 1)))
    (opt-car (filter (λ: ((key : Key))
			 (string=? (Key-key key) s3-file))
		     (reverse (Keys-objects keys))))))
