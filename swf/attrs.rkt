;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Amazon API Library
;; Copyright (C) 2007-2013  Raymond Paul Racine
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

#| Parse routines for common message simple and complex types. |#

#lang typed/racket/base

(provide:
 [parse-workflow-type (JsObject -> WorkflowType)]
 [parse-workflow-execution (JsObject -> WorkflowExecution)]
 [parse-workflow-type (JsObject -> WorkflowType)]
 [queue->jsobject ((Option String) -> Json)] 
 [policy->attr ((Option ChildPolicy) -> (Option String))]
 [duration->attr ((Option Duration) -> (Option String))])

(require
 (only-in prelude/std/opt
          opt-map)
 (only-in format/json/tjson
          Json JsObject JsList jsobject-opt)          
 (only-in "../dynamodb/parse.rkt"
          attr-value-real
          attr-value-integer
          attr-value-string
          attr-value-jsobject)
 (only-in "types.rkt"
          WorkflowExecution
          WorkflowType ChildPolicy Duration))

#| Serialization helpers for API calls |#

(: duration->attr ((Option Duration) -> (Option String)))
(define (duration->attr duration)
  (opt-map duration (λ: ((duration : Duration))
                      (if (number? duration)
                          (number->string duration)
                          (symbol->string duration)))))

(: policy->attr ((Option ChildPolicy) -> (Option String)))
(define (policy->attr policy)
  (opt-map policy symbol->string))

(: queue->jsobject ((Option String) -> Json))
(define (queue->jsobject queue)
  (opt-map queue (λ: ((s : String)) (jsobject-opt `((name . ,s))))))

#| Deserialization helpers for API calls |#

(: parse-workflow-type (JsObject -> WorkflowType))
(define (parse-workflow-type jsobj)
  (WorkflowType (attr-value-string jsobj 'name)
                (attr-value-string jsobj 'version)))

(: parse-workflow-execution (JsObject -> WorkflowExecution))
(define (parse-workflow-execution jsobj)
  (WorkflowExecution (attr-value-string jsobj 'workflowId) (attr-value-string jsobj 'runId)))

