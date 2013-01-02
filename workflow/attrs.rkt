#lang typed/racket/base

(provide:
 [queue->jsobject ((Option String) -> Json)] 
 [policy->attr ((Option ChildPolicy) -> (Option String))]
 [duration->attr ((Option Duration) -> (Option String))]
 [parse-history-events (JsList -> (Listof HistoryEvent))]
 [parse-workflow-type (JsObject -> WorkflowType)]
 [parse-workflow-execution (JsObject -> WorkflowExecution)])

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
          WorkflowType ChildPolicy Duration)
 (only-in "historyevent.rkt"
          EventType EventType?
          HistoryEvent))

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


(: parse-history-event-attributes (Symbol JsObject -> JsObject))
(define (parse-history-event-attributes event-type jsobj)
  
  (: build-stupid-attribute-name (-> Symbol))
  (define (build-stupid-attribute-name)
    (let ((event-type (symbol->string event-type)))
      (string->symbol (string-append (string-downcase (substring event-type 0 1))
                                     (substring event-type 1)
                                     "EventAttributes"))))
  
  (attr-value-jsobject jsobj (build-stupid-attribute-name)))


(: parse-history-event (JsObject -> HistoryEvent))
(define (parse-history-event jsobj)  
  (let ((event-type (string->symbol (attr-value-string jsobj 'eventType))))
    (HistoryEvent (attr-value-integer jsobj 'eventId)
                  (attr-value-real jsobj 'eventTimestamp)
                  (assert event-type EventType?)
                  (parse-history-event-attributes event-type jsobj))))

(: parse-history-events (JsList -> (Listof HistoryEvent)))
(define (parse-history-events events)
  (map parse-history-event (cast events (Listof JsObject))))

(: parse-workflow-execution (JsObject -> WorkflowExecution))
(define (parse-workflow-execution jsobj)
  (WorkflowExecution (attr-value-string jsobj 'runId)
                     (attr-value-string jsobj 'workflowId)))