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

#lang typed/racket/base

(provide
 EventType EventType?
 (struct-out HistoryEvent))

(provide:
 [parse-history-events (JsList -> (Listof HistoryEvent))])

(require
 (only-in format/json/tjson
          JsObject JsList)
 (only-in "../dynamodb/parse.rkt"
          attr-value-real
          attr-value-integer
          attr-value-string
          attr-value-jsobject)
 (only-in "types.rkt"
	  WorkflowExecution)
 (only-in "attrs.rkt"
	  parse-workflow-type))

(define-type EventType (U 'WorkflowExecutionStarted 
                          'WorkflowExecutionCancelRequested 
                          'WorkflowExecutionCompleted 
                          'CompleteWorkflowExecutionFailed 
                          'WorkflowExecutionFailed 
                          'FailWorkflowExecutionFailed 
                          'WorkflowExecutionTimedOut 
                          'WorkflowExecutionCanceled 
                          'CancelWorkflowExecutionFailed '
                          'WorkflowExecutionContinuedAsNew 
                          'ContinueAsNewWorkflowExecutionFailed 
                          'WorkflowExecutionTerminated 
                          'DecisionTaskScheduled 
                          'DecisionTaskStarted 
                          'DecisionTaskCompleted 
                          'DecisionTaskTimedOut 
                          'ActivityTaskScheduled 
                          'ScheduleActivityTaskFailed 
                          'ActivityTaskStarted 
                          'ActivityTaskCompleted 
                          'ActivityTaskFailed 
                          'ActivityTaskTimedOut 
                          'ActivityTaskCanceled 
                          'ActivityTaskCancelRequested 
                          'RequestCancelActivityTaskFailed 
                          'WorkflowExecutionSignaled 
                          'MarkerRecorded 
                          'TimerStarted 
                          'StartTimerFailed 
                          'TimerFired 
                          'TimerCanceled 
                          'CancelTimerFailed 
                          'StartChildWorkflowExecutionInitiated 
                          'StartChildWorkflowExecutionFailed 
                          'ChildWorkflowExecutionStarted 
                          'ChildWorkflowExecutionCompleted 
                          'ChildWorkflowExecutionFailed 
                          'ChildWorkflowExecutionTimedOut 
                          'ChildWorkflowExecutionCanceled 
                          'ChildWorkflowExecutionTerminated 
                          'SignalExternalWorkflowExecutionInitiated 
                          'SignalExternalWorkflowExecutionFailed 
                          'ExternalWorkflowExecutionSignaled 
                          'RequestCancelExternalWorkflowExecutionInitiated 
                          'RequestCancelExternalWorkflowExecutionFailed 
                          'ExternalWorkflowExecutionCancelRequested))

(define-predicate EventType? EventType)

(struct: HistoryEvent ([event-id : Integer]
                       [timestamp : Real]
                       [event-type : EventType]
                       [attributes : JsObject]) #:transparent)

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


