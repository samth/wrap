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

#| Data structures and response building routines for Decision task responses. |#

#lang typed/racket/base

(provide:
 [fail-workflow-execution-decision (case-> (-> JsObject)
					   (String String -> JsObject))]
 [respond-decision-task-completed (String String (Listof JsObject) -> Void)]
 [signal-external-workflow-execution-decision SignalExternalWorkflowExecutionDecisionFn]
 [continue-as-new-workflow-decision ContinueAsNewWorkflowDecisionFn]
 [schedule-activity-task-decision ScheduleActivityTaskDecisionFn])

(require 
 ;;racket/pretty
 (only-in prelude/std/opt
          opt-map)
 (only-in httpclient/uri/guid
          guid)
 (only-in format/json/tjson
          JsObject jsobject jsobject-opt)
 (only-in "types.rkt"
          ChildPolicy
          ActivityType WorkflowType
          VersionedType-name VersionedType-version)
 (only-in "../dynamodb/invoke.rkt"
          workflow))

(define-type ScheduleActivityTaskDecisionFn (ActivityType 
                                             [#:id String] 
                                             [#:input String] 
                                             [#:heartbeat-timeout (Option Integer)]
                                             [#:control String]
                                             [#:schedule-to-start-timeout (Option Integer)]
                                             [#:schedule-to-close-timeout (Option Integer)]
                                             [#:start-to-close-timeout (Option Integer)]
                                             [#:task-list String]-> JsObject))

(define-type ContinueAsNewWorkflowDecisionFn ([#:version String]
                                              [#:child-policy (Option ChildPolicy)]
                                              [#:execution-timeout (Option Integer)]
                                              [#:task-timeout (Option Integer)]
                                              [#:input String]
                                              [#:tag-list (Listof String)]
                                              [#:task-list String] -> JsObject))

(define-type StartChildWorkflowExecutionDecisionFn (String 
                                                    WorkflowType
                                                    [#:child-policy (Option ChildPolicy)]
                                                    [#:control String]
                                                    [#:execution-timeout (Option Integer)]
                                                    [#:task-timeout (Option Integer)]
                                                    [#:input String]
                                                    [#:tag-list (Listof String)]
                                                    [#:task-list String] -> JsObject))

(define-type SignalExternalWorkflowExecutionDecisionFn (String 
                                                        String
                                                        [#:control String]
                                                        [#:input String]
                                                        [#:run-id String] -> JsObject))

(define respond-decision-task-completed-target "SimpleWorkflowService.RespondDecisionTaskCompleted")

;;(define-type DecisionAck (U 'Ack 'Nack))

(: respond-decision-task-completed (String String (Listof JsObject) -> Void))
(define (respond-decision-task-completed task-token context decisions)  
  (workflow respond-decision-task-completed-target (jsobject `((taskToken . ,task-token)
                                                               (executionContext . ,context)
                                                               (decisions . ,decisions))))
  (void))

;; ScheduleActivityTask schedules an activity task.
(: schedule-activity-task-decision ScheduleActivityTaskDecisionFn)
(define (schedule-activity-task-decision activity-type 
                                         #:id [id (guid)] 
                                         #:input [input ""] 
                                         #:heartbeat-timeout [heartbeat-timeout #f]
                                         #:control [control ""]
                                         #:schedule-to-start-timeout [schedule-to-start-timeout #f]
                                         #:schedule-to-close-timeout [schedule-to-close-timeout #f]
                                         #:start-to-close-timeout [start-to-close-timeout #f]
                                         #:task-list [task-list ""])
  (define attrs (jsobject-opt `((activityType . ,(jsobject `((name . ,(VersionedType-name activity-type))
                                                             (version . ,(VersionedType-version activity-type)))))
                                (input . ,input)
                                (activityId . ,id)
                                (heartbeatTimeout . ,heartbeat-timeout)
                                (control . ,control)
                                (scheduleToStartTimeout . ,(opt-map schedule-to-start-timeout number->string))
                                (scheduleToCloseTimeout . ,(opt-map schedule-to-close-timeout number->string))
                                (startToCloseTimeot . ,(opt-map start-to-close-timeout number->string))
                                (taskList . ,(jsobject-opt `((name . ,task-list)))))))
  (jsobject-opt `((decisionType . "ScheduleActivityTask")
                  (scheduleActivityTaskDecisionAttributes . ,attrs))))                                         

#| RequestCancelActivityTask attempts to cancel a previously scheduled activity task. 
If the activity task was scheduled but has not been assigned to a worker, then it will be canceled. 
If the activity task was already assigned to a worker, then the worker will be informed
that cancellation has been requested in the response to RecordActivityTaskHeartbeat. |#
(: request-cancel-activity-task-decision (case-> (-> JsObject)
                                                 (String -> JsObject)))
(define (request-cancel-activity-task-decision [activity-id (guid)])
  (jsobject `((decisionType . "RequestCancelActivityTask")
              (requestCancelActivityTaskDecisionAttributes . ,(jsobject `((activityId . ,activity-id)))))))

#| RecordMarker records a MarkerRecorded event in the history. 
Markers can be used for adding custom information in the history for instance 
to let deciders know that they do not need to look at the history beyond the marker event. |#
(: record-marker-decision (case-> (String -> JsObject)
                                  (String String -> JsObject)))
(define (record-marker-decision name [details ""])
  (jsobject `((decisionType . "RecordMarker")
              (recordMarkerDecisionAttributes . ,(jsobject-opt `((markerName . ,name)
                                                                 (details . ,details)))))))

#| CompleteWorkflowExecution closes the workflow execution 
and records a WorkflowExecutionCompleted event in the history. |#
(: complete-workflow-execution-decision (case-> (-> JsObject)
                                                (String -> JsObject)))
(define (complete-workflow-execution-decision [result ""])
  (jsobject-opt `((decisionType . "CompleteWorkflowExecution")
                  (completeWorkflowExecutionDecisionAttributes . ,(jsobject-opt `((result . ,result)))))))


#| FailWorkflowExecution closes the workflow execution 
and records a WorkflowExecutionFailed event in the history. |#
(: fail-workflow-execution-decision (case-> (-> JsObject)
                                            (String String -> JsObject)))
(define (fail-workflow-execution-decision [reason ""] [details ""])
  (jsobject-opt `((decisionType . "FailWorkflowExecution")
                  (failWorkflowExecutionDecisionAttributes . ,(jsobject-opt `((reason . ,reason)
                                                                              (details . ,details)))))))

#| CancelWorkflowExecution closes the workflow execution 
and records a WorkflowExecutionCanceled event in the history. |#
(: cancel-workflow-execution-decision (case-> (-> JsObject)
                                              (String -> JsObject)))
(define (cancel-workflow-execution-decision [details ""])
  (jsobject-opt `((decisionType . "CancelWorkflowExecution")
                  (cancelWorkflowExecutionDecisionAttributes . ,(jsobject-opt `((details . ,details)))))))

#| StartTimer starts a timer for this workflow execution and records a TimerStarted event in the history. 
This timer will fire after the specified delay and record a TimerFired event. |#
(: start-timer-decision (case-> (String Natural -> JsObject)
                                (String Natural String -> JsObject)))
(define (start-timer-decision id duration [control ""])
  (jsobject `((decisionType . "StartTimer")
              (startTimerDecisionAttributes . ,(jsobject-opt `((timerId . ,id)
                                                               (startToFireTimeout . ,duration)
                                                               (control . ,control)))))))

#| CancelTimer cancels a previously started timer and records a TimerCanceled event in the history. |#
(: cancel-timer-decision (String -> JsObject))
(define (cancel-timer-decision id)
  (jsobject `((decisionType . "CancelTimer")
              (cancelTimerDecisionAttributes . ,(jsobject `((timerId . ,id)))))))

#| ContinueAsNewWorkflowExecution closes the workflow execution 
and starts a new workflow execution of the same type using the same workflow id 
and a unique run Id. A WorkflowExecutionContinuedAsNew event is recorded in the history. |#
(: continue-as-new-workflow-decision ContinueAsNewWorkflowDecisionFn)                                       
(define (continue-as-new-workflow-decision #:version [version ""]
                                           #:child-policy [policy #f]
                                           #:execution-timeout [execution-to #f]
                                           #:task-timeout [task-to #f]
                                           #:input [input ""]
                                           #:tag-list [tag-list '()]
                                           #:task-list [task-list ""])
  (define attrs (jsobject-opt `((workflowTypeVersion . ,version)
                                (childPolicy . ,(if policy (symbol->string policy) #f))
                                (executionStartToCloseTimeout . ,execution-to)
                                (taskStartToCloseTimeout . ,task-to)
                                (input . ,input)
                                (tagList . ,tag-list)
                                (task-list . ,(jsobject-opt `((name . ,task-list)))))))
  (jsobject-opt `((decisionType . "ContinueAsNewWorkflow")
                  (continueAsNewWorkflowDecisionAttributes . ,attrs))))

#| SignalExternalWorkflowExecution requests a signal to be delivered 
to the specified external workflow execution and records
a SignalExternalWorkflowExecutionInitiated event in the history. |#
(: signal-external-workflow-execution-decision SignalExternalWorkflowExecutionDecisionFn)
(define (signal-external-workflow-execution-decision workflow-id signal-name [control ""] [input ""] [run-id ""])
  (define attrs (jsobject-opt `((workflowId . ,workflow-id)
                                (signalName . ,signal-name)
                                (runId . , run-id)
                                (control . ,control)
                                (input . ,input))))
  (jsobject `((decisionType . "SignalExternalWorkflowExecution")
              (signalExternalWorkflowExecutionDecisionAttributes . ,attrs))))

#| RequestCancelExternalWorkflowExecution requests that a request be made to cancel 
the specified external workflow execution and records 
a RequestCancelExternalWorkflowExecutionInitiated event in the history. |#
(: request-cancel-external-workflow-execution-decision (String [#:run-id String] [#:control String] -> JsObject))
(define (request-cancel-external-workflow-execution-decision workflow-id #:run-id [run-id ""] #:control [control ""])
  (define attrs (jsobject-opt `((runId . ,run-id)
                                (control . ,control))))
  (jsobject-opt `((decisionType . "RequestCancelExternalWorkflowExecution")              
                  (requestCancelExternalWorkflowExecutionDecisionAttributes . ,attrs))))

#|--------------------------------------------------------------------------------- 
StartChildWorkflowExecution requests that a child workflow execution be started 
and records a StartChildWorkflowExecutionInitiated event in the history. 
The child workflow execution is a separate workflow execution with its own history. 
-----------------------------------------------------------------------------------|#
(: start-child-workflow-execution-decision StartChildWorkflowExecutionDecisionFn)
(define (start-child-workflow-execution-decision workflow-id workflow-type
                                                 #:child-policy [policy #f] 
                                                 #:control [control ""]
                                                 #:execution-timeout [execution-timeout #f]
                                                 #:task-timeout [task-timeout #f]
                                                 #:input [input ""]
                                                 #:tag-list [tag-list '()]
                                                 #:task-list [task-list ""])
  (define attrs (jsobject-opt `((workflowId . ,workflow-id)
                                (workflowType . ,(jsobject `((name . ,(VersionedType-name workflow-type))
                                                             (version . ,(VersionedType-version workflow-type)))))
                                (childPolicy . ,(if policy (symbol->string policy) #f))
                                (control . ,control)
                                (executionStartToCloseTimeout . ,execution-timeout)
                                (taskStartToCloseTimeout . , task-timeout)
                                (input . ,input)
                                (tagList . ,tag-list)
                                (taskList . ,(jsobject-opt `((name . ,task-list)))))))
  (jsobject `((decisionType . "StartChildWorkflowExecutionDecision")
              (startChildWorkflowExecutionDecision . ,attrs))))
