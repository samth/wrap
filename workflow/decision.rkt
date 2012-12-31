#lang typed/racket/base

(provide:
 [signal-external-workflow-execution-decision SignalExternalWorkflowExecutionDecisionFn]
 [continue-as-new-workflow-decision ContinueAsNewWorkflowDecisionFn]
 [schedule-activity-task-decision ScheduleActivityTaskDecisionFn])

(require 
 (only-in httpclient/uri/guid
          guid)
 (only-in format/json/tjson
          JsObject jsobject jsobject-opt)
 (only-in "types.rkt"
          ChildPolicy
          ActivityType WorkflowType
          VersionedType-name VersionedType-version))

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

;{"taskToken": "AAAAKgAAAAEAAAAAAAAAAQLPoqDSLcx4ksNCEQZCyEBqpKhE+FgFSOvHd9zlCROacKYHh640MkANx2y9YM3CQnec0kEb1oRvB6DxKesTY3U/UQhvBqPY7E4BYE6hkDj/NmSbt9EwEJ/a+WD+oc2sDNfeVz2x+6wjb5vQdFKwBoQ6MDWLFbAhcgK+ymoRjoBHrPsrNLX3IA6sQaPmQRZQs3FRZonoVzP6uXMCZPnCZQULFjU1kTM8VHzH7ywqWKVmmdvnqyREOCT9VqmYbhLntJXsDj+scAvuNy17MCX9M9AJ7V/5qrLCeYdWA4FBQgY4Ew6IC+dge/UZdVMmpW/uB7nvSk6owQIhapPh5pEUwwY/yNnoVLTiPOz9KzZlANyw7uDchBRLvUJORFtpP9ZQIouNP8QOvFWm7Idc50ahwGEdTCiG+KDXV8kAzx7wKHs7l1TXYkC15x0h3XPH0MdLeEjipv98EpZaMIVtgGSdRjluOjNWEL2zowZByitleI5bdvxZdgalAXXKEnbYE6/rfLGReAJKdh2n0dmTMI+tK7uuxIWX6F4ocqSI1Xb2x5zZ",
; "decisions":
;  [
;    {"decisionType": "ScheduleActivityTask",
;     "scheduleActivityTaskDecisionAttributes":
;      {"activityType":
;        {"name": "activityVerify",
;         "version": "1.0"},
;       "activityId": "verification-27",
;       "control": "digital music",
;       "input": "5634-0056-4367-0923,12/12,437",
;       "scheduleToCloseTimeout": "900",
;       "taskList":
;        {"name": "specialTaskList"},
;       "scheduleToStartTimeout": "300",
;       "startToCloseTimeout": "600",
;       "heartbeatTimeout": "120"}
;    }
;  ],
; "executionContext": "Black Friday"}

;ScheduleActivityTask schedules an activity task.
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
                                (scheduleToStartTimeout . ,schedule-to-start-timeout)
                                (scheduleToCloseTimeout . ,schedule-to-close-timeout)
                                (startToCloseTimeot . ,start-to-close-timeout)
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

;StartTimer starts a timer for this workflow execution and records a TimerStarted event in the history. This timer will fire after the specified delay and record a TimerFired event.
(: start-timer-decision (case-> (String Natural -> JsObject)
                                (String Natural String -> JsObject)))
(define (start-timer-decision id duration [control ""])
  (jsobject `((decisionType . "StartTimer")
              (startTimerDecisionAttributes . ,(jsobject-opt `((timerId . ,id)
                                                               (startToFireTimeout . ,duration)
                                                               (control . ,control)))))))

;CancelTimer cancels a previously started timer and records a TimerCanceled event in the history.
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

#| StartChildWorkflowExecution requests that a child workflow execution be started 
   and records a StartChildWorkflowExecutionInitiated event in the history. 
   The child workflow execution is a separate workflow execution with its own history. |#


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
