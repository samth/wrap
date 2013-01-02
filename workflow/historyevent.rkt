#lang typed/racket/base

(provide
 EventType EventType?
 (struct-out HistoryEvent))

(require
 (only-in format/json/tjson
          JsObject))

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
