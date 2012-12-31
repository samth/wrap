#lang typed/racket/base

(require 
 racket/pretty
 (only-in format/json/tjson
          JsObject Json json->string jsobject jsobject-opt)
 (only-in "types.rkt"
          WorkflowExecution 
          WorkflowType ActivityType 
          Input TaskToken)
 (only-in "historyevent.rkt"
          HistoryEvent)
 (only-in "../dynamodb/parse.rkt"
          attr-value-jsobject 
          attr-value-integer
          attr-value-string)
 (only-in "../dynamodb/invoke.rkt"
          workflow))

(define-type TaskList String)

(struct: ActivityTask ([id : String]
                       [type : ActivityType]
                       [input : Input]
                       [started-event-id : Integer]
                       [token : TaskToken]
                       [workflow : WorkflowExecution]))

(struct: DecisionTask ([events : (Listof HistoryEvent)]
                       [next-page-token : String]
                       [previous-started-event-id : Integer]
                       [started-event-id : Integer]
                       [task-token : String]
                       [workflow-execution : WorkflowExecution]
                       [workflow-type : WorkflowType]))

(define poll-for-activity-target "SimpleWorkflowService.PollForActivityTask")
(define poll-for-decision-target "SimpleWorkflowService.PollForDecisionTask")

(: no-available-activity (JsObject -> Boolean))
(define (no-available-activity jsobj)
  (let ((started-event-id (attr-value-integer jsobj 'startedEventId)))
    (zero? started-event-id))) ;; long poll timed out with a response from AWS of no activity received

;; Note a proper response may be a "no task" response, which is why the return value is optional.
(: parse-activity-task (JsObject -> (Option ActivityTask)))
(define (parse-activity-task json)
  (pretty-print json)
  (let ((started-event-id (attr-value-integer json 'startedEventId)))
    (if (no-available-activity json) ;; timedout with no activity received
        #f
        (ActivityTask "id" (ActivityType "name" "ver") 
                      "inputkadfafadf"  started-event-id "token" 
                      (WorkflowExecution "run-id" "wf-id")))))

;; AWS long polls, so a call will "hang" for 60 secs prior to returning.
(: poll-for-activity-task (String (Option String) String -> TaskList))
(define (poll-for-activity-task domain identity task-list)
  (let ((payload (jsobject-opt `((domain . ,domain)
                                 (identity . ,identity)
                                 (taskList . ,(jsobject `((name . ,task-list))))))))
    (pretty-print payload)
    (json->string (workflow poll-for-activity-target payload))))


;; Decision Task API

(: parse-decision-task-response (JsObject -> (Option DecisionTask)))
(define (parse-decision-task-response jsobj)
  (pretty-print jsobj)
  (if (no-available-activity jsobj)
      #f
      (DecisionTask '() "" 0 0 "" (WorkflowExecution "" "") (WorkflowType "" ""))))

;; AWS long polls, so a call will "hang" for 60 secs prior to returning.
(: poll-for-decision-task (case-> (String String -> (Option DecisionTask))
                                  (String String String (Option Integer) String Boolean -> (Option DecisionTask))))
(define (poll-for-decision-task domain task-list 
                                [identity ""] 
                                [max-page-size #f] 
                                [next-page-token ""] 
                                [reverse-order #f])
  (parse-decision-task-response (workflow poll-for-decision-target 
                                          (jsobject-opt `((domain . ,domain)
                                                          (identity . ,identity)
                                                          (maximumPageSize . ,max-page-size)
                                                          (nextPageToken . ,next-page-token)
                                                          (reverseOrder . ,reverse-order)
                                                          (taskList . ,(jsobject `((name . ,task-list)))))))))