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

#| Poll for SWF Decision and Activity tasks from a queue. |#

#lang typed/racket/base

(provide
 (struct-out DecisionTask)
 (struct-out ActivityTask)
 poll-for-decision-task
 poll-for-activity-task)

(require 
 racket/pretty
 (only-in racket/function
          thunk)
 (only-in prelude/std/opt
          opt-getorelse)
 (only-in format/json/tjson
          JsObject Json json->string jsobject jsobject-opt)
 (only-in "types.rkt"
          WorkflowExecution 
          WorkflowType ActivityType 
          Input TaskToken)
 (only-in "attrs.rkt"
          parse-workflow-type
          parse-workflow-execution)
 (only-in "history/historyevent.rkt"
          HistoryEvent)
(only-in "history/history.rkt"
	 parse-history-events)
 (only-in "../dynamodb/parse.rkt"          
          attr-value-jsobject attr-value-jslist
          attr-value-integer attr-value-integer-opt
          attr-value-string attr-value-string-opt)
 (only-in "../dynamodb/invoke.rkt"
          workflow))

(struct: ActivityTask ([id : String]
                       [type : ActivityType]
                       [input : (Option Input)]
                       [started-event-id : Integer]
                       [token : TaskToken]
                       [workflow : WorkflowExecution]) #:transparent)

(struct: DecisionTask ([task-token : String]
                       [events : (Listof HistoryEvent)]
                       [next-page-token : (Option String)]
                       [started-event-id : Integer]                      
                       [previous-started-event-id : Integer]                       
                       [workflow-execution : WorkflowExecution]
                       [workflow-type : WorkflowType]) #:transparent)

(define poll-for-activity-target "SimpleWorkflowService.PollForActivityTask")
(define poll-for-decision-target "SimpleWorkflowService.PollForDecisionTask")

(: no-available-activity (JsObject -> Boolean))
(define (no-available-activity jsobj)
  (let ((started-event-id (attr-value-integer jsobj 'startedEventId)))
    (zero? started-event-id))) ;; long poll timed out with a response from AWS of no activity received

;; Note a proper response may be a "no task" response, which is why the return value is optional.
(: parse-activity-task-response (JsObject -> (Option ActivityTask)))
(define (parse-activity-task-response response)

  (: parse-activity-type (-> ActivityType))
  (define (parse-activity-type)
    (let ((activity (attr-value-jsobject response 'activityType)))
      (ActivityType (attr-value-string activity 'name)
		    (attr-value-string activity 'version))))
  
  (: parse-workflow-execution (-> WorkflowExecution))
  (define (parse-workflow-execution)
    (let ((wf (attr-value-jsobject response 'workflowExecution)))
      (WorkflowExecution (attr-value-string wf 'workflowId)
			 (attr-value-string wf 'runId))))  
  
  (let ((started-event-id (attr-value-integer response 'startedEventId)))
    (if (no-available-activity response) ;; timedout with no activity received
        #f
        (ActivityTask (attr-value-string response 'activityId)
		      (parse-activity-type)
                      (attr-value-string-opt response 'input)
		      (attr-value-integer response 'startedEventId)
		      (attr-value-string response 'taskToken)
		      (parse-workflow-execution)))))

#| AWS long polls, so a call will "hang" for 60 secs prior to returning. |#
(: poll-for-activity-task (String (Option String) String -> (Option ActivityTask)))
(define (poll-for-activity-task domain identity task-list)
  (let ((payload (jsobject-opt `((domain . ,domain)
                                 (identity . ,identity)
                                 (taskList . ,(jsobject `((name . ,task-list))))))))
    (parse-activity-task-response (workflow poll-for-activity-target payload))))

#| Decision Task API |#
(: parse-decision-task-response (JsObject -> (Option DecisionTask)))
(define (parse-decision-task-response jsobj)
  (if (no-available-activity jsobj)
      #f
      (DecisionTask (attr-value-string jsobj 'taskToken)
                    (parse-history-events (attr-value-jslist jsobj 'events))
                    (attr-value-string-opt jsobj 'nextPageToken)
                    (attr-value-integer jsobj 'startedEventId)
                    (opt-getorelse (attr-value-integer-opt jsobj 'previousStartedEventId)
                                   (thunk (attr-value-integer jsobj 'startedEventId)))
                    (parse-workflow-execution (attr-value-jsobject jsobj 'workflowExecution))
                    (parse-workflow-type (attr-value-jsobject jsobj 'workflowType)))))

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
