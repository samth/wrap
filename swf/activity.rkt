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
          parse-history-events
          parse-workflow-type
          parse-workflow-execution)
 (only-in "historyevent.rkt"
          HistoryEvent)
 (only-in "../dynamodb/parse.rkt"          
          attr-value-jsobject attr-value-jslist
          attr-value-integer attr-value-integer-opt
          attr-value-string attr-value-string-opt)
 (only-in "../dynamodb/invoke.rkt"
          workflow))

(define-type TaskList String)

(struct: ActivityTask ([id : String]
                       [type : ActivityType]
                       [input : Input]
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
(: parse-activity-task (JsObject -> (Option ActivityTask)))
(define (parse-activity-task json)
  ;;(pretty-print json)
  (let ((started-event-id (attr-value-integer json 'startedEventId)))
    (if (no-available-activity json) ;; timedout with no activity received
        #f
        (ActivityTask "id" (ActivityType "name" "ver") 
                      "inputkadfafadf"  started-event-id "token" 
                      (WorkflowExecution "wf-id" "run-id")))))

;; AWS long polls, so a call will "hang" for 60 secs prior to returning.
(: poll-for-activity-task (String (Option String) String -> TaskList))
(define (poll-for-activity-task domain identity task-list)
  (let ((payload (jsobject-opt `((domain . ,domain)
                                 (identity . ,identity)
                                 (taskList . ,(jsobject `((name . ,task-list))))))))
    ;;(pretty-print payload)
    (json->string (workflow poll-for-activity-target payload))))


					;'#hasheq((previousStartedEventId . 0)
					;         (events
					;          .
					;          (#hasheq((eventId . 1)
					;                   (workflowExecutionStartedEventAttributes
					;                    .
					;                    #hasheq((input . "my input data here")
					;                            (taskList . #hasheq((name . "mr-master")))
					;                            (parentInitiatedEventId . 0)
					;                            (taskStartToCloseTimeout . "1200")
					;                            (executionStartToCloseTimeout . "7200")
					;                            (childPolicy . "TERMINATE")
					;                            (workflowType
					;                             .
					;                             #hasheq((name . "mapreduce") (version . "1.0")))))
					;                   (eventType . "WorkflowExecutionStarted")
					;                   (eventTimestamp . 1.3569767483749))
					;           #hasheq((eventId . 2)
					;                   (decisionTaskScheduledEventAttributes
					;                    .
					;                    #hasheq((taskList . #hasheq((name . "mr-master")))
					;                            (startToCloseTimeout . "1200")))
					;                   (eventType . "DecisionTaskScheduled")
					;                   (eventTimestamp . 1.3569767483749))
					;           #hasheq((eventId . 3)
					;                   (decisionTaskStartedEventAttributes
					;                    .
					;                    #hasheq((scheduledEventId . 2)))
					;                   (eventType . "DecisionTaskStarted")
					;                   (eventTimestamp . 1.35697734219))))
					;         (workflowType . #hasheq((name . "mapreduce") (version . "1.0")))
					;         (startedEventId . 3)
					;         (workflowExecution
					;          .
					;          #hasheq((runId . "11KxU8+9lE8BB+7F2FMTAfwBOb+LxeEtjG2gQBf9YEl2U=")
					;                  (workflowId . "9420161c-4f32-112d-9b8b-0090f5ccb571")))
					;         (taskToken
					;          .
					;          "AAAAKgAAAAEAAAAAAAAAAbhOa7pTSREnSf3IsydwCMC/k3Nhx1H2RXEiA8TwMobm7isRaNtiVMNUgjhNqBd4xtb2TloC5jaQIovzOpRnNRWFC6ZgPtGvJVyG4Cvq2rpNlAEfevRNGEwEHhc4dxmqsh+2dNaoL6Pio0rA6Ud3JX7K8JHOnAxxiEDptqbEMmqQD48W002ZCV4dIIdW9Jehaq72aAELQKsGEIoc+6BaetFI3ps2zzU7yjQCyIni2klAotT387fEPw6ArPcmUWh8qowsc/yVZDmH01oP6FqZecpYF9KGlTy+wBS2qq8sCqg9IZaRMdmXJuphRv7gpkcClw=="))
					;- : (U False DecisionTask)
					;(DecisionTask '() "" 0 0 "" #<WorkflowExecution> #<WorkflowType>)

;; Decision Task API

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
