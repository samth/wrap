#lang typed/racket/base

(require
 racket/pretty
 (only-in prelude/std/opt
          opt-map)
 (only-in format/json/tjson
          jsobject jsobject-opt)
 (only-in "types.rkt"
          ChildPolicy Duration)
 (only-in "attrs.rkt"
          duration->string)
 (only-in "../dynamodb/invoke.rkt"
          workflow))

(define register-workflow-target "SimpleWorkflowService.RegisterWorkflowType")

;{"domain": "867530901",
; "name": "customerOrderWorkflow",
; "version": "1.0",
; "description": "Handle customer orders",
; "defaultTaskStartToCloseTimeout": "600",
; "defaultExecutionStartToCloseTimeout": "3600",
; "defaultTaskList":
;	{"name": "mainTaskList"},
; "defaultChildPolicy": "TERMINATE"}

(: register-workflow (case-> 
                      (String String String -> Void)
                      (String String String (Option String) (Option String) (Option Duration) (Option Duration) (Option ChildPolicy) -> Void)))
(define (register-workflow domain name version [description #f] [task-list #f] [max-task-time #f] [max-time #f] [child-policy #f])
  (let ((payload (jsobject-opt `((domain . ,domain)
                                 (name . ,name)
                                 (version . ,version)    
                                 (description . ,description)
                                 (defaultTaskList . ,(opt-map task-list (λ: ((s : String)) (jsobject `((name . ,s))))))
                                 (defaultExecutionStartToCloseTimeout . ,(opt-map max-time duration->string))
                                 (defaultTaskStartToCloseTimeout . ,(opt-map max-task-time duration->string))
                                 (defaultChildPolicy . ,(opt-map child-policy symbol->string))))))
    (pretty-print (workflow register-workflow-target payload))    
    (void)))
