#lang typed/racket/base

(provide
 start-workflow-execution
 terminate-workflow-execution)

(require
 racket/pretty
 (only-in prelude/std/opt
          opt-map)
 (only-in httpclient/uri/guid
          guid)
 (only-in format/json/tjson
          JsObject jsobject jsobject-opt) 
 (only-in "types.rkt"
	  WorkflowExecution
          VersionedType-name VersionedType-version
          WorkflowType ChildPolicy Duration)
 (only-in "attrs.rkt"
          queue->jsobject
          policy->attr
          duration->attr)
 (only-in "../dynamodb/invoke.rkt"
          workflow)
 (only-in "../dynamodb/parse.rkt"
          attr-value-string))

(define register-workflow-target "SimpleWorkflowService.RegisterWorkflowType")

#| Register a Workflow |#

;; WARNING - Call with care.  The SWF API has NO mechanism to delete a registered workflow.
(: register-workflow (case-> 
                      (String String String -> Void)
                      (String String String (Option String) (Option String) (Option Duration) (Option Duration) (Option ChildPolicy) -> Void)))
(define (register-workflow domain name version [description #f] [task-queue #f] [max-task-time #f] [max-time #f] [child-policy #f])
  (let ((payload (jsobject-opt `((domain . ,domain)
                                 (name . ,name)
                                 (version . ,version)    
                                 (description . ,description)
                                 (defaultTaskList . ,(queue->jsobject task-queue))
                                 (defaultExecutionStartToCloseTimeout . ,(duration->attr max-time))
                                 (defaultTaskStartToCloseTimeout . ,(duration->attr max-task-time))
                                 (defaultChildPolicy . ,(policy->attr child-policy))))))
    (workflow register-workflow-target payload)
    (void)))

#| Start a Workflow execution |#

(define start-workflow-execution-target "SimpleWorkflowService.StartWorkflowExecution")

(: parse-start-workflow-execution-response (String JsObject -> WorkflowExecution))
(define (parse-start-workflow-execution-response wid jsobj)
  (WorkflowExecution wid (attr-value-string jsobj 'runId)))

(: start-workflow-execution (String WorkflowType 
				    [#:id String]
				    [#:policy (Option ChildPolicy)]
				    [#:execution-timeout (Option Natural)]
				    [#:task-timeout (Option Natural)]
				    [#:input String]
				    [#:tags (Listof String)]
				    [#:queue String] -> WorkflowExecution))
(define (start-workflow-execution domain workflow-type
                                  #:id [id (guid)]
                                  #:policy [child-policy #f] 
                                  #:execution-timeout [execution-timeout #f]
                                  #:task-timeout [task-timeout #f]
                                  #:input [input ""] 
                                  #:tags [tag-list '()]
                                  #:queue [task-queue ""])
  (parse-start-workflow-execution-response id  (workflow start-workflow-execution-target 
							 (jsobject-opt `((domain . ,domain)
									 (workflowId . ,id)
									 (workflowType . ,(jsobject `((name . ,(VersionedType-name workflow-type))
												      (version . ,(VersionedType-version workflow-type)))))
									 (childPolicy . ,(policy->attr child-policy))
									 (executionStartToCloseTimeout . ,(opt-map execution-timeout number->string))
									 (taskStartToCloseTimeout . ,(opt-map task-timeout number->string))
									 (tagList . ,tag-list)
									 (input . ,input)
									 (taskList . ,(queue->jsobject task-queue)))))))

#| Terminate a Workflow, a hard kill |#

(define terminate-workflow-execution-target "SimpleWorkflowService.TerminateWorkflowExecution")

(: terminate-workflow-execution (String String 
                                        [#:run-id String]
                                        [#:policy (Option ChildPolicy)]
                                        [#:details String]
                                        [#:reason String] -> Void))
(define (terminate-workflow-execution domain workflow-id 
                                      #:run-id [run-id ""]
                                      #:policy [child-policy #f]
                                      #:details [details ""]
                                      #:reason [reason ""])
  (pretty-print workflow-id)
  (pretty-print run-id)
  (workflow terminate-workflow-execution-target
            (jsobject-opt `((domain . ,domain)
                            (workflowId . ,workflow-id)
                            (runId . ,run-id)
                            (childPolicy . ,(policy->attr child-policy))
                            (details . ,details)
                            (reason . ,reason))))
  (void))

#| Request cancelling a Workflow - soft kill, a request to the decider to terminate |#

(define request-cancel-workflow-execution-target "SimpleWorkflowService.RequestCancelWorkflowExecution")

(: request-cancel-workflow-execution (String String [#:run-id String] -> Void))                                             
(define (request-cancel-workflow-execution domain workflow-id #:run-id [run-id ""])
  (workflow request-cancel-workflow-execution-target
            (jsobject-opt `((domain . ,domain)
                            (workflowId . ,workflow-id)
                            (runId . ,run-id))))
  (void))
