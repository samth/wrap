#lang typed/racket/base

(provide
 (struct-out VersionedType)
 (struct-out ActivityType)
 (struct-out WorkflowType)
 (struct-out WorkflowExecution)
 Input TaskToken 
 Duration
 ChildPolicy
 WFResponseCode)

(define-type Duration (U 'NONE Natural))

(define-type ChildPolicy (U 'TERMINATE 'REQUEST_CANCEL 'ABANDON))

(define-type Input String)
(define-type TaskToken String)

(define-type WFResponseCode 
  (U 'Success 
     'DomainAlreadyExists 'LimitExceeded 'OperationNotPermitted))

(struct: WorkflowExecution ([run-id : String]
                            [workflow-id : String]))

(struct: VersionedType ([name : String]
                        [version : String]))

(struct: ActivityType VersionedType ())
(struct: WorkflowType VersionedType ())