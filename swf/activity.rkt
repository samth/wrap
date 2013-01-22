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

#| Data structures and response building routines for Activity task responses. |#

#lang typed/racket/base

(provide:
 [respond-activity-task-completed (String String -> Void)])

(require
 (only-in format/json/tjson
          JsObject jsobject jsobject-opt)
 (only-in "../dynamodb/invoke.rkt"
          workflow))

(define respond-activity-task-completed-target "SimpleWorkflowService.RespondActivityTaskCompleted")

(: respond-activity-task-completed (String String -> Void))
(define (respond-activity-task-completed task-token result)
  (workflow respond-activity-task-completed-target (jsobject `((result . ,result)
							       (taskToken . ,task-token))))
  (void))
  


