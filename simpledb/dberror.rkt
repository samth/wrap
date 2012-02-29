#lang typed/racket/base

(provide
 DBError DBError?
 DBError-http-code
 DBError-http-msg
 DBError-err-code
 DBError-err-msg)

(struct: DBError ([http-code : Integer]
		  [http-msg  : String]
		  [err-code  : Symbol]
		  [err-msg   : String]))

;; AccessFailure	 Access to the resource " + resourceName + " is denied.	403 Forbidden
;; AttributeDoesNotExist	 Attribute ("+ name + ") does not exist	404 Not Found
;; AuthFailure	 AWS was not able to validate the provided access credentials.	403 Forbidden
;; AuthMissingFailure	 AWS was not able to authenticate the request: access credentials are missing.	403 Forbidden
;; ConditionalCheckFailed	 Conditional check failed. Attribute (" + name + ") value exists.	409 Conflict
;; ConditionalCheckFailed	 Conditional check failed. Attribute ("+ name +") value is ("+ value +") but was expected ("+ expValue +")	409 Conflict
;; ExistsAndExpectedValue	 Expected.Exists=false and Expected.Value cannot be specified together	400 Bad Request
;; FeatureDeprecated	 The replace flag must be specified per attribute, not per item.	400 Bad Request
;; IncompleteExpectedExpression	 If Expected.Exists=true or unspecified, then Expected.Value has to be specified	400 Bad Request
;; InternalError	 Request could not be executed due to an internal service error.	500 Internal Server Error
;; InvalidAction	 The action " + actionName + " is not valid for this web service.	400 Bad Request
;; InvalidHTTPAuthHeader	 The HTTP authorization header is bad, use " + correctFormat".	400 Bad Request
;; InvalidHttpRequest	 The HTTP request is invalid. Reason: " + reason".	400 Bad Request
;; InvalidLiteral	 Illegal literal in the filter expression.	400 Bad Request
;; InvalidNextToken	 The specified next token is not valid.	400 Bad Request
;; InvalidNumberPredicates	 Too many predicates in the query expression.	400 Bad Request
;; InvalidNumberValueTests	 Too many value tests per predicate in the query expression.	400 Bad Request
;; InvalidParameterCombination	 The parameter " + param1 + " cannot be used with the parameter " + param2".	400 Bad Request
;; InvalidParameterValue	 Value (" + value + ") for parameter MaxNumberOfDomains is invalid. MaxNumberOfDomains must be between 1 and 100.	400 Bad Request
;; InvalidParameterValue	 Value (" + value + ") for parameter MaxNumberOfItems is invalid. MaxNumberOfItems must be between 1 and 2500.	400 Bad Request
;; InvalidParameterValue	 Value (" + value + ") for parameter MaxNumberOfDomains is invalid. MaxNumberOfDomains must be between 1 and 100.	400 Bad Request
;; InvalidParameterValue	 Value (" + value + ") for parameter " + paramName + " is invalid. " + reason".	400 Bad Request
;; InvalidParameterValue	 Value (" + value + ") for parameter Name is invalid. Value exceeds maximum length of 1024.	400 Bad Request
;; InvalidParameterValue	 Value (" + value + ") for parameter Value is invalid. Value exceeds maximum length of 1024.	400 Bad Request
;; InvalidParameterValue	 Value (" + value + ") for parameter DomainName is invalid.	400 Bad Request
;; InvalidParameterValue	 Value (" + value + ") for parameter Replace is invalid. The Replace flag should be either true or false.	400 Bad Request
;; InvalidParameterValue	 Value (" + value + ") for parameter Expected.Exists is invalid. Expected.Exists should be either true or false.	400 Bad Request
;; InvalidParameterValue	 Value (" + value + ") for parameter Name is invalid.The empty string is an illegal attribute name	400 Bad Request
;; InvalidParameterValue	 Value (" + value + ") for parameter Value is invalid. Value exceeds maximum length of 1024.	400 Bad Request
;; InvalidParameterValue	Value (" + value + ") for parameter ConsistentRead is invalid. The ConsistentRead flag should be either true or false.	400 Bad Request
;; InvalidQueryExpression	 The specified query expression syntax is not valid.	400 Bad Request
;; InvalidResponseGroups	 The following response groups are invalid: " + invalidRGStr.	400 Bad Request
;; InvalidService	 The Web Service " + serviceName + " does not exist.	400 Bad Request
;; InvalidSortExpression	 The sort attribute must be present in at least one of the predicates, and the predicate cannot contain the is null operator.	400 Bad Request
;; InvalidURI	 The URI " + requestURI + " is not valid.	400 Bad Request
;; InvalidWSAddressingProperty	 WS-Addressing parameter " + paramName + " has a wrong value: " + paramValue".	400 Bad Request
;; InvalidWSDLVersion	 Parameter (" + parameterName +") is only supported in WSDL version 2009-04-15 or beyond. Please upgrade to new version	400 Bad Request
;; MissingAction	 No action was supplied with this request.	400 Bad Request
;; MissingParameter	 The request must contain the specified missing parameter.	400 Bad Request
;; MissingParameter	 The request must contain the parameter " + paramName".	400 Bad Request
;; MissingParameter	The request must contain the parameter ItemName.	400 Bad Request
;; MissingParameter	The request must contain the parameter DomainName.	400 Bad Request
;; MissingParameter	Attribute.Value missing for Attribute.Name='name'.	400 Bad Request
;; MissingParameter	Attribute.Name missing for Attribute.Value='value'.	400 Bad Request
;; MissingParameter	No attributes for item ='" + itemName + "'.	400 Bad Request
;; MissingParameter	 The request must contain the parameter Name	400 Bad Request
;; MissingWSAddressingProperty	 WS-Addressing is missing a required parameter (" + paramName + ")".	400 Bad Request
;; MultipleExistsConditions	 Only one Exists condition can be specified	400 Bad Request
;; MultipleExpectedNames	 Only one Expected.Name can be specified	400 Bad Request
;; MultipleExpectedValues	 Only one Expected.Value can be specified	400 Bad Request
;; MultiValuedAttribute	 Attribute (" + name + ") is multi-valued. Conditional check can only be performed on a single-valued attribute	409 Conflict
;; NoSuchDomain	 The specified domain does not exist.	400 Bad Request
;; NoSuchVersion	 The requested version (" + version + ") of service " + service + " does not exist.	400 Bad Request
;; NotYetImplemented	 Feature " + feature + " is not yet available".	401 Unauthorized
;; NumberDomainsExceeded	 The domain limit was exceeded.	409 Conflict
;; NumberDomainAttributes

;; Exceeded
;; Too many attributes in this domain.	409 Conflict
;; NumberDomainBytesExceeded	 Too many bytes in this domain.	409 Conflict
;; NumberItemAttributes

;; Exceeded
;; Too many attributes in this item.	409 Conflict
;; NumberSubmitted

;; AttributesExceeded
;; Too many attributes in a single call.	409 Conflict
;; NumberSubmittedAttributesExceeded
;; Too many attributes for item itemName in a single call. Up to 256 attributes per call allowed.	409 Conflict
;; NumberSubmittedItemsExceeded
;; Too many items in a single call. Up to 25 items per call allowed.	409 Conflict
;; RequestExpired	 Request has expired. " + paramType + " date is " + date".	400 Bad Request
;; RequestTimeout	 A timeout occurred when attempting to query domain <domain name> with query expression <query expression>. BoxUsage [<box usage value>]".	408 Request Timeout
;; ServiceUnavailable	Service Amazon SimpleDB is busy handling other requests, likely due to too many simultaneous requests. Consider reducing the frequency of your requests, and try again. See About Response Code 503.	503 Service Unavailable
;; TooManyRequestedAttributes	 Too many attributes requested.	400 Bad Request
;; UnsupportedHttpVerb	 The requested HTTP verb is not supported: " + verb".	400 Bad Request
;; UnsupportedNextToken	 The specified next token is no longer supported. Please resubmit your query.	400 Bad Request
;; URITooLong	 The URI exceeded the maximum limit of "+ maxLength".	400 Bad Request
