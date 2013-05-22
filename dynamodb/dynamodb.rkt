;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2012  Raymond Paul Racine
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

#lang typed/racket/base

(provide
 create-table CreateTableResp
 delete-table describe-table
 get-item GetItemResp GetItemResp? GetItemResp-items
 put-item PutItemResp
 update-item UpdateItemResp
 delete-item DeleteItemResult
 scan Filter
 ScanResp ScanResp? ScanResp-lastkey
 ScanResp-consumed ScanResp-count
 ScanResp-scanned ScanResp-items
 ReturnValues
 list-tables ListTablesResp
 Exists
 Key Key? Key-name Key-type
 KeyVal KeyVal? KeyVal-value KeyVal-type
 ItemKey ItemKey? ItemKey-hashkey ItemKey-rangekey
 Item Item? Item-name Item-value Item-type
 ItemVal
 ItemUpdate
 Throughput Throughput? Throughput-read Throughput-write
 Range Range? Range-values Range-operator
 query QueryResp QueryResp? QueryResp-lastkey QueryResp-consumed QueryResp-count QueryResp-items)

(require
 (only-in "error.rkt"
	  AWSFailure)
 (only-in "types.rkt"
	  Throughput Throughput? Throughput-read Throughput-write
	  Key Key? Key-name Key-type
	  KeyVal KeyVal? KeyVal-value KeyVal-type
	  Exists Exists? ReturnValues
	  ItemKey ItemKey? ItemKey-hashkey ItemKey-rangekey
	  Item Item? Item-name Item-type Item-value
	  ItemUpdate ItemVal
	  Range Range? Range-values Range-operator)
 (only-in "createtable.rkt"
	  create-table CreateTableResp)
 (only-in "deletetable.rkt"
	  delete-table DeleteTableResp)
 (only-in "describetable.rkt"
	  describe-table DescribeTableResp DescribeTableResp?)
 (only-in "listtable.rkt"
	  list-tables ListTablesResp ListTablesResp?)
 (only-in "getitem.rkt"
	  get-item GetItemResp GetItemResp? GetItemResp-consumed GetItemResp-items)
 (only-in "putitem.rkt"
	  put-item
	  PutItemResp PutItemResp?)
 (only-in "deleteitem.rkt"
	  delete-item DeleteItemResult)
 (only-in "updateitem.rkt"
	  update-item UpdateItemResp)
 (only-in "scan.rkt"
	  scan Filter
	  ScanResp ScanResp? ScanResp-lastkey
	  ScanResp-consumed ScanResp-count
	  ScanResp-scanned ScanResp-items)
 (only-in "query.rkt"
	  query QueryResp QueryResp?
	  QueryResp-items QueryResp-count
	  QueryResp-consumed QueryResp-lastkey))
