#!/usr//bin/env mzscheme
#lang scheme/base
;; Naked on Pluto Copyright (C) 2010 Aymeric Mansoux, Marloes de Valk, Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require scheme/system
         scheme/foreign
         scheme/cmdline
         web-server/servlet
         web-server/servlet-env
         web-server/http/response-structs
	 racket/match
         "scripts/request.ss"
         "scripts/logger.ss"
         "scripts/json.ss"
         "scripts/sync.ss"
         "scripts/utils.ss"
         "scripts/eavdb.ss"
         "scripts/txt.ss"
;         "scripts/input.ss"
	 )

; a utility to change the process owner,
; assuming mzscheme is called by root.
;;(unsafe!)
;;(define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)))

(define db-name "client/htdocs/symbai.db")
(define db (db-open db-name))
(open-log "log.txt")

;(write-db db "sync" "/home/dave/code/mongoose-web/web/input.csv")

(define registered-requests
  (list

   (register
    (req 'ping '())
    (lambda (req)
      (pluto-response (scheme->txt '("hello")))))

   (register
    (req 'upload '())
    (lambda (req)
      (match (bindings-assq #"binary" (request-bindings/raw req))
	     ((struct binding:file (id filename headers content))	      
	      (with-output-to-file 
		  (string-append "files/" (bytes->string/utf-8 filename)) #:exists 'replace
		  (lambda ()
		    (write-bytes content)))))
      (pluto-response (scheme->txt '("ok")))))

   ;; http://localhost:8888/mongoose?fn=sync&table=sync&entity-type=mongoose&unique-id=dave1234&dirty=1&version=0&next:varchar=%22foo%22&blah:int=20

   (register
    (req 'sync '(table entity-type unique-id dirty version))
    (lambda (req table entity-type unique-id dirty version . data)
      (pluto-response
       (scheme->txt
        (check-for-sync
         db
         table
         entity-type
         unique-id
         (string->number dirty)
         (string->number version) data)))))

   (register
    (req 'entity-versions '(table))
    (lambda (req table)
      (pluto-response
       (scheme->txt
        (entity-versions db table)))))

   (register
    (req 'entity '(table unique-id))
    (lambda (req table unique-id)
      (pluto-response
       (scheme->txt
        (send-entity db table unique-id)))))

   (register
    (req 'entity-types '(table))
    (lambda (req table)
      (pluto-response
       (scheme->txt
        (get-all-entity-types db table)))))

   (register
    (req 'entity-csv '(table type))
    (lambda (req table type)
      (let ((r (csv db table type)))
	(msg "--------------------------------------- csv request for" type "[" r "]")
	(pluto-response
	 r))))

   ))

(define (start request)
  (msg "request")
  (let ((values (url-query (request-uri request))))
    (if (not (null? values))   ; do we have some parameters?
        (let ((name (assq 'fn values)))
          (if name           ; is this a well formed request?
	      (request-dispatch
	       registered-requests
	       (req (string->symbol (cdr name))
		    (filter
		     (lambda (v)
		       (not (eq? (car v) 'fn)))
		     values))
	       request)
	      (pluto-response "could't find a function name")))
        (pluto-response "malformed thingy"))))

(printf "server is running...~n")

; Here we become the user 'nobody'.
; This is a security rule that *only works* if nobody owns no other processes
; than mzscheme. Otherwise better create another dedicated unprivileged user.
; Note: 'nobody' must own the state directory and its files.

;(setuid 65534)

;;

(serve/servlet
 start
 ;; port number is read from command line as argument
 ;; ie: ./server.scm 8080
 #:listen-ip "192.168.2.1"
 #:port (string->number (command-line #:args (port) port))
 #:command-line? #t
 #:servlet-path "/symbai"
 #:server-root-path
 (build-path "client"))
