;; Starwisp Copyright (C) 2013 Dave Griffiths
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


;; abstractions for synced databased

(msg "dbsync.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff in memory

(define (store-set store key value)
  (cond
   ((null? store) (list (list key value)))
   ((eq? key (car (car store)))
    (cons (list key value) (cdr store)))
   (else
    (cons (car store) (store-set (cdr store) key value)))))

(define (store-get store key default)
  (cond
   ((null? store) default)
   ((eq? key (car (car store)))
    (cadr (car store)))
   (else
    (store-get (cdr store) key default))))

(define (store-exists? store key)
  (cond
   ((null? store) #f)
   ((eq? key (car (car store)))
    #t)
   (else
    (store-exists? (cdr store) key))))

(define store '())

(define (set-current! key value)
  (set! store (store-set store key value)))

(define (get-current key default)
  (store-get store key default))

(define (current-exists? key)
  (store-exists? store key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; db abstraction

(define (entity-init! db table entity-type ktv-list)
  (entity-reset!)
  (entity-set! ktv-list)
  (set-current! 'db db)
  (set-current! 'table table)
  (set-current! 'entity-type entity-type))


;; store a ktv, replaces existing with same key
(define (entity-add-value! key type value)
  (set-current!
   'entity-values
   (ktv-set
    (get-current 'entity-values '())
    (ktv key type value))))

(define (entity-set! ktv-list)
  (set-current! 'entity-values ktv-list))

(define (entity-get-value key)
  (ktv-get (get-current 'entity-values '()) key))

;; version to check the entity has the key
(define (entity-set-value! key type value)
  (msg "entity-set-value!")
  (let ((existing-type (ktv-get-type (get-current 'entity-values '()) key)))
    (if (equal? existing-type type)
        (set-current!
         'entity-values
         (ktv-set
          (get-current 'entity-values '())
          (ktv key type value)))
        (msg "entity-set-value -" key "of type" type "doesn't exist on this entity"))
    (msg "done entity-set-value!")))

(define (date-time->string dt)
  (string-append
   (number->string (list-ref dt 0)) "-"
   (substring (number->string (+ (list-ref dt 1) 100)) 1 3) "-"
   (substring (number->string (+ (list-ref dt 2) 100)) 1 3) " "
   (substring (number->string (+ (list-ref dt 3) 100)) 1 3) ":"
   (substring (number->string (+ (list-ref dt 4) 100)) 1 3) ":"
   (substring (number->string (+ (list-ref dt 5) 100)) 1 3)))

;; build entity from all ktvs, insert to db, return unique_id
(define (entity-record-values!)
  (let ((db (get-current 'db #f))
        (table (get-current 'table #f))
        (type (get-current 'entity-type #f)))
    ;; standard bits
    (entity-add-value! "user" "varchar" (get-current 'user-id "none"))
    (entity-add-value! "time" "varchar" (date-time->string (date-time)))
    (entity-add-value! "lat" "real" (car (get-current 'location '(0 0))))
    (entity-add-value! "lon" "real" (cadr (get-current 'location '(0 0))))
    (entity-add-value! "deleted" "int" 0)
    (let ((values (get-current 'entity-values '())))
      (cond
       ((not (null? values))
        (let ((r (insert-entity/get-unique
                  db table type (get-current 'user-id "no id")
                  values)))
          (msg "inserted a " type)
          (entity-reset!) r))
       (else
        (msg "no values to add as entity!") #f)))
    ;; just to be on the safe side
    (entity-reset!)))

(define (entity-update-values!)
  (let ((db (get-current 'db #f))
        (table (get-current 'table #f)))
    ;; standard bits
    (let ((values (get-current 'entity-values '()))
          (unique-id (ktv-get (get-current 'entity-values '()) "unique_id")))
      (cond
       ((and unique-id (not (null? values)))
        (update-entity db table (entity-id-from-unique db table unique-id) values)
        (msg "updated " unique-id)
        (msg values)
        (entity-reset!))
       (else
        (msg "no values or no id to update as entity:" unique-id "values:" values))))))

(define (entity-reset!)
  (set-current! 'entity-values '())
  (set-current! 'db "reset")
  (set-current! 'table "reset")
  (set-current! 'entity-type "reset"))

(define (assemble-array entities)
  (foldl
   (lambda (i r)
     (if (equal? r "") (ktv-get i "unique_id")
         (string-append r "," (ktv-get i "unique_id"))))
   ""
   entities))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syncing code

(define url "http://192.168.2.1:8889/symbai?")

(msg "url")

(define (build-url-from-ktv ktv)
  (string-append "&" (ktv-key ktv) ":" (ktv-type ktv) "=" (stringify-value-url ktv)))

(define (build-url-from-ktvlist ktvlist)
  (foldl
   (lambda (ktv r)
     (string-append r (build-url-from-ktv ktv)))
   "" ktvlist))

(define (build-url-from-entity table e)
  (string-append
   url
   "fn=sync"
   "&table=" table
   "&entity-type=" (list-ref (car e) 0)
   "&unique-id=" (list-ref (car e) 1)
   "&dirty=" (number->string (list-ref (car e) 2))
   "&version=" (number->string (list-ref (car e) 3))
   (build-url-from-ktvlist (cadr e))))

;; todo fix all hardcoded paths here
(define (send-files ktvlist)
  (foldl
   (lambda (ktv r)
     (if (equal? (ktv-type ktv) "file")
         (cons (http-upload
                (string-append "upload-" (ktv-value ktv))
                "http://192.168.2.1:8889/symbai?fn=upload"
                (string-append "/sdcard/symbai/files/" (ktv-value ktv)))
               r)
         r))
   '() ktvlist))

(msg "spit")


;; spit all dirty entities to server
(define (spit db table entities)
  (foldl
   (lambda (e r)
     (debug! (string-append "Sending a " (car (car e)) " to Raspberry Pi"))
     (append
      (list
       (http-request
        (string-append "req-" (list-ref (car e) 1))
        (build-url-from-entity table e)
        (lambda (v)
          (cond
           ((or (equal? (car v) "inserted") (equal? (car v) "match"))
            (update-entity-clean db table (cadr v))
            (append
             (send-files e)
             (debug! (string-append "Uploaded " (car (car e))))))
           ((equal? (car v) "no change")
            (debug! (string-append "No change for " (car (car e)))))
           ((equal? (car v) "updated")
            ;; send new files hereish
            (update-entity-clean db table (cadr v))
            (append
             (send-files e)
             (debug! (string-append "Updated changed " (car (car e))))))
           (else
            (debug! (string-append
                     "Problem uploading "
                     (car (car e)) " : " (car v)))))
          (list
           (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty db))))))
      r))
   '()
   entities))

(msg "request files")

;; todo fix all hardcoded paths here
(define (request-files ktvlist)
  (foldl
   (lambda (ktv r)
     (if (equal? (ktv-type ktv) "file")
         (cons (http-download
                (string-append "download-" (ktv-value ktv))
                (string-append "http://192.168.2.1:8889/files/" (ktv-value ktv))
                (string-append "/sdcard/symbai/files/" (ktv-value ktv)))
               r)
         r))
   '() ktvlist))

(msg "suck ent")


(define (suck-entity-from-server db table unique-id exists)
  ;; ask for the current version
  (http-request
   (string-append unique-id "-update-new")
   (string-append url "fn=entity&table=" table "&unique-id=" unique-id)
   (lambda (data)
     ;; check "sync-insert" in sync.ss raspberry pi-side for the contents of 'entity'
     (let ((entity (list-ref data 0))
           (ktvlist (list-ref data 1)))
       (if (not exists)
           (insert-entity-wholesale
            db table
            (list-ref entity 0) ;; entity-type
            (list-ref entity 1) ;; unique-id
            0 ;; dirty
            (list-ref entity 2) ;; version
            ktvlist)
           (update-to-version
            db table (get-entity-id db table unique-id)
            (list-ref entity 2) ktvlist))
       (debug! (string-append (if exists "Got new: " "Updated: ") (ktv-get ktvlist "name")))
       (list
        (request-files ktvlist)
        (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty db)))))))

;; repeatedly read version and request updates
(define (suck-new db table)
  (debug! "Requesting new entities")
  (list
   (http-request
    "new-entities-req"
    (string-append url "fn=entity-versions&table=" table)
    (lambda (data)
      (let ((r (foldl
                (lambda (i r)
                  (let* ((unique-id (car i))
                         (version (cadr i))
                         (exists (entity-exists? db table unique-id))
                         (old
                          (if exists
                              (> version (get-entity-version
                                          db table
                                          (get-entity-id db table unique-id)))
                              #f)))
                    ;; if we don't have this entity or the version on the server is newer
                    (if (or (not exists) old)
                        (cons (suck-entity-from-server db table unique-id exists) r)
                        r)))
                '()
                data)))
        (cond
         ((null? r)
          (debug! "No new data to download")
          (set-current! 'download 1)
          (append
           (if (eqv? (get-current 'upload 0) 1)
               (list (play-sound "ping")) '())
           (list
            (toast "No new data to download")) r))
         (else
          (debug! (string-append
                   "Requesting "
                   (number->string (length r)) " entities"))
          (cons
           (play-sound "active")
           r))))))))

(msg "build-dirty defined...")

(define (build-dirty db)
  (let ((sync (get-dirty-stats db "sync"))
        (stream (get-dirty-stats db "stream")))
    (string-append
     "Sync data: " (number->string (car sync)) "/" (number->string (cadr sync)) " "
     "Stream data: " (number->string (car stream)) "/" (number->string (cadr stream)))))

(define (upload-dirty db)
  (let ((r (append
            (spit db "sync" (dirty-entities db "sync"))
            (spit db "stream" (dirty-entities db "stream")))))
    (append (cond
             ((> (length r) 0)
              (debug! (string-append "Uploading " (number->string (length r)) " items..."))
              (list
               (toast "Uploading data...")
               (play-sound "active")))
             (else
              (debug! "No data changed to upload")
              (set-current! 'upload 1)
              (append
               (if (eqv? (get-current 'download 0) 1)
                   (list (play-sound "ping")) '())
               (list
                (toast "No data changed to upload"))))) r)))

(define (connect-to-net fn)
  (list
   (network-connect
    "network"
    "mongoose-web"
    (lambda (state)
      (debug! (string-append "Raspberry Pi connection state now: " state))
      (append
       (if (equal? state "Connected") (fn) '())
       (list
        ;;(update-widget 'text-view (get-id "sync-connect") 'text state)
        ))))))
