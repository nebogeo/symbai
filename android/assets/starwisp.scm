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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strings

(define obs-gc "Group Composition")
(define obs-pf "Pup Focal")
(define obs-gp "Group Events")

(define entity-types
  (list
   "pup-focal"
   "pup-focal-nearest"
   "pup-focal-pupfeed"
   "pup-focal-pupfind"
   "pup-focal-pupcare"
   "pup-focal-pupaggr"
   "group-interaction"
   "group-alarm"
   "group-move"))

;; colours

(define pf-col (list 255 204 51 255))
(define gp-col (list 255 102 0 255))
(define gc-col (list 164 82 9 255))

(define pf-bgcol (list 255 204 51 127))
(define gp-bgcol (list 255 102 0 127))
(define gc-bgcol (list 164 82 9 127))

;(define pf-col (list  22  19 178  127))
;(define gp-col (list 255  97   0  127))
;(define gc-col (list 255 236   0  127))



(define trans-col (list 0 0 0 0))

(define (get-fragment-index name frag)
  (define (_ i l)
    (cond
     ((null? l) 0)
     ((equal? name (cadr (car l))) i)
     (else (_ (+ i 1) (cdr l)))))
  (_ 0 frag))

(define gc-fragments
  (list
   (list "Start" "gc-start")
   (list "Weights" "gc-weights")
   (list "Pregnant" "gc-preg")
   (list "Pup assoc" "gc-pup-assoc")
   (list "Oestrus" "gc-oestrus")
   (list "Babysit" "gc-babysitting")
   (list "End" "gc-end")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent database

(define db "/sdcard/opensauces/local-mongoose.db")
(db-open db)
(setup db "local")
(setup db "sync")
(setup db "stream")

(insert-entity-if-not-exists
 db "local" "app-settings" "null" 1
 (list
  (ktv "user-id" "varchar" "No name yet...")))

(display (db-all db "local" "app-settings"))(newline)

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

;; store a ktv, replaces existing with same key
(define (entity-add-value! key type value)
  (set-current!
   'entity-values
   (ktv-set
    (get-current 'entity-values '())
    (ktv key type value))))

(define (entity-set! ktv-list)
  (set-current! 'entity-values ktv-list))

(define (date-time->string dt)
  (string-append
   (number->string (list-ref dt 0)) "-"
   (number->string (list-ref dt 1)) "-"
   (number->string (list-ref dt 2)) " "
   (number->string (list-ref dt 3)) ":"
   (number->string (list-ref dt 4)) ":"
   (substring (number->string (+ 100 (list-ref dt 5))) 1 2)))

;; build entity from all ktvs, insert to db, return unique_id
(define (entity-record-values db table type)
  ;; standard bits
  (entity-add-value! "user" "varchar" (get-current 'user-id "none"))
  (entity-add-value! "time" "varchar" (date-time->string (date-time)))
  (entity-add-value! "lat" "real" (car (get-current 'location '(0 0))))
  (entity-add-value! "lon" "real" (cadr (get-current 'location '(0 0))))
  (let ((values (get-current 'entity-values '())))
    (cond
     ((not (null? values))
      (let ((r (insert-entity/get-unique
                db table type (get-current 'user-id "no id")
                values)))
        (msg "inserted a " type)
        (entity-reset!) r))
     (else
      (msg "no values to add as entity!") #f))))

(define (entity-update-values db table)
  ;; standard bits
  (let ((values (get-current 'entity-values '()))
        (unique-id (ktv-get (get-current 'entity-values '()) "unique_id")))
    (cond
     ((and unique-id (not (null? values)))
      (update-entity db table (entity-id-from-unique db table unique-id) values)
      (msg "updated " unique-id)
      (entity-reset!))
     (else
      (msg "no values or no id to update as entity:" unique-id "values:" values)))))

(define (entity-reset!)
  (set-current! 'entity-values '()))

(define (assemble-array entities)
  (foldl
   (lambda (i r)
     (if (equal? r "") (ktv-get i "unique_id")
         (string-append r "," (ktv-get i "unique_id"))))
   ""
   entities))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syncing code

(define url "http://192.168.2.1:8888/mongoose?")

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
            (debug! (string-append "Uploaded " (car (car e)))))
           ((equal? (car v) "no change")
            (debug! (string-append "No change for " (car (car e)))))
           ((equal? (car v) "updated")
            (update-entity-clean db table (cadr v))
            (debug! (string-append "Updated changed " (car (car e)))))
           (else
            (debug! (string-append
                     "Problem uploading "
                     (car (car e)) " : " (car v)))))
          (list
           (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty))))))
      r))
   '()
   entities))

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
        (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty)))))))

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

(define (build-dirty)
  (let ((sync (get-dirty-stats db "sync"))
        (stream (get-dirty-stats db "stream")))
    (string-append
     "Pack data: " (number->string (car sync)) "/" (number->string (cadr sync)) " "
     "Focal data: " (number->string (car stream)) "/" (number->string (cadr stream)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user interface abstraction

(define (mbutton id title fn)
  (button (make-id id) title 15 fillwrap fn))

(define (mbutton2 id title fn)
  (button (make-id id) title 30 (layout 150 100 1 'centre 0) fn))

(define (mtoggle-button id title fn)
  (toggle-button (make-id id) title 30 (layout 'fill-parent 'wrap-content 1 'centre 0) "fancy" fn))

(define (mtoggle-button-yes id title fn)
  (toggle-button (make-id id) title 30 (layout 49 43 1 'centre 0) "yes" fn))

(define (mtoggle-button-maybe id title fn)
  (toggle-button (make-id id) title 30 (layout  49 43 1 'centre 0) "maybe" fn))

(define (mtoggle-button-no id title fn)
  (toggle-button (make-id id) title 30 (layout  49 43 1 'centre 0) "no" fn))

(define (mtoggle-button2 id title fn)
  (toggle-button (make-id id) title 30 (layout 150 100 1 'centre 0) "plain" fn))

(define (mtext id text)
  (text-view (make-id id) text 30 wrap))

(define (mtitle id text)
  (text-view (make-id id) text 50 (layout 'fill-parent 'wrap-content 1 'centre 0)))

(define (medit-text id text type fn)
  (vert
   (mtext (string-append id "-title") text)
   (edit-text (make-id id) "" 30 type fillwrap fn)))

(define (mclear-toggles id-list)
  (map
   (lambda (id)
     (update-widget 'toggle-button (get-id id) 'checked 0))
   id-list))

(define (mclear-toggles-not-me me id-list)
  (foldl
   (lambda (id r)
     (if (equal? me id)
         r (cons (update-widget 'toggle-button (get-id id) 'checked 0) r)))
   '() id-list))

(define (xwise n l)
  (define (_ c l)
    (cond
      ((null? l) (if (null? c) '() (list c)))
      ((eqv? (length c) (- n 1))
       (cons (append c (list (car l))) (_ '() (cdr l))))
      (else
       (_ (append c (list (car l))) (cdr l)))))
  (_ '() l))

;;;;

(define (build-grid-selector name type title)
  (linear-layout
   0 'vertical
   (layout 'fill-parent 'wrap-content 1 'left 0)
   (list 0 0 0 0)
   (list
    (mtext "title" title)
    (linear-layout
     0 'horizontal
     (layout 'fill-parent 'wrap-content 1 'left 2) trans-col
     (list
      (image-view (make-id "im") "arrow_left" (layout 200 'fill-parent 1 'left 0))
      (scroll-view
       (make-id "scroller")
       (layout 'wrap-content 'wrap-content 1 'left 20)
       (list
        (linear-layout
         (make-id name) 'horizontal
         (layout 'wrap-content 'wrap-content 1 'centre 20) trans-col
         (list
          (button-grid (make-id name) type 3 30 (layout 100 60 1 'left 40)
                       (list) (lambda (v) '()))))))
      (image-view (make-id "im") "arrow_right" (layout 200 'fill-parent 1 'right 0)))))))

;; assumes grid selectors on mongeese only
(define (fast-get-name item)
  (list-ref (list-ref item 1) 2))

(define (build-button-items name items unknown)
  (append
   (map
    (lambda (item)
      (let ((item-name (fast-get-name item)))
        (list (make-id (string-append name item-name))
              item
              item-name)))
    items)
   (if unknown
       (list
        (list (make-id (string-append name "-unknown"))
              (list (ktv "name" "varchar" "Unknown")
                    (ktv "unique_id" "varchar" "Unknown"))
              "???"))
       '())))

(define (populate-grid-selector name type items unknown fn)
  (prof-start "popgrid")
  (prof-start "popgrid setup")
  (let ((id->items (build-button-items name items unknown))
        (selected-set '()))
    (prof-end "popgrid setup")
    (let ((r (update-widget
     'button-grid (get-id name) 'grid-buttons
     (list
      type 3 30 (layout 100 60 1 'left 0)
      (map
       (lambda (ii)
         (dbg (list (car ii) (caddr ii))))
       id->items)
      (lambda (v state)
        (cond
         ((equal? type "toggle")
          ;; update list of selected items
          (if state
              (set! selected-set (set-add v selected-set))
              (set! selected-set (set-remove v selected-set)))
          ;; find all items currently selected
          (fn (map
               (lambda (v)
                 (cadr (findv v id->items)))
               selected-set)))
         (else
          ;;(msg (findv v id->items))
          (fn (cadr (findv v id->items))))))))))
      (prof-end "popgrid")
      r)))

(define (db-mongooses-by-pack)
  (db-all-where
   db "sync" "mongoose"
   (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))))

(define (db-mongooses-by-pack-male)
  (db-all-where2or
   db "sync" "mongoose"
   (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
   (ktv "gender" "varchar" "Male") "Unknown"))

(define (db-mongooses-by-pack-female)
  (db-all-where2or
   db "sync" "mongoose"
   (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
   (ktv "gender" "varchar" "Female") "Unknown"))


;; (y m d h m s)
(define (date-minus-months d ms)
  (let ((year (list-ref d 0))
        (month (- (list-ref d 1) 1)))
    (let ((new-month (- month ms)))
      (list
       (if (< new-month 0) (- year 1) year)
       (+ (if (< new-month 0) (+ new-month 12) new-month) 1)
       (list-ref d 2)
       (list-ref d 3)
       (list-ref d 4)
       (list-ref d 5)))))

(define (db-mongooses-by-pack-pups)
  (db-all-newer
   db "sync" "mongoose"
   (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
   (ktv "dob" "varchar" (date->string (date-minus-months (date-time) 6)))))

(define (db-mongooses-by-pack-adults)
  (db-all-older
   db "sync" "mongoose"
   (ktv "pack-id" "varchar" (ktv-get (get-current 'pack '()) "unique_id"))
   (ktv "dob" "varchar" (date->string (date-minus-months (date-time) 6)))))



(define (tri-state id text key)
  (linear-layout
   (make-id "") 'vertical (layout 'fill-parent 'wrap-content '1 'centre 0) trans-col
   (list
    (linear-layout
     (make-id "") 'horizontal (layout 'wrap-content 'wrap-parent '1 'centre 0) trans-col
     (list
      (mtoggle-button-yes
       (string-append id "-y") ""
       (lambda (v)
         (cond
          (v
           (entity-add-value! key "varchar" "yes")
           (list
            (update-widget 'toggle-button (get-id (string-append id "-n")) 'checked 0)
            (update-widget 'toggle-button (get-id (string-append id "-m")) 'checked 0)))
               (else
                (list
                 (update-widget 'toggle-button (get-id (string-append id "-y")) 'checked 1))))
              ))
      (mtoggle-button-maybe
       (string-append id "-m")  ""
       (lambda (v)
         (cond
          (v
           (entity-add-value! key "varchar" "maybe")
           (list
            (update-widget 'toggle-button (get-id (string-append id "-y")) 'checked 0)
            (update-widget 'toggle-button (get-id (string-append id "-n")) 'checked 0)))
          (else
           (list
            (update-widget 'toggle-button (get-id (string-append id "-m")) 'checked 1))))
         ))

      (mtoggle-button-no
       (string-append id "-n") ""
       (lambda (v)
         (cond
          (v
           (entity-add-value! key "varchar" "no")
           (list
            (update-widget 'toggle-button (get-id (string-append id "-y")) 'checked 0)
            (update-widget 'toggle-button (get-id (string-append id "-m")) 'checked 0)))
          (else
           (list
            (update-widget 'toggle-button (get-id (string-append id "-n")) 'checked 1))))
         ))))

    (text-view 0 text 30 (layout 'wrap-content 'wrap-parent '1 'centre 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define (debug! txt)
  (set-current! 'debug-text (string-append txt "\n" (get-current 'debug-text ""))))

(define (update-debug)
  (update-widget 'debug-text-view (get-id "sync-debug") 'text
                 (get-current 'debug-text "")))

(define (debug-timer-cb)
  (append
   (cond
    ((get-current 'sync-on #f)
     (set-current! 'upload 0)
     (set-current! 'download 0)
     (connect-to-net
      (lambda ()
        (append
         (list (toast "sync-cb"))
         (upload-dirty db)
         (suck-new db "sync")))))
    (else '()))
   (list
    (delayed "debug-timer" (+ 5000 (random 5000)) debug-timer-cb)
    (update-debug))))


(define pf-length 20) ;; minutes...

(define (timer-cb)
  (set-current!
   'timer-seconds
   (- (get-current 'timer-seconds 59) 1))
  (append
   (cond
    ((< (get-current 'timer-seconds 59) 0)
     (set-current! 'timer-minutes (- (get-current 'timer-minutes pf-length) 1))
     (set-current! 'timer-seconds 59)
     (cond ((< (get-current 'timer-minutes pf-length) 1)
            (list
             (alert-dialog
              "pup-focal-end"
              "Pup focal time is up, have you finished?"
              (lambda (v)
                (cond
                 ((eqv? v 1)
                  (list (finish-activity 1)))
                 (else
                  (set-current! 'timer-minutes 1)
                  (list)))))))
           (else
            (list (replace-fragment (get-id "pf-top") "pf-scan1")))))
    (else '()))
   (list
    (delayed "timer" 1000 timer-cb)
    (update-widget
     'text-view (get-id "pf-timer-time-minutes") 'text
     (string-append (number->string (get-current 'timer-minutes pf-length))))
    (update-widget
     'text-view (get-id "pf-timer-time") 'text
     (string-append (number->string (get-current 'timer-seconds 59))))
    )))

(define (next-button id dialog-msg next-frag fn)
     (mbutton (string-append id "-nextb") "Next"
              (lambda ()
                (list
                 (alert-dialog
                  (string-append id "-d")
                  dialog-msg
                  (lambda (v)
                    (cond
                     ((eqv? v 1)
                      (append
                       (fn) (list (replace-fragment
                                   (get-id "gc-top") next-frag))))
                     (else '()))))))))

(define (force-pause)
  (list
   (delayed "timer" 1000 (lambda () '()))
   (update-widget 'toggle-button (get-id "pf-pause") 'checked 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fragments

(define-fragment-list

  (fragment
   "pf-timer"
   (linear-layout
    (make-id "") 'vertical fillwrap trans-col
    (list
     (mtitle "pf-details" "Pack: xxx Pup: xxx")))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (update-widget 'text-view (get-id "pf-details") 'text
                     (string-append
                      "Pack: " (ktv-get (get-current 'pack '()) "name") " "
                      "Pup: " (ktv-get (get-current 'individual '()) "name"))
                     )))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))




  )

(msg "one")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; activities

(define ingredients (list "water" "quail eggs" "black tea leaves" "apple wood chips"))
(define methods (list "boil" "smoke" "simmer" "strain" "dry" "serve" "add"))

(define (ingredient id)
  (draggable
   (make-id (string-append id "in1")) 'horizontal wrap (list 255 255 127 127)
   (list
    (spinner (make-id "sin1") ingredients wrap (lambda (v) '())))
   (lambda () (string-append "ingredient-" id))))

(define (method id)
  (draggable
   (make-id (string-append id "m1")) 'vertical wrap (list 225 127 80 127)
   (list
    (spinner (make-id "sm1") methods wrap (lambda (v) '())))
   (lambda () (string-append "method-" id))))

(define (note id)
  (draggable
   (make-id (string-append id "note1")) 'horizontal wrap (list 225 127 127 127)
   (list
    (edit-text (make-id "sm1") "NOTE: Do not peel the eggs, the shell is edible and has absorbed the taste of the smoke and tea." 20 "plain" wrap (lambda () '())))
   (lambda () (string-append "note-" id))))

(define did 100)
(define (new-id)
  (set! did (+ did 1))
  (number->string did))

(define-activity-list

  (activity
   "main"
   (vert
    (linear-layout
     0 'vertical (layout 'fill-parent 'wrap-content 1 'centre 0) (list 255 255 255 127)
     (list
      (mtitle "" "Open Sauces Notebook")))
    (horiz
     (mbutton "eval" "Parse me"
              (lambda ()
                (list (walk-draggable
                       "eval" 99
                       (lambda (v)
                         (list (toast v)))))))
     (mbutton "add-ingredient" "Add ingredient"
              (lambda ()
                (list (update-widget 'draggable 99 'contents (list (ingredient (new-id))
                 )))))
     (mbutton "add-method" "Add method"
              (lambda ()
                (list (update-widget 'draggable 99 'contents (list (method (new-id))
                 )))))
     (mbutton "add-note" "Add note"
              (lambda ()
                (list (update-widget 'draggable 99 'contents (list (note (new-id))
                 ))))))
    (draggable
     99 'vertical (layout 'fill-parent 'fill-parent 1 'left 0) (list 255 255 255 127)
      (list
      (ingredient "4")
      (ingredient "5")
      (method "4")
      (method "5")
      (note "1")
      )
     (lambda (v) '())))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (let ((user-id (ktv-get (get-entity db "local" 1) "user-id")))
       (set-current! 'user-id user-id)
       (list)))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  )
