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


;; colours


(define trans-col (list 0 0 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent database

(define db "/sdcard/starwisp/local-symbai.db")
(db-open db)
(setup db "local")
(setup db "sync")
(setup db "stream")

(insert-entity-if-not-exists
 db "local" "app-settings" "null" 1
 (list
  (ktv "user-id" "varchar" "No name yet...")))

;;(display (db-all db "local" "app-settings"))(newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user interface abstraction

;;;;;;;;;;;;; i18n ;;;;;;;;;;;;;;;;;;;;;;

(define i18n-lang 0)

(define i18n-text
  (list
   (list 'title (list "Symbai" "Symbai" "Symbai"))
   (list 'sync (list "Sync" "Sync" "Sync"))
   (list 'languages (list "Choose language" "Choose language" "Choose language"))
   (list 'english (list "English" "English" "English"))
   (list 'khasi (list "Khasi" "Khasi" "Khasi"))
   (list 'hindi (list "Hindi" "Hindi" "Hindi"))
   (list 'user-id (list "User ID" "User ID" "User ID"))
   (list 'ok (list "Ok" "Ok" "Ok"))
   (list 'cancel (list "Cancel" "Cancel" "Cancel"))
   ))

(define (mtext-lookup id)
  (alog id)
  (define (_ l)
    (cond
     ((null? l) (string-append (symbol->string id) " not translated"))
     ((eq? (car (car l)) id) (list-ref (cadr (car l)) i18n-lang))
     (else (_ (cdr l)))))
  (_ i18n-text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mbutton id title fn)
  (button (make-id id)
          (mtext-lookup title)
          40 (layout 'fill-parent 'wrap-content -1 'left 5) fn))

(define (mbutton-scale id title fn)
  (button (make-id id)
          (mtext-lookup title)
          40 (layout 'fill-parent 'wrap-content 1 'left 5) fn))

(define (mtoggle-button id title fn)
  (toggle-button (make-id id)
                 (mtext-lookup title)
                 30 (layout 'fill-parent 'wrap-content 1 'left 0) "fancy" fn))

(define (mtoggle-button-yes id title fn)
  (toggle-button (make-id id)
                 (mtext-lookup title)
                 30 (layout 49 43 1 'left 0) "yes" fn))

(define (mtoggle-button-maybe id title fn)
  (toggle-button (make-id id)
                 (mtext-lookup title)
                 30 (layout  49 43 1 'left 0) "maybe" fn))

(define (mtoggle-button-no id title fn)
  (toggle-button (make-id id)
                 (mtext-lookup title)
                 30 (layout  49 43 1 'left 0) "no" fn))

(define (mtext id text)
  (text-view (make-id id)
             (mtext-lookup text)
             30 (layout 'wrap-content 'wrap-content -1 'left 0)))

(define (mtext-scale id text)
  (text-view (make-id id)
             (mtext-lookup text)
             30 (layout 'wrap-content 'wrap-content 1 'left 0)))

(define (mtitle id text)
  (text-view (make-id id)
             (mtext-lookup text)
             50 (layout 'fill-parent 'wrap-content -1 'left 0)))

(define (medit-text id text type fn)
  (vert
   (mtext (string-append id "-title") text)
   (edit-text (make-id id) "" 30 type fillwrap fn)))

(define (mspinner id name types fn)
   (vert
    (mtext "" name)
    (spinner (make-id (string-append id "-spinner")) (map mtext-lookup types) fillwrap
             (lambda (c) (fn c)))))

(define (mspinner-other id name types fn)
  (horiz
   (mspinner id name types fn (lambda (c) (fn c)))
   (vert
    (mtext "" 'other)
    (edit-text (make-id (string-append id "-edit-text")) "" 30 "normal" fillwrap
               (lambda (t) (fn t))))))


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

;;;;

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


(define (force-pause)
  (list
   (delayed "timer" 1000 (lambda () '()))
   (update-widget 'toggle-button (get-id "pf-pause") 'checked 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fragments

(define-fragment-list

  (fragment
   "top"
   (horiz
    (image-view 0 "face" (layout 48 64 -1 'left 0))
    (text-view (make-id "") 'title 30
               (layout 'fill-parent 'fill-parent 0.25 'centre 10))

    (linear-layout
     0 'vertical
     (layout 'fill-parent 'wrap-content 0.75 'left 0)
     (list 0 0 0 0)

     (list
      (text-view (make-id "") 'name 20
                 (layout 'fill-parent 'wrap-content 1 'left 0))
      (text-view (make-id "") 'photoid 20
                 (layout 'fill-parent 'wrap-content 1 'left 0)))))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))


  (fragment
   "bottom"
   (linear-layout
    0 'horizontal
    (layout 'fill-parent 'fill-parent 1 'left 0)
    (list 0 0 0 0)
    (list
     (mbutton-scale "cancel" 'cancel (lambda () (list)))
     (mbutton-scale "ok" 'ok (lambda () (list)))))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))


  )

(msg "one")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; activities


(define-activity-list

  (activity
   "main"

   (vert-fill
    (relative
     '(("parent-top"))
     (list 100 100 255 127)
     (build-fragment "top" (make-id "top") fillwrap))

    (scroll-view-vert
     0 (layout 'fill-parent 'fill-parent 1 'left 0)
     (list
      (vert-fill

       (mtitle "" 'title)

       (horiz
        (medit-text "user-id" 'user-id "normal" (lambda () (list)))
        (mbutton-scale "sync" 'sync (lambda () (list))))

       (mspinner "languages" 'languages (list 'english 'khasi 'hindi) (lambda (c) (list)))
       (mbutton "new-village" 'new-village (lambda () (list (start-activity "village" 0 ""))))

       )))

    (relative
     '(("parent-bottom"))
     (list 100 100 255 127)
     (vert
      (spacer 5)
      (build-fragment "bottom" (make-id "bottom") fillwrap)))
    )

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


  (activity
   "village"

   (vert-fill
    (relative
     '(("parent-top"))
     (list 100 100 255 127)
     (build-fragment "top" (make-id "top") fillwrap))

    (scroll-view-vert
     0 (layout 'fill-parent 'fill-parent 1 'left 0)
     (list
      (vert-fill
       ;;(image-view (make-id "face") "face" (layout 640 470 1 'left 0))
       (mtitle "" 'title)
       (mtext "" 'database)
       (mbutton "main-sync" 'sync (lambda () (list (start-activity "sync" 0 ""))))
       (mspinner-other "test" 'test (list 'one 'two 'three) (lambda (c) (list)))
       (mbutton "main-sync" 'sync (lambda () (list (start-activity "sync" 0 ""))))
       (mspinner-other "test" 'test (list 'one 'two 'three) (lambda (c) (list)))
       (mbutton "main-sync" 'sync (lambda () (list (start-activity "sync" 0 ""))))
       (mspinner-other "test" 'test (list 'one 'two 'three) (lambda (c) (list)))
       (mbutton "main-sync" 'sync (lambda () (list (start-activity "sync" 0 ""))))
       (mspinner-other "test" 'test (list 'one 'two 'three) (lambda (c) (list)))
       (mbutton "main-sync" 'sync (lambda () (list (start-activity "sync" 0 ""))))
       (mspinner-other "test" 'test (list 'one 'two 'three) (lambda (c) (list)))
       (mbutton "main-sync" 'sync (lambda () (list (start-activity "sync" 0 ""))))
       (mspinner-other "test" 'test (list 'one 'two 'three) (lambda (c) (list)))
       (mbutton "main-sync" 'sync (lambda () (list (start-activity "sync" 0 ""))))
       (mspinner-other "test" 'test (list 'one 'two 'three) (lambda (c) (list)))
       )))

    (relative
     '(("parent-bottom"))
     (list 100 100 255 127)
     (vert
      (spacer 5)
      (build-fragment "bottom" (make-id "bottom") fillwrap)))
    )
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

  (activity
   "sync"
   (vert
    )

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'sync-on #f)
     (append
      (debug-timer-cb)
      (list
       (update-widget 'debug-text-view (get-id "sync-debug") 'text (get-current 'debug-text ""))
       (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty))
       )))
   (lambda (activity) '())
   (lambda (activity) (list (delayed "debug-timer" 1000 (lambda () '()))))
   (lambda (activity) (list (delayed "debug-timer" 1000 (lambda () '()))))
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))



  )
