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
(msg "starting up....")
(define entity-types (list "village" "household" "individual"))

(define trans-col (list 0 0 0 0))
(define colour-one (list 0 0 255 100))
(define colour-two (list  127 127 255 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent database

(define db "/sdcard/symbai/local-symbai.db")
(db-open db)
(setup db "local")
(setup db "sync")
(setup db "stream")

(define settings-entity-id-version 2)

(insert-entity-if-not-exists
 db "local" "app-settings" "null" settings-entity-id-version
 (list
  (ktv "user-id" "varchar" "not set")
  (ktv "language" "int" 0)
  (ktv "house-id" "int" 0)
  (ktv "photo-id" "int" 0)
  (ktv "current-village" "varchar" "none")))

(define (get-setting-value name)
  (ktv-get (get-entity db "local" settings-entity-id-version) name))

(define (set-setting! key type value)
  (update-entity
   db "local" settings-entity-id-version (list (ktv key type value))))

(define (get/inc-setting key)
  (let ((r (get-setting-value key)))
    (set-setting! key "int" (+ r 1))
    r))

(set-current! 'user-id (get-setting-value "user-id"))
(set! i18n-lang (get-setting-value "language"))

;;(display (db-all db "local" "app-settings"))(newline)

(define tribes-list '(khasi other))
(define subtribe-list '(khynriam pnar bhoi war other))
(define education-list   '(primary middle high secondary university))
(define married-list '(currently-married currently-single seperated))
(define residence-list '(birthplace spouse-village))
(define gender-list '(male female))
(define house-type-list '(concrete tin thatched other))

(define social-types-list '(knowledge prestige))
(define social-relationship-list '(mother father sister brother spouse children co-wife spouse-mother spouse-father spouse-brother-wife spouse-sister-husband friend neighbour other))
(define social-residence-list '(same other))
(define social-strength-list '(daily weekly monthly less))

(define village-ktvlist
  (list
   (ktv "name" "varchar" (mtext-lookup 'default-village-name))
   (ktv "block" "varchar" "")
   (ktv "district" "varchar" "")
   (ktv "school-closest-access" "varchar" "")
   (ktv "hospital-closest-access" "varchar" "")
   (ktv "post-office-closest-access" "varchar" "")
   (ktv "railway-station-closest-access" "varchar" "")
   (ktv "state-bus-service-closest-access" "varchar" "")
   (ktv "district-bus-service-closest-access" "varchar" "")
   (ktv "panchayat-closest-access" "varchar" "")
   (ktv "ngo-closest-access" "varchar" "")
   (ktv "market-closest-access" "varchar" "")
   (ktv "car" "int" 0)))

(define household-ktvlist
  (list
   (ktv "name" "varchar" "")
   (ktv "num-pots" "int" 0)
   (ktv "num-children" "int" 0)
   (ktv "house-lat" "real" 0) ;; get from current location?
   (ktv "house-lon" "real" 0)
   (ktv "toilet-lat" "real" 0)
   (ktv "toilet-lon" "real" 0)))

(define individual-ktvlist
  (list
   (ktv "name" "varchar" "")
   (ktv "first-name" "varchar" "")
   (ktv "family" "varchar" "")
   (ktv "photo-id" "varchar" "")
   (ktv "photo" "file" "")
   (ktv "tribe" "varchar" "")
   (ktv "subtribe" "varchar" "")
   (ktv "child" "int" -1)
   (ktv "age" "int" -1)
   (ktv "gender" "varchar" "")
   (ktv "literate" "int" 0)
   (ktv "education" "varchar" "")
   (ktv "head-of-house" "varchar" "")
   (ktv "marital-status" "varchar" "")
   (ktv "times-married" "int" -1)
   (ktv "id-spouse" "varchar" "")
   (ktv "children-living" "int" -1)
   (ktv "children-dead" "int" -1)
   (ktv "children-together" "int" -1)
   (ktv "children-apart" "int" -1)
   (ktv "residence-after-marriage" "varchar" "")
   (ktv "num-siblings" "int" -1)
   (ktv "birth-order" "int" -1)
   (ktv "length-time" "int" -1)
   (ktv "place-of-birth" "varchar" "")
   (ktv "num-residence-changes" "int" -1)
   (ktv "village-visits-month" "int" -1)
   (ktv "village-visits-year" "int" -1)
   (ktv "occupation-agriculture" "int" 0)
   (ktv "occupation-gathering" "int" 0)
   (ktv "occupation-labour" "int" 0)
   (ktv "occupation-cows" "int" 0)
   (ktv "occupation-fishing" "int" 0)
   (ktv "occupation-other" "varchar" "")
   (ktv "contribute" "int" 0)
   (ktv "own-land" "int" 0)
   (ktv "rent-land" "int" 0)
   (ktv "hire-land" "int" 0)
   (ktv "house-type" "varchar" "")
   (ktv "loan" "int" -1)
   (ktv "earning" "int" -1)
   (ktv "radio" "int" 0)
   (ktv "tv" "int" 0)
   (ktv "mobile" "int" 0)
   (ktv "visit-market" "int" -1)
   (ktv "town-sell" "int" -1)
   (ktv "social-one" "varchar" "")
   (ktv "social-one-nickname" "varchar" "")
   (ktv "social-one-relationship" "varchar" "")
   (ktv "social-one-residence" "varchar" "")
   (ktv "social-one-strength" "varchar" "")
   (ktv "social-two" "varchar" "")
   (ktv "social-two-nickname" "varchar" "")
   (ktv "social-two-relationship" "varchar" "")
   (ktv "social-two-residence" "varchar" "")
   (ktv "social-two-strength" "varchar" "")
   (ktv "social-three" "varchar" "")
   (ktv "social-three-nickname" "varchar" "")
   (ktv "social-three-relationship" "varchar" "")
   (ktv "social-three-residence" "varchar" "")
   (ktv "social-three-strength" "varchar" "")
   (ktv "social-four" "varchar" "")
   (ktv "social-four-nickname" "varchar" "")
   (ktv "social-four-relationship" "varchar" "")
   (ktv "social-four-residence" "varchar" "")
   (ktv "social-four-strength" "varchar" "")
   (ktv "social-five" "varchar" "")
   (ktv "social-five-nickname" "varchar" "")
   (ktv "social-five-relationship" "varchar" "")
   (ktv "social-five-residence" "varchar" "")
   (ktv "social-five-strength" "varchar" "")
   (ktv "friendship-one" "varchar" "")
   (ktv "friendship-one-nickname" "varchar" "")
   (ktv "friendship-one-relationship" "varchar" "")
   (ktv "friendship-one-residence" "varchar" "")
   (ktv "friendship-one-strength" "varchar" "")
   (ktv "friendship-two" "varchar" "")
   (ktv "friendship-two-nickname" "varchar" "")
   (ktv "friendship-two-relationship" "varchar" "")
   (ktv "friendship-two-residence" "varchar" "")
   (ktv "friendship-two-strength" "varchar" "")
   (ktv "friendship-three" "varchar" "")
   (ktv "friendship-three-nickname" "varchar" "")
   (ktv "friendship-three-relationship" "varchar" "")
   (ktv "friendship-three-residence" "varchar" "")
   (ktv "friendship-three-strength" "varchar" "")
   (ktv "friendship-four" "varchar" "")
   (ktv "friendship-four-nickname" "varchar" "")
   (ktv "friendship-four-relationship" "varchar" "")
   (ktv "friendship-four-residence" "varchar" "")
   (ktv "friendship-four-strength" "varchar" "")
   (ktv "friendship-five" "varchar" "")
   (ktv "friendship-five-nickname" "varchar" "")
   (ktv "friendship-five-relationship" "varchar" "")
   (ktv "friendship-five-residence" "varchar" "")
   (ktv "friendship-five-strength" "varchar" "")
   ))

(define crop-ktvlist
  (list
   (ktv "name" "varchar" (mtext-lookup 'default-crop-name))
   (ktv "unit" "varchar" "unit")
   (ktv "used" "real" -1)
   (ktv "sold" "real" -1)
   (ktv "seed" "varchar" "")))

(define child-ktvlist
  (list
   (ktv "name" "varchar" (mtext-lookup 'default-child-name))
   (ktv "alive" "int" 1)
   (ktv "gender" "varchar" "")
   (ktv "age" "int" -1)
   (ktv "living-at-home" "int" 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define (debug! txt)
  (set-current! 'debug-text (string-append txt "\n" (get-current 'debug-text ""))))

(define (update-debug)
  (update-widget 'debug-text-view (get-id "sync-debug") 'text
                 (get-current 'debug-text "")))

(define (debug-timer-cb)
  (alog "debug timer cb")
  (append
   (cond
    ((get-current 'sync-on #f)
     ;(when (zero? (random 10))
     ;      (msg "mangling...")
     ;      (mangle-test! db "sync" entity-types))
     (set-current! 'upload 0)
     (set-current! 'download 0)
     (connect-to-net
      (lambda ()
        (msg "connected, going in...")
        (alog "got here...")
        (append
         (list (toast "Syncing"))
         (upload-dirty db)
         ;; important - don't receive until all are sent...
         (if (have-dirty? db "sync") '()
             (append
              (suck-new db "sync")
              (start-sync-files)))))))
    (else '()))
   (list
    (delayed "debug-timer" (+ 10000 (random 5000)) debug-timer-cb)
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
    (image-button (make-id "top-icon") "logo" (layout 48 64 -1 'centre 0)
                  (lambda () (list (start-activity-goto "main2" 0 ""))))
    (text-view (make-id "title") "" 30
               (layout 'fill-parent 'fill-parent 0.5 'centre 10))

    (linear-layout
     0 'vertical
     (layout 'fill-parent 'wrap-content 0.5 'centre 0)
     (list 0 0 0 0)

     (list
      (text-view (make-id "top-village") 'name 20
                 (layout 'wrap-content 'wrap-content 1 'right 0))
      (text-view (make-id "top-household") 'name 20
                 (layout 'wrap-content 'wrap-content 1 'right 0))
      (text-view (make-id "top-photo-id") 'photo-id 20
                 (layout 'wrap-content 'wrap-content 1 'right 0)))))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))


  (fragment
   "bottom"
   (linear-layout
    0 'horizontal
    (layout 'fill-parent 'fill-parent 1 'centre 0)
    (list 0 0 0 0)
    (list
;     (mbutton-scale
;      'save
;      (lambda ()
;        (list
;         (alert-dialog
;          "ok-check"
;          (mtext-lookup 'save-are-you-sure)
;          (lambda (v)
;            (cond
;             ((eqv? v 1)
;              (entity-update-values!)
;              (list))
;             (else
;              (list))))))))
     (mbutton-scale 'back (lambda () (list (finish-activity 1))))))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list))
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '())
   (lambda (fragment) '()))


  )

(define (update-top-bar)
  (let ((village (get-entity-name db "sync" (get-current 'village #f)))
        (household (get-entity-name db "sync" (get-current 'household #f)))
        (individual (get-entity-name db "sync" (get-current 'individual #f))))
    (list
     (update-widget 'text-view (get-id "title") 'text
                    (get-current 'activity-title "Title not set"))
     (update-widget 'text-view (get-id "top-village") 'text (if village (string-append "Village: " village) ""))
     (update-widget 'text-view (get-id "top-household") 'text (if household (string-append "Household: " household) ""))
     (update-widget 'text-view (get-id "top-photo-id") 'text (if individual (string-append "Individual: " individual) "")))))



(define (build-activity . contents)
  (vert-fill
   (relative
    '(("parent-top"))
    colour-one ;;(list 100 100 255 127)
    (build-fragment "top" (make-id "top") fillwrap))

   (scroll-view-vert
    0 (layout 'fill-parent 'fill-parent 1 'centre 0)
    (list
     (apply vert-fill contents)))

   (relative
    '(("parent-bottom"))
    colour-one
    (vert
     (spacer 5)
     (build-fragment "bottom" (make-id "bottom") fillwrap)))))


(define (grid-ify widgets n)
  (map
   (lambda (w)
     (linear-layout
      0 'horizontal
      (layout 'wrap-content 'wrap-content 1 'left 0)
      (list 0 0 0 0)
      w))
   (chop widgets n)))

(define (filter-set! l)
  (set-current! 'individual-filter l))

(define (filter-clear!)
  (filter-set! '()))

(define (filter-add! f)
  (set-current!
   'individual-filter
   (merge-filter f (get-current 'individual-filter '()))))

(define (filter-remove! key)
  (set-current!
   'individual-filter
   (delete-filter key (get-current 'individual-filter '()))))

(define (filter-get)
  (get-current 'individual-filter '()))

(define button-size (list (inexact->exact (round (* 192 0.9)))
                          (inexact->exact (round (* 256 0.9)))))


(define (build-photo-buttons search)
  (grid-ify
   (map
    (lambda (e)
      (let* ((id (ktv-get e "unique_id"))
             (image-name (ktv-get e "photo"))
             (image (if (image-invalid? image-name)
                        "face" (string-append "/sdcard/symbai/files/" image-name))))
        (cond
         ((> (length search) 500)
          (button
           (make-id (string-append "chooser-" id))
           (ktv-get e "name") 30 (layout (car button-size) (/ (cadr button-size) 3) 1 'centre 5)
           (lambda ()
             (set-current! 'choose-result id)
             (list (finish-activity 0)))))

         ((equal? image "face")
          (button
           (make-id (string-append "chooser-" id))
           (ktv-get e "name") 30 (layout (car button-size) (cadr button-size) 1 'centre 5)
           (lambda ()
             (set-current! 'choose-result id)
             (list (finish-activity 0)))))

         (else
          (vert
           (image-button
            (make-id (string-append "chooser-" id))
            image (layout (car button-size) (cadr button-size) 1 'centre 5)
            (lambda ()
              (set-current! 'choose-result id)
              (list (finish-activity 0))))
           (text-view 0 (ktv-get e "name") 20 (layout 'wrap-content 'wrap-content -1 'centre 0)))
          ))))
    search)
   3))

;; getting late in the day...
(define filter-index 0)
(define filter-households '())

(define (gradual-build)
  (if (or (null? filter-households)
          (> filter-index (- (length filter-households) 1)))
      '()
      (let ((household (list-ref filter-households filter-index)))
        (set! filter-index (+ filter-index 1))
        (let ((search (db-filter-only db "sync" "individual"
                                      (append (filter-get)
                                              (list (list "parent" "varchar" "="
                                                          (ktv-get household "unique_id"))))
                                      (list
                                       (list "photo" "file")
                                       (list "name" "varchar")))))
          (list
           (delayed "filter-delayed" 100 gradual-build)
           (update-widget
            'linear-layout (get-id "choose-pics") 'contents-add
            (list
             (apply vert
                    (cons (text-view 0 (ktv-get household "name") 40 fillwrap)
                          (build-photo-buttons search)))))
           )))))



(define (update-individual-filter-inner households)
  (set! filter-households households)
  (delayed "filter-delayed" 100 gradual-build))

(define (update-individual-filter)
  (let ((households (db-filter-only db "sync" "household"
                                    (list (list "parent" "varchar" "=" (get-setting-value "current-village")))
                                    (list (list "name" "varchar")))))
    (update-individual-filter-inner households)))


(define (update-individual-filter2)
  (alog "uif-inner")
  (let ((search (db-filter-only db "sync" "individual"
                                (filter-get)
                                (list
                                 (list "photo" "file")
                                 (list "name" "varchar")))))
    (alog "uif-house-search end")
    (update-widget
     'linear-layout (get-id "choose-pics") 'contents
     (build-photo-buttons search))
    ))



(define (image/name-from-unique-id db table unique-id)
  (let ((e (get-entity-by-unique db table unique-id)))
    (list
     (ktv-get e "name")
     (ktv-get e "photo"))))

(define (build-person-selector id key filter request-code)
  (vert
   (mtitle id)
   (image-view (make-id (string-append (symbol->string id) "-image"))
               "face" (layout 240 320 -1 'centre 0))
   (mtext-small (string->symbol (string-append (symbol->string id) "-text")))
   (button
    (make-id (string-append "change-" (symbol->string id)))
    (mtext-lookup 'change-id)
    40 (layout 'fill-parent 'wrap-content -1 'centre 5)
    (lambda ()
      (filter-set! filter)
      (list (start-activity "individual-chooser" request-code ""))))))

(define (build-small-person-selector id key filter request-code)
  (linear-layout
   0 'vertical
   (layout 300 'wrap-content 1 'centre 10)
   (list 0 0 0 0)
   (list
    (mtitle id)
    (image-view (make-id (string-append (symbol->string id) "-image"))
                "face" (layout 120 160 -1 'centre 0))
    (mtext-small (string->symbol (string-append (symbol->string id) "-text")))
    (button
     (make-id (string-append "change-" (symbol->string id)))
     (mtext-lookup 'change-id)
     40 (layout 'wrap-content 'wrap-content -1 'centre 5)
     (lambda ()
       (filter-set! filter)
       (list (start-activity "individual-chooser" request-code "")))))))


;; from activity on result with request id: choose-code
;; todo determine *which* selector this came from...
(define (person-selector-return request-code key choose-code)
  (when (eqv? request-code choose-code)
        (entity-set-value! key "varchar" (get-current 'choose-result "not set"))
        (entity-update-values!)))

;; need to load from across entities, so need db, table
(define (update-person-selector db table id key)
  (let ((entity-id (entity-get-value key)))
    (let ((image-name (image/name-from-unique-id db table entity-id))
          (id (get-id (string-append (symbol->string id) "-image")))
          (text-id (get-id (string-append (symbol->string id) "-text"))))
      (if (image-invalid? (cadr image-name))
          (list
           (update-widget 'image-view id 'image "face")
           (update-widget 'text-view text-id 'text (or (car image-name) "")))
          (list
           (update-widget 'text-view text-id 'text (or (car image-name) ""))
           (update-widget 'image-view id 'external-image
                          (string-append dirname "files/" (cadr image-name))))))))

(define (build-social-connection id key type request-code shade)
  (let ((id-text (string-append (symbol->string id))))
    (linear-layout
     0 'horizontal
     (layout 'wrap-content 'wrap-content 1 'centre 20)
     (if shade colour-one colour-two)
     (list
      (build-small-person-selector id key (list) request-code)
      (vert
       (horiz
        (linear-layout
         0 'vertical (layout 'fill-parent 'wrap-content 1 'centre 20) (list 0 0 0 0)
         (list
          (text-view 0 (mtext-lookup 'social-nickname)
                     30 (layout 'wrap-content 'wrap-content 1 'centre 0))
          (edit-text (make-id (string-append id-text "-nickname")) "" 30 "normal"
                     (layout 'fill-parent 'wrap-content 1 'centre 0)
                     (lambda (v)
                       (entity-set-value! (string-append key "-nickname") "varchar" v)
                       '()))))

        (mspinner-other-vert
         (string->symbol (string-append id-text "-relationship"))
         'social-relationship
         social-relationship-list
         (lambda (v)
           (entity-set-value! (string-append key "-relationship") "varchar"
                              (spinner-choice social-relationship-list v))
           '())))

       (horiz
        (mspinner-other-vert
         (string->symbol (string-append id-text "-residence"))
         'social-residence
         social-residence-list
         (lambda (v)
           (entity-set-value! (string-append key "-residence") "varchar"
                              (spinner-choice social-residence-list v)) '()))
        (vert
         (text-view 0 (mtext-lookup 'social-strength)
                    30 (layout 'wrap-content 'wrap-content 1 'centre 10))
         (spinner
          (make-id (string-append id-text "-strength-spinner"))
          (map mtext-lookup social-strength-list)
          (layout 'wrap-content 'wrap-content 1 'centre 0)
          (lambda (v)
            (entity-set-value! (string-append key "-strength") "varchar"
                               (spinner-choice social-strength-list v)) '())))))))))

(define (social-connection-return request-code key choose-code)
  (when (eqv? request-code choose-code)
        (entity-set-value! key "varchar" (get-current 'choose-result "not set"))))

(define (update-social-connection db table id key type request-code)
  (let ((id-text (string-append (symbol->string id))))
    (append
     (update-person-selector db table id key)
     (mupdate-spinner-other
      (string->symbol (string-append id-text "-relationship"))
      (string-append key "-relationship")
      social-relationship-list)
     (mupdate-spinner-other
      (string->symbol (string-append id-text "-residence"))
      (string-append key "-residence")
      social-residence-list)
     (list
      (mupdate
       'edit-text
       (string->symbol (string-append id-text "-nickname"))
       (string-append key "-nickname"))
      (mupdate-spinner
       (string->symbol (string-append id-text "-strength"))
       (string-append key "-strength")
       social-strength-list))
     )))

(define (build-amenity-widgets id shade)
  (let ((id-text (symbol->string id)))

    (horiz-colour
     (if shade colour-one colour-two)
     (linear-layout
      0 'vertical (layout 200 'wrap-content -1 'left 0)
      (list 0 0 0 0)
      (list
       (text-view (symbol->id id)
                  (mtext-lookup id)
                  30 (layout 'wrap-content 'wrap-content -1 'left 0))
       (mtoggle-button-scale
        (string->symbol (string-append id-text "-in-village"))
        (lambda (v)
          (entity-set-value! id-text "int" v)
          (list (update-widget
                 'edit-text
                 (get-id (string-append id-text "-closest-access-container"))
                 (if (eqv? v 1) 'hide 'show) 0))))))
     (medit-text-scale
      (string->symbol (string-append id-text "-closest-access"))
      "normal" (lambda (v) (entity-set-value!
                            (string-append id-text "-closest-access")
                            "varchar" v) '()))
     (vert
      (mbutton-scale
       (string->symbol (string-append id-text "-gps"))
       (lambda ()  (do-gps
                    (string->symbol (string-append id-text "-gps"))
                    (string-append id-text "-gps"))))
      (mtext-small (string->symbol (string-append id-text "-gps-lat")))
      (mtext-small (string->symbol (string-append id-text "-gps-lon")))))))

(define (update-amenity-widgets id)
  (let ((id-text (symbol->string id)))
    (append
     (list
      (mupdate 'toggle-button (string->symbol (string-append id-text "-in-village")) id-text)
      (mupdate 'edit-text
               (string->symbol (string-append id-text "-closest-access"))
               (string-append id-text "-closest-access"))
      (update-widget
       'edit-text
       (get-id (string-append id-text "-closest-access-container"))
       (if (eqv? (entity-get-value id-text) 1)
           'hide 'show) 0))
     (mupdate-gps
      (string->symbol (string-append id-text "-gps"))
      (string-append id-text "-gps")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; activities

(define photo-code 999)
(define choose-code 998)
(define spouse-request-code 997)
(define mother-request-code 996)
(define father-request-code 995)

(define social-request-code-one 994)
(define social-request-code-two 993)
(define social-request-code-three 992)
(define social-request-code-four 991)
(define social-request-code-five 990)

(define-activity-list

  (activity
   "main"
   (vert
    (image-view 0 "logo" (layout 'wrap-content 'wrap-content -1 'centre 0))
    (button (make-id "main-start")
            "Symbai"
            40 (layout 'wrap-content 'wrap-content -1 'centre 5)
            (lambda () (list (start-activity-goto "main2" 0 "")))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "main2"
   (build-activity
    (mtitle 'title)
    (horiz
     (medit-text 'user-id "normal"
                 (lambda (v)
                   (set-setting! "user-id" "varchar" v)
                   (set-current! 'user-id v)
                   (list)))

     (mspinner 'languages (list 'english 'khasi 'hindi)
               (lambda (c)
                 (set-setting! "language" "int" c)
                 (set! i18n-lang c)
                 (list)))
     (mbutton-scale 'find-individual (lambda () (list (start-activity "individual-chooser" choose-code "")))))

    (build-list-widget
     db "sync" 'households "household" "household" (lambda () (get-setting-value "current-village"))
     (lambda ()
       (let ((name
              ;; if it's the first household - change the id...
              (if (zero? (length (db-filter-only
                                  db "sync" "household"
                                  (list (list "parent" "varchar" "="
                                              (get-setting-value "current-village")))
                                  (list (list "name" "varchar")))))
                  (string-append
                   (ktv-get (get-entity-by-unique db "sync" (get-setting-value "current-village")) "name")
                   ":"
                   (get-setting-value "user-id")
                   ":gamehousehold")
                  (string-append
                   (ktv-get (get-entity-by-unique db "sync" (get-setting-value "current-village")) "name")
                   ":"
                   (get-setting-value "user-id") ":"
                   (number->string (get/inc-setting "house-id"))))))
         ;; autogenerate the name from the current ID
         (ktvlist-merge
          household-ktvlist
          (list (ktv "name" "varchar" name))))))

    (mbutton 'villages (lambda () (list (start-activity "villages" 0 ""))))

    (mbutton 'sync (lambda () (list (start-activity "sync" 0 ""))))

    (horiz
     (medit-text 'house-id "numeric" (lambda (v) (set-setting! "house-id" "int" (string->number v)) (list)))
     (medit-text 'photo-id "numeric" (lambda (v) (set-setting! "photo-id" "int" (string->number v)) (list))))
    )

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (alog "start main start")
     (set-current! 'activity-title "Main screen")
     (set-current! 'village (get-setting-value "current-village"))
     (set-current! 'household #f)
     (set-current! 'individual #f)
     (let ((r (append
      (update-top-bar)
      (list
       (update-widget 'edit-text (get-id "user-id") 'text (get-setting-value "user-id"))
       (update-widget 'edit-text (get-id "house-id") 'text (get-setting-value "house-id"))
       (update-widget 'edit-text (get-id "photo-id") 'text (get-setting-value "photo-id"))
       (update-widget 'spinner (get-id "languages-spinner") 'selection
                      (get-setting-value "language"))
       (gps-start "gps" (lambda (loc)
                          (set-current! 'location loc)
                          (list (toast (string-append
                                        (number->string (car loc)) ", "
                                        (number->string (cadr loc)))))))
       (update-list-widget
        db "sync" "household" "household" (get-setting-value "current-village"))))))
       (alog "end main start") r))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (cond
      ((and (eqv? requestcode choose-code)
            (get-current 'choose-result 0))
       (list (start-activity "individual" 0 (get-current 'choose-result 0))))
      ((eqv? requestcode photo-code)
       (list (update-widget
              'image-view (get-id "image")
              'external-image (string-append dirname "photo.jpg"))))
      (else
       '()))))

  (activity
   "villages"
   (build-activity
    (mspinner 'current-village '()
              (lambda (v)
                (set-setting! "current-village" "varchar"
                              (cadr (list-ref (get-current 'villages-list '()) v)))
                '()))
    (build-list-widget
     db "sync" 'villages "village" "village" (lambda () #f)
     (lambda () village-ktvlist)))


   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Villages")
     (set-current! 'villages-list (build-array-from-names db "sync" "village"))
     (append
      (update-top-bar)
      (list
       (update-widget 'spinner (get-id "current-village-spinner") 'array
                      (map car (get-current 'villages-list '())))
       (update-widget 'spinner (get-id "current-village-spinner") 'selection
                      (find-index-from-name-array
                       (get-current 'villages-list '())
                       (get-current 'village #f)))
       (update-list-widget db "sync" "village" "village" #f))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))



  (activity
   "village"
     (build-activity
      (horiz
       (medit-text 'village-name "normal" (lambda (v) (entity-set-value! "name" "varchar" v) '()))
       (medit-text 'block "normal" (lambda (v) (entity-set-value! "block" "varchar" v) '())))
      (horiz
       (medit-text 'district "normal" (lambda (v) (entity-set-value! "district" "varchar" v) '()))
       (mtoggle-button-scale 'car (lambda (v) (entity-set-value! "car" "int" v) '())))

      (mbutton 'household-list
               (lambda ()
                 (list (start-activity "household-list" 0
                                       (get-current 'village #f)))))

      (mtitle 'amenities)
      (build-amenity-widgets 'school #t)
      (build-amenity-widgets 'hospital #f)
      (build-amenity-widgets 'post-office #t)
      (build-amenity-widgets 'railway-station #f)
      (build-amenity-widgets 'state-bus-service #t)
      (build-amenity-widgets 'district-bus-service #f)
      (build-amenity-widgets 'panchayat #t)
      (build-amenity-widgets 'NGO #f)
      (build-amenity-widgets 'market #t)
      (delete-button))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Village")
     (entity-init! db "sync" "village" (get-entity-by-unique db "sync" arg))
     (set-current! 'village arg)
     (set-current! 'household #f)
     (set-current! 'individual #f)
     (append
      (update-top-bar)
      (list
       (mupdate 'edit-text 'village-name "name")
       (mupdate 'edit-text 'block "block")
       (mupdate 'edit-text 'district "district")
       (mupdate 'toggle-button 'car "car"))
      (update-amenity-widgets 'school)
      (update-amenity-widgets 'hospital)
      (update-amenity-widgets 'post-office)
      (update-amenity-widgets 'railway-station)
      (update-amenity-widgets 'state-bus-service)
      (update-amenity-widgets 'district-bus-service)
      (update-amenity-widgets 'panchayat)
      (update-amenity-widgets 'NGO)
      (update-amenity-widgets 'market)))

   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "household-list"
   (build-activity
    (build-list-widget
     db "sync" 'households "household" "household" (lambda () (get-current 'village #f))
     (lambda ()
       ;; autogenerate the name from the current ID
       (ktvlist-merge
        household-ktvlist
        (list (ktv "name" "varchar"
                   (string-append
                    (ktv-get (get-entity-by-unique db "sync" (get-setting-value "current-village")) "name")
                    (get-setting-value "user-id")
                    (number->string (get/inc-setting "house-id")))))))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Households")
     (append
      (update-top-bar)
      (list (update-list-widget
             db "sync" "household" "household" arg))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "household"
   (build-activity
    (horiz
     (medit-text 'num-pots "numeric" (lambda (v) (entity-set-value! "num-pots" "int" v) '()))
     (medit-text 'num-children "numeric" (lambda (v) (entity-set-value! "num-children" "int" v) '())))
    (horiz
     (vert
      (mtext 'location)
      (mbutton 'house-gps (lambda () (do-gps 'house "house")))
      (mtext-small 'house-lat)
      (mtext-small 'house-lon))
     (vert
      (mtext 'toilet-location)
      (mbutton 'toilet-gps (lambda () (do-gps 'toilet "toilet")))
      (mtext-small 'toilet-lat)
      (mtext-small 'toilet-lon)))


    (build-list-widget
     db "sync" 'individuals "individual" "individual"
     (lambda () (get-current 'household #f))
     (lambda ()
       (let ((photo-id (get/inc-setting "photo-id"))
             (household-name (ktv-get (get-entity-by-unique db "sync" (get-current 'household #f)) "name")))
         (ktvlist-merge
          individual-ktvlist
          (list
           (ktv "name" "varchar"
                (string-append
                 household-name ":"
                 (get-current 'user-id "no id") ":"
                 (number->string photo-id)))
           (ktv "photo-id" "varchar"
                (number->string photo-id))
           (ktv "social-type" "varchar"
                (symbol->string
                 (list-ref social-types-list
                           (modulo photo-id (length social-types-list)))))
         )))))


    (delete-button))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Household")
     (entity-init! db "sync" "household" (get-entity-by-unique db "sync" arg))
     (set-current! 'household arg)
     (set-current! 'individual #f)
     (append
      (update-top-bar)
      (list
       (update-list-widget db "sync" "individual" "individual" arg)
       (mupdate 'edit-text 'num-pots "num-pots")
       (mupdate 'edit-text 'num-children "num-children"))
      (mupdate-gps 'house "house")
      (mupdate-gps 'toilet "toilet")))

   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "individual"
   (build-activity
    (horiz
     (image-view (make-id "photo") "face" (layout 240 320 -1 'centre 10))
     (vert
      (mtext 'name-display)
      (spacer 20)
      (mtext 'first-name-display)
      (spacer 20)
      (mtext 'family-display)
      (spacer 20)
      (mtext 'photo-id-display)
      ))
    (mbutton 'agreement-button (lambda () (list (start-activity "agreement" 0 ""))))
    (horiz
     (mbutton-scale 'details-button (lambda () (list (start-activity "details" 0 ""))))
     (mbutton-scale 'family-button (lambda () (list (start-activity "family" 0 "")))))
    (horiz
     (mbutton-scale 'migration-button (lambda () (list (start-activity "migration" 0 ""))))
     (mbutton-scale 'income-button (lambda () (list (start-activity "income" 0 "")))))
    (horiz
     (mbutton-scale 'genealogy-button (lambda () (list (start-activity "genealogy" 0 ""))))
     (mbutton-scale 'friendship-button (lambda () (list (start-activity "friendship" 0 "")))))
    (horiz
     (mbutton-scale 'social-button (lambda () (list (start-activity "social" 0 ""))))
     (mbutton-scale 'move-button (lambda () (list (start-activity "move" 0 "")))))
    (spacer 20)
    (delete-button))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Individual")
     (entity-init! db "sync" "individual" (get-entity-by-unique db "sync" arg))
     (set-current! 'individual arg)
     (append
      (update-top-bar)
      (list
       (mupdate 'text-view 'name-display "name")
       (mupdate 'text-view 'first-name-display "first-name")
       (mupdate 'text-view 'family-display "family")
       (mupdate 'text-view 'photo-id-display "photo-id")
       (mupdate 'image-view 'photo "photo"))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "details"
   (build-activity
    (horiz

     (vert
      (image-view (make-id "photo") "face" (layout 240 320 -1 'centre 10))
      (mbutton
       'change-photo
       (lambda ()
         (set-current!
          'photo-name (string-append (entity-get-value "unique_id") "-" (get-unique "p") "-face.jpg"))
         (list
          (take-photo (string-append dirname "files/" (get-current 'photo-name "")) photo-code))
         )))

     (vert
      (medit-text 'details-first-name "normal" (lambda (v) (entity-set-value! "first-name" "varchar" v) '()))
      (medit-text 'details-family "normal" (lambda (v) (entity-set-value! "family" "varchar" v) '()))))
    (mspinner-other 'tribe tribes-list (lambda (v) (entity-set-value! "tribe" "varchar" (spinner-choice tribes-list v)) '()))
    (mspinner-other 'sub-tribe subtribe-list (lambda (v) (entity-set-value! "subtribe" "varchar" (spinner-choice subtribe-list v)) '()))
    (horiz
     (medit-text 'age "numeric" (lambda (v) (entity-set-value! "age" "int" v) '()))
     (mspinner 'gender gender-list (lambda (v) (entity-set-value! "gender" "varchar" (spinner-choice gender-list v)) '())))
    (horiz
     (mtoggle-button-scale 'literate (lambda (v) (entity-set-value! "literate" "int" v) '()))
     (mspinner 'education education-list (lambda (v) (entity-set-value! "education" "varchar" v) '())))

    (mbutton 'details-next (lambda () (list (start-activity "family" 0 ""))))
    (spacer 20)
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Details")
     (append
      (update-top-bar)
      (mupdate-spinner-other 'tribe "tribe" tribes-list)
      (mupdate-spinner-other 'sub-tribe "subtribe" subtribe-list)
      (list
       (mupdate 'edit-text 'details-first-name "first-name")
       (mupdate 'edit-text 'details-family "family")
       (mupdate 'image-view 'photo "photo")
       (mupdate 'edit-text 'age "age")
       (mupdate-spinner 'gender "gender" gender-list)
       (mupdate 'toggle-button 'literate "literate")
       (mupdate-spinner 'education "education" education-list)
       )))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (cond
      ((eqv? requestcode photo-code)
       ;; todo: means we save when the camera happens
       ;; need to do this before init is called again in on-start,
       ;; which happens next
       (let ((unique-id (entity-get-value "unique_id")))
         (when (eqv? resultcode -1) ;; success!
               (entity-set-value! "photo" "file" (get-current 'photo-name "error no photo name!!"))
               (entity-update-values!))
         ;; need to reset the individual from the db now (as update reset it)
         (entity-init! db "sync" "individual" (get-entity-by-unique db "sync" unique-id)))
       (list
        (mupdate 'image-view 'photo "photo")))
      (else
       '()))))

  (activity
   "family"
   (build-activity
    (horiz
     (vert
      (mspinner 'head-of-house gender-list (lambda (v) (entity-set-value! "head-of-house" "varchar" (spinner-choice gender-list v)) '()))
      (mspinner 'marital-status married-list (lambda (v) (entity-set-value! "marital-status" "varchar" (spinner-choice married-list v)) '()))
      (medit-text 'times-married "numeric"
                  (lambda (v)
                    (entity-set-value! "times-married" "int" v)
                    (list
                     (update-widget 'linear-layout (get-id "residence-after-marriage-container")
                                    (if (equal? v "0") 'hide 'show) 0)))))

     (build-person-selector 'spouse "id-spouse" (list) spouse-request-code)
     )

;;    (mtitle 'children)
;;    (horiz
;;     (medit-text 'children-living "numeric" (lambda (v) (entity-set-value! "children-living" "int" v) '()))
;;     (medit-text 'children-dead "numeric" (lambda (v) (entity-set-value! "children-dead" "int" v) '())))
;;    (horiz
;;     (medit-text 'children-together "numeric" (lambda (v) (entity-set-value! "children-together" "int" v) '()))
;;     (medit-text 'children-apart "numeric" (lambda (v) (entity-set-value! "children-apart" "int" v) '())))

    (mspinner-other 'residence-after-marriage residence-list (lambda (v) (entity-set-value!
                                                                          "residence-after-marriage" "varchar"
                                                                          (spinner-choice residence-list v)) '()))
    (medit-text 'num-siblings "numeric" (lambda (v) (entity-set-value! "num-siblings" "int" v) '()))
    (medit-text 'birth-order "numeric" (lambda (v) (entity-set-value! "birth-order" "int" v) '()))
    (mbutton 'family-next (lambda () (list (start-activity "migration" 0 ""))))
    (spacer 20)
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Family")
     (append
      (update-top-bar)
      (update-person-selector db "sync" 'spouse "id-spouse")
      (list
       (mupdate-spinner 'head-of-house "head-of-house" gender-list)
       (mupdate-spinner 'marital-status "marital-status" married-list)
       (mupdate 'edit-text 'times-married "times-married")
       ;;(mupdate 'id-spouse "id-spouse")
;;       (mupdate 'edit-text 'children-living "children-living")
;;       (mupdate 'edit-text 'children-dead "children-dead")
;;       (mupdate 'edit-text 'children-together "children-together")
;;       (mupdate 'edit-text 'children-apart "children-apart")
       (mupdate-spinner 'residence-after-marriage "residence-after-marriage" residence-list)
       (mupdate 'edit-text 'num-siblings "num-siblings")
       (mupdate 'edit-text 'birth-order "birth-order"))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (person-selector-return requestcode "id-spouse" spouse-request-code)

     ;; intercept a spouse setting and set the other individual
     ;; BIDIRECTIONAL AUTOSPOUSE
     (when (and (eqv? requestcode spouse-request-code)
                (get-current 'choose-result #f))
           (update-entity db "sync" (entity-id-from-unique db "sync" (get-current 'choose-result #f))
                          (list (ktv "id-spouse" "varchar" (entity-get-value "unique_id")))))

     ;; save and reinit otherwise we can get out of sync here with the spouse :/
     (let ((unique-id (entity-get-value "unique_id")))
       (entity-update-values!)
       ;; need to reset the individual from the db now (as update reset it)
       (entity-init! db "sync" "individual" (get-entity-by-unique db "sync" unique-id)))

     '()))

  (activity
   "move"
   (build-activity
    (mspinner 'move-household '()
              (lambda (v)
                (entity-set-value!
                 "parent" "varchar"
                 (cadr (list-ref (get-current 'move-household-list '()) v)))
                '())))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Move")
     (set-current! 'move-household-list (build-array-from-names db "sync" "household"))
     (append
      (update-top-bar)
      (list
       (update-widget 'spinner (get-id "move-household-spinner") 'array
                      (map car (get-current 'move-household-list '()))))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))



  (activity
   "migration"
   (build-activity
    (medit-text 'length-time "numeric" (lambda (v) (entity-set-value! "length-time" "int" v) '()))
    (medit-text 'place-of-birth "normal" (lambda (v) (entity-set-value! "place-of-birth" "varchar" v) '()))
    (medit-text 'num-residence-changes "numeric" (lambda (v) (entity-set-value! "num-residence-changes" "int" v) '()))
    (medit-text 'village-visits-month "numeric" (lambda (v) (entity-set-value! "village-visits-month" "int" v) '()))
    (medit-text 'village-visits-year "numeric" (lambda (v) (entity-set-value! "village-visits-year" "int" v) '()))
    (mbutton 'migration-next (lambda () (list (start-activity "income" 0 ""))))
    (spacer 20)
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Migration")
     (append
      (update-top-bar)
      (list
       (mupdate 'edit-text 'length-time "length-time")
       (mupdate 'edit-text 'place-of-birth "place-of-birth")
       (mupdate 'edit-text 'num-residence-changes "num-residence-changes")
       (mupdate 'edit-text 'village-visits-month "village-visits-month")
       (mupdate 'edit-text 'village-visits-year "village-visits-year"))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "income"
   (build-activity
    (vert
     (mtitle 'occupation)
     (horiz
      (mtoggle-button-scale 'occupation-agriculture (lambda (v) (entity-set-value! "occupation-agriculture" "int" v) '()))
      (mtoggle-button-scale 'occupation-gathering (lambda (v) (entity-set-value! "occupation-gathering" "int" v) '()))
      (mtoggle-button-scale 'occupation-labour (lambda (v) (entity-set-value! "occupation-labour" "int" v) '())))
     (horiz
      (mtoggle-button-scale 'occupation-cows (lambda (v) (entity-set-value! "occupation-cows" "int" v) '()))
      (mtoggle-button-scale 'occupation-fishing (lambda (v) (entity-set-value! "occupation-fishing" "int" v) '()))
      (medit-text 'occupation-other "normal" (lambda (v) (entity-set-value! "occupation-other" "varchar" v) '()))))

    (horiz
     (mtoggle-button-scale 'contribute (lambda (v) (entity-set-value! "contribute" "int" v) '()))
     (mtoggle-button-scale 'own-land (lambda (v) (entity-set-value! "own-land" "int" v) '())))
    (horiz
     (mtoggle-button-scale 'rent-land (lambda (v) (entity-set-value! "rent-land" "int" v) '()))
     (mtoggle-button-scale 'hire-land (lambda (v) (entity-set-value! "hire-land" "int" v) '())))
    (mtext 'crops-detail)
    (build-list-widget
     db "sync" 'crops "crop" "crop" (lambda () (get-current 'individual #f))
     (lambda () crop-ktvlist))
    (mspinner-other 'house-type house-type-list (lambda (v) (entity-set-value! "house-type" "varchar"
                                                                               (spinner-choice house-type-list v)) '()))
    (horiz
     (medit-text 'loan "numeric" (lambda (v) (entity-set-value! "loan" "int" v) '()))
     (medit-text 'earning "numeric" (lambda (v) (entity-set-value! "earning" "int" v) '())))
    (mtext 'in-the-home)
    (horiz
     (mtoggle-button-scale 'radio (lambda (v) (entity-set-value! "radio" "int" v) '()))
     (mtoggle-button-scale 'tv (lambda (v) (entity-set-value! "tv" "int" v) '()))
     (mtoggle-button-scale 'mobile (lambda (v) (entity-set-value! "mobile" "int" v) '())))
    (horiz
     (medit-text 'visit-market "numeric" (lambda (v) (entity-set-value! "visit-market" "int" v) '()))
     (medit-text 'town-sell "numeric" (lambda (v) (entity-set-value! "town-sell" "int" v) '())))
    (mbutton 'income-next (lambda () (list (start-activity "genealogy" 0 ""))))
    (spacer 20)
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Income")
     ;; reset after crop entity
     (entity-init! db "sync" "individual" (get-entity-by-unique db "sync" (get-current 'individual #f)))
     (append
      (update-top-bar)
      (mupdate-spinner-other 'house-type "house-type" house-type-list)
      (list
       (update-list-widget db "sync" "crop" "crop" (get-current 'individual #f))
       (mupdate 'toggle-button 'occupation-agriculture "occupation-agriculture")
       (mupdate 'toggle-button 'occupation-gathering "occupation-gathering")
       (mupdate 'toggle-button 'occupation-labour "occupation-labour")
       (mupdate 'toggle-button 'occupation-cows "occupation-cows")
       (mupdate 'toggle-button 'occupation-fishing "occupation-fishing")
       (mupdate 'edit-text 'occupation-other "occupation-other")
       (mupdate 'toggle-button 'contribute "contribute")
       (mupdate 'toggle-button 'own-land "own-land")
       (mupdate 'toggle-button 'rent-land "rent-land")
       (mupdate 'toggle-button 'hire-land "hire-land")
       (mupdate 'edit-text 'loan "loan")
       (mupdate 'edit-text 'earning "earning")
       (mupdate 'toggle-button 'radio "radio")
       (mupdate 'toggle-button 'tv "tv")
       (mupdate 'toggle-button 'mobile "mobile")
       (mupdate 'edit-text 'visit-market "visit-market")
       (mupdate 'edit-text 'town-sell "town-sell"))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "crop"
   (build-activity
    (vert
     (medit-text 'crop-name "normal" (lambda (v) (entity-set-value! "name" "varchar" v) '()))
     (medit-text 'crop-unit "normal" (lambda (v) (entity-set-value! "unit" "varchar" v) '()))
     (medit-text 'crop-used "numeric" (lambda (v) (entity-set-value! "used" "real" (string->number v)) '()))
     (medit-text 'crop-sold "numeric" (lambda (v) (entity-set-value! "sold" "real" (string->number v)) '()))
     (medit-text 'crop-seed "numeric" (lambda (v) (entity-set-value! "seed" "varchar" v) '()))
     (delete-button)))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Crop")
     (entity-init! db "sync" "crop" (get-entity-by-unique db "sync" arg))
     (set-current! 'crop arg)
     (append
      (update-top-bar)
      (list
       (mupdate 'edit-text 'crop-name "name")
       (mupdate 'edit-text 'crop-unit "unit")
       (mupdate 'edit-text 'crop-used "used")
       (mupdate 'edit-text 'crop-sold "sold")
       (mupdate 'edit-text 'crop-seed "seed")
       )))

   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "child"
   (build-activity
    (vert
     (medit-text 'child-name "normal" (lambda (v) (entity-set-value! "name" "varchar" v) '()))
     (horiz
      (mspinner 'child-gender gender-list (lambda (v) (entity-set-value! "gender" "varchar" (spinner-choice gender-list v)) '()))
      (medit-text 'child-age "numeric" (lambda (v) (entity-set-value! "age" "int" (string->number v)) '())))
     (horiz
      (mtoggle-button-scale 'child-alive (lambda (v) (entity-set-value! "alive" "int" v) '()))
      (mtoggle-button-scale 'child-home (lambda (v) (entity-set-value! "living-at-home" "int" v) '())))
     (delete-button)))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Child")
     (entity-init! db "sync" "child" (get-entity-by-unique db "sync" arg))
     (set-current! 'child arg)
     (append
      (update-top-bar)
      (list
       (mupdate 'edit-text 'child-name "name")
       (mupdate-spinner 'child-gender "gender" gender-list)
       (mupdate 'edit-text 'child-age "age")
       (mupdate 'toggle-button 'child-alive "alive")
       (mupdate 'toggle-button 'child-home "living-at-home")
       )))

   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "genealogy"
   (build-activity
    (horiz
     (build-person-selector 'mother "id-mother" (list) mother-request-code)
     (build-person-selector 'father "id-father" (list) father-request-code))
    (build-list-widget
     db "sync" 'children "child" "child" (lambda () (get-current 'individual #f))
     (lambda () child-ktvlist))
    (mbutton 'gene-next (lambda () (list (start-activity "friendship" 0 ""))))
    (spacer 20))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     ;; reset after child entity
     (set-current! 'activity-title "Genealogy")
     (entity-init! db "sync" "individual" (get-entity-by-unique db "sync" (get-current 'individual #f)))
     (append
      (update-top-bar)
      (list (update-list-widget db "sync" "child" "child" (get-current 'individual #f)))
      (update-person-selector db "sync" 'mother "id-mother")
      (update-person-selector db "sync" 'father "id-father")))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (person-selector-return requestcode "id-mother" mother-request-code)
     (person-selector-return requestcode "id-father" father-request-code)
     '()))

  (activity
   "social"
   (build-activity
    (mspinner 'social-type social-types-list
              (lambda (v)
                (entity-set-value! "social-type" "varchar"
                                   (spinner-choice social-types-list v))
                (list
                 (update-widget 'text-view (get-id "social-question") 'text
                                (cond
                                 ((eqv? v 1) (mtext-lookup 'prestige-question))
                                 (else (mtext-lookup 'knowledge-question)))))))
    (text-view (make-id "social-question")
               "" 30 (layout 'wrap-content 'wrap-content -1 'centre 20))
    (build-social-connection 'social-one "social-one" "friend" social-request-code-one #t)
    (build-social-connection 'social-two "social-two" "friend" social-request-code-two #f)
    (build-social-connection 'social-three "social-three" "friend" social-request-code-three #t)
    (build-social-connection 'social-four "social-four" "friend" social-request-code-four #f)
    (build-social-connection 'social-five "social-five" "friend" social-request-code-five #t)
    (mbutton 'social-next (lambda () (list (start-activity-goto "individual" 0 (get-current 'individual #f)))))
    (spacer 20)
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Social network")
     (append
      (update-top-bar)
      (list
       (mupdate-spinner 'social-type "social-type" social-types-list))
      (update-social-connection db "sync" 'social-one "social-one" "friend" social-request-code-one)
      (update-social-connection db "sync" 'social-two "social-two" "friend" social-request-code-two)
      (update-social-connection db "sync" 'social-three "social-three" "friend" social-request-code-three)
      (update-social-connection db "sync" 'social-four "social-four" "friend" social-request-code-four)
      (update-social-connection db "sync" 'social-five "social-five" "friend" social-request-code-five)))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (social-connection-return requestcode "social-one" social-request-code-one)
     (social-connection-return requestcode "social-two" social-request-code-two)
     (social-connection-return requestcode "social-three" social-request-code-three)
     (social-connection-return requestcode "social-four" social-request-code-four)
     (social-connection-return requestcode "social-five" social-request-code-five)
     '()))

  (activity
   "friendship"
   (build-activity
    (text-view (symbol->id 'friendship-question)
               (mtext-lookup 'friendship-question)
               30 (layout 'wrap-content 'wrap-content -1 'centre 20))
    (build-social-connection 'social-one "friendship-one" "friend" social-request-code-one #t)
    (build-social-connection 'social-two "friendship-two" "friend" social-request-code-two #f)
    (build-social-connection 'social-three "friendship-three" "friend" social-request-code-three #t)
    (build-social-connection 'social-four "friendship-four" "friend" social-request-code-four #f)
    (build-social-connection 'social-five "friendship-five" "friend" social-request-code-five #t)
    (mbutton 'friendship-next (lambda () (list (start-activity "social" 0 ""))))
    (spacer 20)
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'activity-title "Frienships")
     (append
      (update-top-bar)
      (update-social-connection db "sync" 'social-one "friendship-one" "friend" social-request-code-one)
      (update-social-connection db "sync" 'social-two "friendship-two" "friend" social-request-code-two)
      (update-social-connection db "sync" 'social-three "friendship-three" "friend" social-request-code-three)
      (update-social-connection db "sync" 'social-four "friendship-four" "friend" social-request-code-four)
      (update-social-connection db "sync" 'social-five "friendship-five" "friend" social-request-code-five)))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (social-connection-return requestcode "friendship-one" social-request-code-one)
     (social-connection-return requestcode "friendship-two" social-request-code-two)
     (social-connection-return requestcode "friendship-three" social-request-code-three)
     (social-connection-return requestcode "friendship-four" social-request-code-four)
     (social-connection-return requestcode "friendship-five" social-request-code-five)
     '()))

  ;; todo: stop/rec/play on exit
  (activity
   "agreement"
   (build-activity
    (horiz

     (mtoggle-button-scale
      'agree-record
      (lambda (v)
        (list
         (if (eqv? v 1) (soundfile-start-recording "/sdcard/symbai/test.3gp")
             (soundfile-stop-recording)))))
     (mtoggle-button-scale
      'agree-playback
      (lambda (v)
        (list
         (if (eqv? v 1) (soundfile-start-playback "/sdcard/symbai/test.3gp")
             (soundfile-stop-playback)))))
     )
    (mbutton 'agreement-next (lambda () (list (start-activity "details" 0 ""))))
    (spacer 20)
    )
   (lambda (activity arg)
     (set-current! 'activity-title "Agreement")
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     '()))

  (activity
   "individual-chooser"
   (build-activity
    (vert

     (linear-layout
      (make-id "choose-pics") 'vertical
      (layout 'fill-parent 'wrap-content 0.75 'centre 0)
      (list 0 0 0 0)
      (list))

     (mtitle 'filter)
     (horiz
      (mspinner 'gender '(off female male)
                (lambda (v)
                  (if (equal? v 0)
                      (filter-remove! "gender")
                      (filter-add! (make-filter "gender" "varchar" "="
                                                (spinner-choice '(off female male) v))))
                  (if (get-current 'filter-switch #f)
                      (list (update-individual-filter)) '())
                  ))
      (medit-text
       'name "normal"
       (lambda (v)
         (if (equal? v "")
             (filter-remove! "name")
             (filter-add! (make-filter "name" "varchar" "like" (string-append v "%"))))
         (if (get-current 'filter-switch #f)
             (list (update-individual-filter)) '()))
         )
      (mtoggle-button-scale 'filter-switch
                            (lambda (v)
                              (set-current! 'filter-switch (not (zero? v)))
                              '())))

     (horiz
      (medit-text 'quick-name "normal"
                  (lambda (v) (set-current! 'chooser-quick-name v) '()))
      (mbutton-scale
       'quick-add
       (lambda ()
         (list
          (alert-dialog
           "quick-add-check"
           (mtext-lookup 'add-are-you-sure)
           (lambda (v)
             (cond
              ((eqv? v 1)
               (set-current!
                'choose-result
                (entity-create!
                 db "sync" "individual"
                 (ktvlist-merge
                  individual-ktvlist
                  (list
                   (ktv "name" "varchar" (get-current 'chooser-quick-name (mtext-lookup 'no-name)))
                   (ktv "parent" "varchar" (get-current 'household #f))))))
               (list (finish-activity 0)))
              (else
               (list)))))))))
     ))
   (lambda (activity arg)
     (set-current! 'activity-title "Chooser")
     (set-current! 'choose-result #f)
     (activity-layout activity))
   (lambda (activity arg)
     (list (update-individual-filter (list))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "sync"
   (vert
    (text-view (make-id "sync-title") "Sync database" 40 fillwrap)
    (mtext 'sync-dirty "...")
    (mtoggle-button-scale 'sync-all (lambda (v) (set-current! 'sync-on v) '()))
    (mtitle 'export-data)
    (horiz
     (mbutton-scale 'sync-download
               (lambda ()
                 (debug! (string-append "Downloading whole db"))
                 (append
                 (foldl
                  (lambda (e r)
                    (debug! (string-append "Downloading /sdcard/symbai/" e ".csv"))
                    (cons
                     (http-download
                      (string-append "getting-" e)
                      (string-append url "fn=entity-csv&table=sync&type=" e)
                      (string-append "/sdcard/symbai/" e ".csv"))
                     r))
                  (list
                   (http-download
                    "getting-db"
                    "http://192.168.2.1:8889/symbai.db"
                    (string-append "/sdcard/symbai/symbai.db"))
                   )
                  entity-types)
                 (list))))
     (mbutton-scale 'sync-export
               (lambda ()
                 (debug! "Sending mail")
                 (list
                  (send-mail
                   ""
                   "From Symbai" "Please find attached your data"
                   (cons
                    "/sdcard/symbai/symbai.db"
                    (map
                     (lambda (e)
                       (string-append "/sdcard/symbai/" e ".csv"))
                     entity-types))))))
     (mbutton-scale 'email-local
               (lambda ()
                 (debug! "Sending mail")
                 (list
                  (send-mail
                   ""
                   "From symbai" "Please find attached your local data"
                   (list "/sdcard/symbai/local-symbai.db")))))
     )
    (spacer 10)
    (mtitle 'debug)
    (scroll-view-vert
     0 (layout 'fill-parent 200 1 'left 0)
     (list
      (vert
       (debug-text-view (make-id "sync-debug") "..." 15 (layout 'fill-parent 400 1 'left 0)))))
    (spacer 10)
    (horiz
     (mbutton-scale 'sync-back (lambda () (list (finish-activity 1))))
     (mbutton-scale 'sync-prof (lambda () (prof-print) (list))))
    )

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (set-current! 'sync-on #f)
     (append
      (debug-timer-cb)
      (list
       (update-widget 'debug-text-view (get-id "sync-debug") 'text (get-current 'debug-text ""))
       (update-widget 'text-view (get-id "sync-dirty") 'text (build-dirty db))
       )))
   (lambda (activity) '())
   (lambda (activity) (list (delayed "debug-timer" 1000 (lambda () '()))))
   (lambda (activity) (list (delayed "debug-timer" 1000 (lambda () '()))))
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))






  )


;(build-test! db "sync" village-ktvlist household-ktvlist individual-ktvlist)
