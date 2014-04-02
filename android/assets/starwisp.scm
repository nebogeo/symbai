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

(insert-entity-if-not-exists
 db "local" "app-settings" "null" 1
 (list
  (ktv "user-id" "varchar" "No name yet...")))

(define entity-types (list "village"))

;;(display (db-all db "local" "app-settings"))(newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user interface abstraction

;;;;;;;;;;;;; i18n ;;;;;;;;;;;;;;;;;;;;;;


(define i18n-text
  (list
   (list 'test-num (list "1.0000000" "1.0000000" "1.0000000"))
   (list 'test-text (list "I am test text" "I am test text" "I am test text"))
   (list 'one (list "one"))
   (list 'two (list "two"))
   (list 'three (list "three"))
   (list 'village (list "Village"))
   (list 'household (list "Household"))
   (list 'households (list "Households"))
   (list 'individual (list "Individual"))
   (list 'individuals (list "Individuals"))

   (list 'add-item-to-list (list "+"))
   (list 'default-village-name (list "New village"))

   (list 'title (list "Symbai" "Symbai" "Symbai"))
   (list 'sync (list "Sync" "Sync" "Sync"))
   (list 'languages (list "Choose language" "Choose language" "Choose language"))
   (list 'english (list "English" "English" "English"))
   (list 'khasi (list "Khasi" "Khasi" "Khasi"))
   (list 'hindi (list "Hindi" "Hindi" "Hindi"))
   (list 'user-id (list "Your user ID" "User ID" "User ID"))
   (list 'ok (list "Ok" "Ok" "Ok"))
   (list 'cancel (list "Cancel" "Cancel" "Cancel"))
   (list 'villages (list "Villages" "Villages" "Villages"))
   (list 'list-empty (list "List empty"))
   (list 'delete (list "Delete"))
   (list 'delete-are-you-sure (list "Are you sure you want to delete this?"))
   (list 'save-are-you-sure (list "Are you sure you want to save changes?"))

   ;; sync
   (list 'sync-all (list "Sync me!"))
   (list 'sync-syncall (list "Sync everything"))
   (list 'export-data (list "Exporting data"))
   (list 'sync-download (list "Download main DB"))
   (list 'sync-export (list "Email main DB"))
   (list 'email-local (list "Email local DB"))
   (list 'debug (list "Debug"))
   (list 'sync-back (list "Back"))
   (list 'sync-prof (list "Profile"))

   ;; village screen
   (list 'village-name (list "Village name" "Village name" "Village name"))
   (list 'block (list "Block" "Block" "Block"))
   (list 'district (list "District" "District" "District"))
   (list 'car (list "Accessible by car"))
   (list 'household-list (list "Household list"))
   (list 'amenities (list "Amenities"))
   (list 'school (list "School"))
   (list 'present (list "Present"))
   (list 'closest-access (list "Closest place of access"))
   (list 'house-gps (list "GPS"))
   (list 'toilet-gps (list "GPS"))
   (list 'school (list "School"))
   (list 'hospital (list "Hospital/Health care centre"))
   (list 'post-office (list "Post Office"))
   (list 'railway-station (list "Railway station"))
   (list 'state-bus-service (list "Inter-state bus service"))
   (list 'district-bus-service (list "Inter-village/district bus service"))
   (list 'panchayat (list "Village Panchayat Office"))
   (list 'NGO (list "Presence of NGO's working with them"))
   (list 'market (list "Market"))

   ;; household
   (list 'household-name (list "Household name"))
   (list 'default-household-name (list "A household"))
   (list 'location (list "House location"))
   (list 'elevation (list "Elevation"))
   (list 'toilet-location (list "Toilet location"))
   (list 'children (list "Children"))
   (list 'male (list "Male"))
   (list 'female (list "Female"))
   (list 'num-pots (list "Number of pots"))
   (list 'adults (list "Adults"))
   (list 'add-individual (list "Add individual"))

   ;; individual
   (list 'default-individual-name (list "A person"))
   (list 'default-family-name (list "A family"))
   (list 'default-photo-id (list "???"))
   (list 'name-display (list "Name"))
   (list 'photo-id-display (list "Photo ID"))
   (list 'family-display (list "Family"))
   (list 'details-button (list "Details"))
   (list 'family-button (list "Family"))
   (list 'migration-button (list "Migration"))
   (list 'income-button (list "Income"))
   (list 'geneaology-button (list "Geneaology"))
   (list 'social-button (list "Social"))
   (list 'agreement-button (list "Agreement"))

   ;; details
   (list 'change-photo (list "Change photo"))
   (list 'details-name (list "Name"))
   (list 'details-photo-id (list "Photo ID"))
   (list 'details-family (list "Family"))
   (list 'tribe (list "Tribe"))
   (list 'sub-tribe (list "Sub tribe"))
   (list 'other (list "Other"))
   (list 'age (list "Age"))
   (list 'gender (list "Gender"))
   (list 'education (list "Education"))

   ;; family
   (list 'spouse (list "Spouse"))
   (list 'head-of-house (list "Head of house"))
   (list 'marital-status (list "Marital status"))
   (list 'ever-married (list "Ever married"))
   (list 'currently-married (list "Currently married"))
   (list 'currently-single (list "Currently single"))
   (list 'seperated (list "Seperated/divorced"))
   (list 'times-married (list "How many times married"))
   (list 'change-spouse (list "Change/add spouse"))
   (list 'living (list "Living"))
   (list 'dead (list "Dead"))
   (list 'together (list "Living together"))
   (list 'apart (list "Living apart"))
   (list 'residence-after-marriage (list "Residence after marriage"))
   (list 'birthplace (list "Birthplace"))
   (list 'spouse-village (list "Spouses natal village"))
   (list 'num-siblings (list "Number of living siblings of the same sex born from same mother"))
   (list 'birth-order (list "Birth order amoung currently living same sex siblings born from same mother"))

   ;; migration
   (list 'length-time (list "Length of time lived in this village (years)"))
   (list 'place-of-birth (list "Place of birth"))
   (list 'num-residence-changes (list "Number of time place of residence changed since birth"))
   (list 'village-visits-month (list "Number of times you have visited another village in the last month"))
   (list 'village-visits-year (list "Number of times you have visited another village in the last year (i.e. betwen last summer and this summer)"))

   ;; income
   (list 'occupation (list "Occupation"))
   (list 'occupation (list "Occupation"))
   (list 'agriculture (list "Agriculture"))
   (list 'gathering (list "Gathering"))
   (list 'labour (list "Labour"))
   (list 'cows (list "Cows"))
   (list 'fishing (list "Fishing"))
   (list 'num-people-in-house (list "Number of people living in this house"))
   (list 'contribute (list "Do you contribute to the family earnings?"))
   (list 'own-land (list "Do you own land?"))
   (list 'rent-land (list "Do you rent out your land?"))
   (list 'hire-land (list "Do you hire someone else's land?"))
   (list 'crops (list "Crops"))
   (list 'crop (list "Crop"))
   (list 'unit (list "Unit"))
   (list 'quantity (list "Quantity"))
   (list 'used-or-eaten (list "Used/Eaten"))
   (list 'sold (list "Sold"))
   (list 'seed (list "Seed (hybrid/local)"))
   (list 'house-type (list "House type"))
   (list 'concrete (list "Concrete"))
   (list 'tin (list "Tin"))
   (list 'thatched (list "Thatched"))
   (list 'loan (list "How much outstanding loan money have you taken in all from any source at this time?"))
   (list 'earning (list "How much do you earn for one day's labour?"))
   (list 'in-the-home (list "In the home"))
   (list 'radio (list "Radio"))
   (list 'tv (list "TV"))
   (list 'mobile (list "Mobile phone"))
   (list 'visit-market (list "How many times a month do you visit the tribal market?"))
   (list 'town-sell (list "How many times a month do you visit the local town or city to sell something?"))

   ;; geneaology

   (list 'mother (list "Mother"))
   (list 'father (list "Father"))
   (list 'change-mother (list "Change mother"))
   (list 'change-father (list "Change father"))
   (list 'alive (list "Alive"))
   (list 'sex (list "Sex"))
   ))


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
        (msg "connected, going in...")
        (append
         (list (toast "sync-cb"))
         (upload-dirty db)
         (suck-new db "sync")))))
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
    (image-view 0 "face" (layout 48 64 -1 'centre 0))
    (text-view (make-id "title") "" 30
               (layout 'fill-parent 'fill-parent 0.25 'centre 10))

    (linear-layout
     0 'vertical
     (layout 'fill-parent 'wrap-content 0.75 'centre 0)
     (list 0 0 0 0)

     (list
      (text-view (make-id "") 'name 20
                 (layout 'fill-parent 'wrap-content 1 'centre 0))
      (text-view (make-id "") 'photo-id 20
                 (layout 'fill-parent 'wrap-content 1 'centre 0)))))
   (lambda (fragment arg)
     (activity-layout fragment))
   (lambda (fragment arg)
     (list
      (update-widget 'text-view (get-id "title") 'text
                     (get-current 'activity-title "Title not set"))))
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
     (mbutton-scale
      'ok
      (lambda ()
        (list
         (alert-dialog
          "ok-check"
          (mtext-lookup 'save-are-you-sure)
          (lambda (v)
            (cond
             ((eqv? v 1)
              (entity-update-values!)
              (list (finish-activity 1)))
             (else
              (list))))))))
     (mbutton-scale 'cancel (lambda () (list (finish-activity 1))))))
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


;; a standard builder for list widgets of entities and a
;; make new button, to add defaults to the list
(define (build-list-widget db table title entity-type edit-activity parent-fn ktv-default)
    (vert-colour
     colour-two
     (horiz
      (mtitle-scale title)
      (button
       (make-id (string-append (symbol->string title) "-add"))
       (mtext-lookup 'add-item-to-list)
       40 (layout 100 'wrap-content 1 'centre 5)
       (lambda ()
         (entity-init! db table entity-type ktv-default)
         (entity-add-value! "parent" "varchar" (parent-fn))
         (entity-record-values!)
         (list (update-list-widget db table entity-type edit-activity (parent-fn))))))
     (linear-layout
      (make-id (string-append entity-type "-list"))
      'vertical
      (layout 'fill-parent 'wrap-content 1 'centre 20)
      (list 0 0 0 0)
      (list))))

;; pull db data into list of button widgets
(define (update-list-widget db table entity-type edit-activity parent)
  (let ((search-results
         (if parent
             (db-with-parent db table entity-type parent)
             (db-all db table entity-type))))
    (update-widget
     'linear-layout
     (get-id (string-append entity-type "-list"))
     'contents
     (if (null? search-results)
         (list (mtext 'list-empty))
         (map
          (lambda (e)
            (button
             (make-id (string-append "list-button-" (ktv-get e "unique_id")))
             (or (ktv-get e "name") "Unamed item")
             40 (layout 'fill-parent 'wrap-content 1 'centre 5)
             (lambda ()
               (msg "sending start act" (ktv-get e "unique_id"))
               (list (start-activity edit-activity 0 (ktv-get e "unique_id"))))))
          search-results)))))

(define (delete-button)
  (mbutton
   'delete
   (lambda ()
     (list
      (alert-dialog
       "delete-check"
       (mtext-lookup 'delete-are-you-sure)
       (lambda (v)
         (cond
          ((eqv? v 1)
           (entity-add-value! "deleted" "int" 1)
           (entity-update-values!)
           (list (finish-activity 1)))
          (else
           (list)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; activities

(define photo-code 999)

(define-activity-list

  (activity
   "main"
   (build-activity
    (mtitle 'title)
    (horiz
     (medit-text 'user-id "normal" (lambda () (list)))
     (mbutton-scale 'sync (lambda () (list (start-activity "sync" 0 "")))))

    (mspinner 'languages (list 'english 'khasi 'hindi) (lambda (c) (list)))
    (mbutton 'test-upload (lambda ()
                            (list
                             (network-connect
                              "network"
                              "mongoose-web"
                              (lambda (state)
                                (msg state)
                                (if (equal? state "Connected")
                                    (list
                                     (http-upload
                                      "test-upload"
                                      "http://192.168.2.1:8889/symbai?fn=upload"
                                      "/sdcard/symbai/photo.jpg"))
                                    '()))
                              ))))
    (build-list-widget
     db "sync" 'villages "village" "village" (lambda () #f)
     (list
      (ktv "name" "varchar" (mtext-lookup 'default-village-name))
      (ktv "block" "varchar" "")
      (ktv "district" "varchar" "test")
      (ktv "car" "int" 0))))

   (lambda (activity arg)
     (set-current! 'activity-title "Main screen")
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (gps-start "gps" (lambda (loc)
                         (set-current! 'location loc)
                         (list (toast (string-append
                                       (number->string (car loc)) ", "
                                       (number->string (cadr loc)))))))
      (update-list-widget db "sync" "village" "village" #f)))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (cond
      ((eqv? requestcode photo-code)
       (msg "camera returned" resultcode)
       (list (update-widget
              'image-view (get-id "image")
              'external-image (string-append dirname "photo.jpg"))))
      (else
       '()))))


  (activity
   "village"
   (let ((place-widgets
          (lambda (id shade)
            (horiz-colour
             (if shade colour-one colour-two)
             (mtoggle-button-scale id (lambda (v) '()))
             (medit-text-scale 'closest-access "normal" (lambda (v) '()))
             (vert
              (mbutton-scale 'gps (lambda ()  '()))
              (mtext-small 'test-num)
              (mtext-small 'test-num))))))
     (build-activity
      (horiz
       (medit-text 'village-name "normal" (lambda (v) (entity-add-value! "name" "varchar" v) '()))
       (medit-text 'block "normal" (lambda (v) (entity-add-value! "block" "varchar" v) '())))
      (horiz
       (medit-text 'district "normal" (lambda (v) (entity-add-value! "district" "varchar" v) '()))
       (mtoggle-button-scale 'car (lambda (v) (entity-add-value! "car" "int" v) '())))

      (mbutton 'household-list
               (lambda ()
                 (list (start-activity "household-list" 0
                                       (get-current 'village #f)))))

      (mtitle 'amenities)
      (place-widgets 'school #t)
      (place-widgets 'hospital #f)
      (place-widgets 'post-office #t)
      (place-widgets 'railway-station #f)
      (place-widgets 'state-bus-service #t)
      (place-widgets 'district-bus-service #f)
      (place-widgets 'panchayat #t)
      (place-widgets 'NGO #f)
      (place-widgets 'market #t)
      (delete-button)))
   (lambda (activity arg)
     (set-current! 'activity-title "Village")
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "sync" "village" (get-entity-by-unique db "sync" arg))
     (set-current! 'village arg)
     (list
      (mupdate 'edit-text 'village-name "name")
      (mupdate 'edit-text 'block "block")
      (mupdate 'edit-text 'district "district")
      (mupdate 'toggle-button 'car "car")))
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
     (list
      (ktv "name" "varchar" (mtext-lookup 'default-household-name))
      (ktv "num-pots" "int" 0)
      (ktv "house-lat" "real" 0) ;; get from current location?
      (ktv "house-lon" "real" 0)
      (ktv "toilet-lat" "real" 0)
      (ktv "toilet-lon" "real" 0))))
   (lambda (activity arg)
     (set-current! 'activity-title "Household List")
     (activity-layout activity))
   (lambda (activity arg)
     (msg "rebuilding household list with" arg)
     (list (update-list-widget
            db "sync" "household" "household" arg)))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "household"
   (build-activity
    (horiz
     (medit-text 'household-name "normal" (lambda (v) (entity-add-value! "name" "varchar" v) '()))
     (medit-text 'num-pots "numeric" (lambda (v) (entity-add-value! "num-pots" "int" v) '())))
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
     db "sync" 'individuals "individual" "individual" (lambda () (get-current 'household #f))
     (list
      (ktv "name" "varchar" (mtext-lookup 'default-individual-name))
      (ktv "family" "varchar" (mtext-lookup 'default-family-name))
      (ktv "photo-id" "varchar" (mtext-lookup 'default-photo-id))
      (ktv "photo" "file" "none")
      (ktv "tribe" "varchar" "none")
      (ktv "subtribe" "varchar" "none")
      (ktv "age" "int" 0)
      (ktv "gender" "varchar" "female")
      (ktv "education" "varchar" "none")))
    (delete-button))
   (lambda (activity arg)
     (set-current! 'activity-title "Household")
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "sync" "household" (get-entity-by-unique db "sync" arg))
     (set-current! 'household arg)
     (append
      (list
       (update-list-widget db "sync" "individual" "individual" arg)
       (mupdate 'edit-text 'household-name "name")
       (mupdate 'edit-text 'num-pots "num-pots"))
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
      (mtext 'family-display)
      (mtext 'photo-id-display)))
    (mbutton 'agreement-button (lambda () (list (start-activity "agreement" 0 ""))))
    (horiz
     (mbutton-scale 'details-button (lambda () (list (start-activity "details" 0 ""))))
     (mbutton-scale 'family-button (lambda () (list (start-activity "family" 0 "")))))
    (horiz
     (mbutton-scale 'migration-button (lambda () (list (start-activity "migration" 0 ""))))
     (mbutton-scale 'income-button (lambda () (list (start-activity "income" 0 "")))))
    (horiz
     (mbutton-scale 'geneaology-button (lambda () (list (start-activity "geneaology" 0 ""))))
     (mbutton-scale 'social-button (lambda () (list (start-activity "social" 0 "")))))
    (spacer 20)
    (delete-button))

   (lambda (activity arg)
     (set-current! 'activity-title "Individual")
     (activity-layout activity))
   (lambda (activity arg)
     (entity-init! db "sync" "individual" (get-entity-by-unique db "sync" arg))
     (set-current! 'individual arg)
     (list
      (mupdate 'text-view 'name-display "name")
      (mupdate 'text-view 'family-display "family")
      (mupdate 'text-view 'photo-id-display "photo-id")
      (mupdate 'image-view 'photo "photo")))
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
         (list
          (take-photo (string-append dirname "files/" (entity-get-value "unique_id") "-face.jpg") photo-code))
         )))

     (vert
      (medit-text 'details-name "normal" (lambda (v) (entity-add-value! "name" "varchar" v) '()))
      (medit-text 'details-family "normal" (lambda (v) (entity-add-value! "family" "varchar" v) '()))
      (medit-text 'details-photo-id "normal" (lambda (v) (entity-add-value! "photo-id" "varchar" v) '()))))
    (mspinner-other 'tribe '(one two three) (lambda (v) (msg "tribe now:" v) (entity-add-value! "tribe" "varchar" v) '()))
    (mspinner-other 'sub-tribe '(one two three) (lambda (v) (entity-add-value! "subtribe" "varchar" v) '()))
    (horiz
     (medit-text 'age "numeric" (lambda (v) (entity-add-value! "age" "int" v) '()))
     (mspinner 'gender '(male female) (lambda (v) (entity-add-value! "gender" "varchar" v) '()))
     (mspinner 'education '(one two three) (lambda (v) (entity-add-value! "education" "varchar" v) '())))
    )
   (lambda (activity arg)
     (set-current! 'activity-title "Individual details")
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (mupdate 'edit-text 'details-name "name")
      (mupdate 'edit-text 'details-family "family")
      (mupdate 'edit-text 'details-photo-id "photo-id")
      (mupdate 'image-view 'photo "photo")
      (mupdate-spinner-other 'tribe "tribe" '(one two three))
      (mupdate-spinner-other 'sub-tribe "subtribe" '(one two three))
      (mupdate 'edit-text 'age "age")
      (mupdate-spinner 'gender "gender" '(male female))
      (mupdate-spinner 'education "education" '(one two three))
      ))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (msg "back from camera")
     (cond
      ((eqv? requestcode photo-code)
       ;; todo: means we save when the camera happens
       ;; need to do this before init is called again in on-start,
       ;; which happens next
       (let ((unique-id (entity-get-value "unique_id")))
         (entity-add-value! "photo" "file" (string-append unique-id "-face.jpg"))
         (entity-update-values!)
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
      (mspinner 'head-of-house '(male female) (lambda (v) '()))
      (mspinner 'marital-status '(ever-married currently-married currently-single seperated) (lambda (v) '()))
      (medit-text 'times-married "numeric" (lambda (v) '())))
     (vert
      (mtitle 'spouse)
      (image-view (make-id "spouse-image") "face" (layout 240 320 -1 'centre 0))
      (mbutton 'change-spouse (lambda () '()))))

    (mtitle 'children)
    (horiz
     (medit-text 'living "numeric" (lambda (v) '()))
     (medit-text 'dead "numeric" (lambda (v) '())))
    (horiz
     (medit-text 'together "numeric" (lambda (v) '()))
     (medit-text 'apart "numeric" (lambda (v) '())))
    (mspinner-other 'residence-after-marriage '(birthplace spouse-village) (lambda (v) '()))
    (medit-text 'num-siblings "numeric" (lambda (v) '()))
    (medit-text 'birth-order "numeric" (lambda (v) '())))
   (lambda (activity arg)
     (set-current! 'activity-title "Individual family")
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "migration"
   (build-activity
    (medit-text 'length-time "numeric" (lambda (v) '()))
    (medit-text 'place-of-birth "normal" (lambda (v) '()))
    (medit-text 'num-residence-changes "numeric" (lambda (v) '()))
    (medit-text 'village-visits-month "numeric" (lambda (v) '()))
    (medit-text 'village-visits-year "numeric" (lambda (v) '()))
    )
   (lambda (activity arg)
     (set-current! 'activity-title "Individual migration")
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "income"
   (build-activity
    (horiz
     (mspinner 'occupation '(agriculture gathering labour cows fishing other)
               (lambda (v) '()))
     (medit-text 'num-people-in-house "numeric" (lambda (v) '())))
    (horiz
     (mtoggle-button-scale 'contribute (lambda (v) '()))
     (mtoggle-button-scale 'own-land (lambda (v) '())))
    (horiz
     (mtoggle-button-scale 'rent-land (lambda (v) '()))
     (mtoggle-button-scale 'hire-land (lambda (v) '())))
    (mtitle 'crops)
    (horiz
     (mtext-scale 'crop) (mtext-scale 'unit) (mtext-scale 'quantity)
     (mtext-scale 'used-or-eaten) (mtext-scale 'sold) (mtext-scale 'seed))
    (mspinner-other 'house-type '(concrete tin thatched) (lambda (v) '()))
    (horiz
     (medit-text 'loan "numeric" (lambda (v) '()))
     (medit-text 'earning "numeric" (lambda (v) '())))
    (mtext 'in-the-home)
    (horiz
     (mtoggle-button-scale 'radio (lambda (v) '()))
     (mtoggle-button-scale 'tv (lambda (v) '()))
     (mtoggle-button-scale 'mobile (lambda (v) '())))
    (horiz
     (medit-text 'visit-market "numeric" (lambda (v) '()))
     (medit-text 'town-sell "numeric" (lambda (v) '())))
    )
   (lambda (activity arg)
     (set-current! 'activity-title "Individual income")
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "geneaology"
   (build-activity
    (horiz
     (vert
      (mtitle 'mother)
      (image-view (make-id "image") "face" (layout 240 320 -1 'centre 0))
      (mbutton 'change-mother (lambda () '())))
     (vert
      (mtitle 'father)
      (image-view (make-id "image") "face" (layout 240 320 -1 'centre 0))
      (mbutton 'change-father (lambda () '()))))
    (mtitle 'children)
    (horiz
     (medit-text 'name "normal" (lambda (v) '()))
     (mtoggle-button-scale 'alive (lambda (v) '()))
     (mspinner 'sex '(female male) (lambda (v) '()))
     (medit-text 'age "numeric" (lambda (v) '())))
    )
   (lambda (activity arg)
     (set-current! 'activity-title "Individual geneaology")
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "social"
   (build-activity
    )
   (lambda (activity arg)
     (set-current! 'activity-title "Individual social network")
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "agreement"
   (build-activity
    )
   (lambda (activity arg)
     (set-current! 'activity-title "Individual agreement")
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "individual-chooser"
   (build-activity
    )
   (lambda (activity arg)
     (set-current! 'activity-title "Individual chooser")
     (activity-layout activity))
   (lambda (activity arg) '())
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
    (horiz
     (mtoggle-button-scale 'sync-all (lambda (v) (set-current! 'sync-on v) '()))
     (mbutton-scale 'sync-syncall
               (lambda ()
                 (let ((r (append
                           (spit db "sync" (dirty-and-all-entities db "sync"))
                           (spit db "stream" (dirty-and-all-entities db "stream")))))
                   (cons (toast "Uploading data...") r)))))
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
                      (string-append url "fn=entity-csv&table=stream&type=" e)
                      (string-append "/sdcard/mongoose/" e ".csv"))
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
                   "From Symbai" "Please find attached your mongoose data"
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
