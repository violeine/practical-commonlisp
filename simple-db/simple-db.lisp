(list 1 2 3) ;make a list with list ==> (1 2 3)

; :a is a keyword, a special symbol that eval into itself
; plist (property list) like poor man hash map
; plist also create with list
(getf (list :a 1 :b 2 :c 3) :a) ; getf get element associate with :a => 1

(getf (list 1 3 3 4) 3) ;=> 4

(defun make-cd (title artist rating ripped)
  ; function CREATE a cd
  (list :title title :artist artist
        :rating rating :ripped ripped))

(make-cd "roses" "chainsmokin" 9 t)

(defvar *db* nil) ; define global variable *db* is naming convention for global

(defun add-record (cd)
  (push cd *db*)) ; push a cd into *db*; PUSH is a macro
;TODO comoment stuff from here
(add-record (make-cd "roses" "chainsmokin" 9 t))
(add-record (make-cd "waterbed" "chainsmokin" 10 nil))

*db*

(defun dump-db ()
  (dolist (cd *db*) ; loop over every cd in the *db*
    (format t "~{~a:~10t~a~%~}~%" cd))) ;; wtf???
; loop overlist
;~ is a format directive; ~a is aesthetic; ~t is tab; ~{ ~} is loop over list

(format t "~a:~10t~a" :artist "kenye") ;=> ARTIST:   kenye
(format t "~a" "Kenye") ; => kenye
(format t "~a" :keyword); => KEYWORD

(dump-db)


(defun prompt-read (prompt)
  (format *query-io* "~a: ~%" prompt)
  (force-output *query-io*) ; *query-io* is global for stream connect to tty
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]:"))
            (return)
            (format *query-io* "~%")))) ; for prompt yes properly newline
; (add-cds)

(defun save-db (name)
  (with-open-file (out name
                       :direction :output   ;write to output default is input
                       :if-exists :supersede) ; if exists => override
    (with-standard-io-syntax
      (print *db* out))))

(save-db "~/my-cds.db")
