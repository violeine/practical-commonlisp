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
  (with-open-file (out name                 ; is the location, a str based on OS
                       :direction :output   ;write to output default is read
                       :if-exists :supersede) ; if exists => override
    (with-standard-io-syntax ;ensure everything works
      (print *db* out)))) ;print is not format? can be read by repl reader?

(defun load-db (name)
  (with-open-file
      (in name)
    (with-standard-io-syntax
      (setf *db* (read in)))))

; (save-db "simple-db/my-cds.db")
 ;(load-db "simple-db/my-cds.db")

; QUERYING

(remove-if-not #'evenp '(1 2 3 4 5 6)) ;is filter

; #' to pass function as variable, otherwise, lisp lookup evenp as variable not
; and not function
; shorthand for (function evenp)

(remove-if-not #'(lambda (x) ; lambda is how to define an anonymous function
                   (= 1 (mod x 2))) '(1 2 3 4 5 6))

(defun select-by-artist ;;specific case
    (name)
  (remove-if-not #'(lambda (cd)
                     (equal (getf cd :artist) name))
                 *db*))

(defun pprint-db (l)
  (dolist (cd l)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(pprint-db (select-by-artist "chainsmokin"))

(defun select ;more general
    (selector-fn)
  (remove-if-not selector-fn *db*))

(select #'(lambda (cd)
            (equal (getf cd :artist) "chainsmokin"))) ; still alot, whatabout
                                                      ;:title? :rating?

;=> TODO write a function that generate selector-fn

(defun where
    (&key title artist rating
          (ripped nil ripped-p)) ;default to nil, and ripped-p check is in or not
  #'(lambda (cd)
      (and
        (if title (equal (getf cd :title) title) t)
        (if artist (equal (getf cd :artist) artist) t)
        (if rating (equal (getf cd :rating) rating) t)
        (if ripped-p (equal (getf cd :ripped) ripped) t))))

(select (where :artist "chainsmokin" ))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
          #'(lambda (row)
              (when (funcall selector-fn row)
                (if title    (setf (getf row :title) title))
                (if artist   (setf (getf row :artist) artist))
                (if rating   (setf (getf row :rating) rating))
                (if ripped-p (setf (getf row :ripped) ripped)))
              row) *db*)))

(defun delete-rows (selector-fn)
    (setf *db* (remove-if selector-fn *db*)))


;; problem: what if you want to add/remove  field, you have to add/remove  if
;; clause to update and where. And if you only supplied only title, why should
;; we care about other fields?

;; answer: write a function that produce only the bit you need ala macro


(defmacro backwards (exprs) (reverse exprs))

(backwards (1 2 3 +))

'(1 2 3) ; return (1 2 3) instead throwing error

'equal ; return symbol equal

(defun make-comparision-expr
    (field val)
  (list 'equal (list 'getf 'cd field) val))

(make-comparision-expr :rating 10);=> (EQUAL (GETF CD :RATING) 10)

`(1 2 3 ,(+ 1 2)) ;` unevaluate a form, "," escape it and eval the next form

(defun make-comparision-expr
    (field value)
  `(equal (getf cd ,field ) ,value))

(make-comparision-expr :rating 10)

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparision-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(macroexpand-1
  '(where :title "Give Us a Break" :ripped t))

(select (where :artist "chainsmokin" :ripped t))


