;; two first tries

(defun test-+ ()
  (and
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4))) ;; good enough but know nothing about where stuff fail?



(defun test-+ ()
  (format t "~:[fail~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3));<- can move into macro
  (format t "~:[fail~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[fail~;pass~] ... ~a~%" (= (+ 1 2) 4) '(= (+ 1 2) 4)))

(test-+)

; refractor

(defun report-result
    (result form)
  (format t "~:[fail~;pass~] ... ~a~%" result form)) ; but result and form is
                                                     ; the same (one eval, one not)

(defmacro check
    (form)
  `(report-result ,form ',form))  ; but this still only one,
                                  ; still have to write every check for each test

(defmacro check
    (&body forms)
  `(progn
     ,@(loop for f in forms collect
             `(report-result ,f ',f)))) ; collect form an array,
                                                              ; @ splice it into parent


(defun test ()
  (check
    (= (+ 1 2 3) 5)
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)))

;; test should only return T when all cases are passed and
;; return only fail results if fail
;; ( and ) not working cause short-circut, one test fail it will return immediately
;; but we can write a macro for this
(defun report-result
    (result form)
  (format t "~:[fail~;pass~] ... ~a~%" result form)
  result)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))


(report-result (= (+ 1 2) 3) `(= (+ 1 2 ) 3))

(defmacro check
    (&body forms)
  `(combine-results
     ,@(loop for f in forms collect
             `(report-result ,f ',f))))

(macroexpand '(check
                 (= (+ 1 2 3) 5)
                 (= (+ 1 2) 3)
                 (= (+ 1 2 3) 6)))

;; better result reporting
;; if so many test-suite, then we dont know which suite is failing

;; dynamic-variable come to save the day

(defvar *test-name* nil)

(defun report-result
    (result form)
  (format t "~:[fail~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defun test-+ ()
  (let ((*test-name* 'test-+)) ;rebind  into proper test name
    (check
      (= (+ 1 2 3) 5)
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6))))

(defun test-* ()
  (let ((*test-name* 'test-*))
    (check
      (= (* 1 2 3) 6)
      (= (* 1 2) 2)
      (= (* 1 2 3) 6))))

(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(test-arithmetic)

;; but we can do better, cus we still have to manually bind *test-name*

(defmacro deftest (name params &body body)
  `(defun ,name ,params
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-+ ()
    (check
      (= (+ 1 2 3) 5)
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)))

(macroexpand '(deftest test-arithmetic ()
  (combine-results
    (test-+))))

(test-arithmetic)

