(defun helloworld () ; defun define function
  (format t "hello-world")) ;format equivalent to printf but more powerfull


(load "helloworld.lisp") ; load file to repl or ,rl

(load
  (compile-file "helloworld.lisp")) ; create compile to fasl file and load it
                                    ; can use ,of




