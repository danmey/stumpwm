(in-package :stumpwm)

(export '(define-mode))

(defun postfix-name (base-name postfix)
  (concat base-name "-" postfix))

(defun var-name (base-name)
  (format nil "*~(~a~)*" base-name))

(defun mksym (name)
  (intern (format nil "~:@(~a~)" name)))

(defmacro define-mode (base-name &rest body)
  (let* ((base-name (symbol-name base-name))
         (mode-name (postfix-name base-name "mode"))
         (hook-name (var-name (postfix-name mode-name "hook")))
         (map-name (var-name (postfix-name mode-name "map")))
         (hook-sym (mksym hook-name)))
    `(progn (setq ,hook-sym "ala ma kota")
            (export '(,hook-sym))
            (defcommand ,(mksym mode-name) () ()
              ,@body))))

(define-mode test ()
  (message *test-mode-hook*)
)

(define-mode test3 ()
  (message *test3-mode-hook*)
)
