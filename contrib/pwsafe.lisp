#-(or sbcl clisp) (error "unimplemented")

(in-package :stumpwm)

(defcommand p () ()
  (load-module "pwsafe2"))

(defun lines-from-string (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\Newline string :start i)
        collect (subseq string i j)
        while j))

(defun pair-split (entry separator)
  (let ((lst (cl-ppcre:split separator entry)))
    (cons (remove #\Newline (car lst)) (remove #\Newline (cadr lst)))))

(defun command-options (options &optional argument) 
  (format nil "~a ~a" 
          (if (listp options) 
              (apply #'concat options) options) 
          (or argument "") t))

(defun pwsafe-command (password options &optional argument) 
  (format nil "echo \"~a\" | pwsafe ~a" password (command-options options argument)))

(defun with-xsel (command &optional options) 
  (format nil "~a | xsel ~a" command (or options "")))

(defcommand pwsafe3 (password) ((:password "Pwsafe password: "))
  (let* ((output (run-shell-command (pwsafe-command password '("-l")) t))
         (entries (lines-from-string output))
         (selected-entry (select-from-menu (current-screen) entries))
         (pwsafe-entry (car (pair-split selected-entry "  -  ")))
         (output (run-shell-command (pwsafe-command password '("-q " "-p " "--echo " "-E ") pwsafe-entry) t))
         (entry-password (cdr (pair-split output "passphrase for.*: "))))
    (set-x-selection entry-password)
    (run-shell-command (with-xsel (format nil "echo \"~a\"" entry-password) "-b") t)))

