#-(or sbcl clisp) (error "unimplemented")

(in-package :stumpwm)

(defstruct pwsafe-entry password name user-name)

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

(defun pwsafe-entry-from-line (password line) 
  (let ((pair (pair-split line "  -  ")))
    (make-pwsafe-entry :password password 
                       :name (car pair)
                       :user-name (cdr pair))))
         
(defun pwsafe-entries (password)
  (let ((output (run-shell-command (pwsafe-command password '("-l")) t)))
         (mapcar 
          (lambda (line) (pwsafe-entry-from-line password line)) 
          (lines-from-string output))))

(defun assoc-entries (entries)
  (mapcar (lambda (entry) (cons (pwsafe-entry-name entry) entry)) entries))

(defun pwsafe-password-to-clipboard (entry)
  (let* ((pwsafe-entry (pwsafe-entry-name entry))
         (output (run-shell-command (pwsafe-command (pwsafe-entry-password entry) '("-q " "-p " "--echo " "-E ") pwsafe-entry) t))
         (entry-password (cdr (pair-split output "passphrase for.*: "))))
    (set-x-selection entry-password)
    (run-shell-command (with-xsel (format nil "echo \"~a\"" entry-password) "-ib") t)
    (message (format nil "Username: ~a (password copied to clipboard)" (pwsafe-entry-user-name entry)))))

(defcommand pwsafe-menu (password) ((:password "Pwsafe password: "))
  (let* ((entries (pwsafe-entries password))
         (entry (select-from-menu (current-screen) 
                                  (assoc-entries entries))))
    (pwsafe-password-to-clipboard (cdr entry))))
                         
(define-stumpwm-type :pwsafe-entry (input prompt)
  (or (argument-pop input)
      (let* ((password (read-one-line (current-screen) "Pwsafe password: " :password t))
             (entries (pwsafe-entries password))
             (entries-assoc (assoc-entries entries))
             (entry-name (completing-read (current-screen)
                         prompt
                         entries-assoc)))
        (cdr (assoc entry-name entries-assoc :test #'equal)))))
             

(defcommand pwsafe-pass (entry) ((:pwsafe-entry "Pwsafe entry: "))
  (pwsafe-password-to-clipboard entry))
