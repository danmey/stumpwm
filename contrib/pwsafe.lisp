;;; Interface to pwsafe keyring
;;;
;;; Copyright 2011 Wojciech Meyer
;;;
;;; Maintainer: Wojciech Meyer
;;;
;;; This module is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This module is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA
;;;

;;; USAGE:
;;;
;;; Put:
;;;
;;;     (load-module "pwsafe")
;;;
;;; ...into your ~/.stumpwmrc
;;;

;;; BUGS:
;;;
;;; Not very safe since the master password is not only stored in memory 
;;; but also echoed through unix pipe. 
;;; You've been warned.
;;; If anybody knows how to improve it just mail me or send a patch.

;;; CODE:


(defpackage :stumpwm.contrib.pwsafe
  (:use :common-lisp :stumpwm))

(in-package :stumpwm.contrib.pwsafe)


(defstruct pwsafe-entry
  "Our structure holding entry data. Master password stored for each
entry therefore VERY UNSAFE."
  password 
  name 
  user-name)

(defun lines-from-string (string)
  "Boiler plate for splitting STRING buffer to lines.  Rewritten
thousands of times already"
  (loop for i = 0 then (1+ j)
        as j = (position #\Newline string :start i)
        collect (subseq string i j)
        while j))

(defun pair-split (entry separator)
  "Split two ENTRY sections separated by SEPARATOR into pair"
  (let ((lst (cl-ppcre:split separator entry)))
    (cons (remove #\Newline (car lst)) (remove #\Newline (cadr lst)))))

(defun command-options (options &optional argument) 
  "Create command OPTIONS with optional ARGUMENTS.  BUGS: Need to
insert space after each switch, should interleave space."
  (format nil "~a ~a" 
          (if (listp options) 
              (apply #'concat options) options) 
          (or argument "") t))

(defun pwsafe-command (password options &optional argument) 
  "Execute pwsafe command using master PASSWORD with OPTIONS and
additional ARGUMENT. Not safe."
  (format nil "echo \"~a\" | pwsafe ~a" password (command-options options argument)))

(defun with-xsel (command &optional options)
  "Pipe COMMAND with OPTIONS to xsel."
  (format nil "~a | xsel ~a" command (or options "")))

(defun pwsafe-entry-from-line (password line) 
  "Create entry from LINE and master PASSWORD. Not safe."
  (let* ((line (or (cdr (pair-split line "Enter passphrase for.*:")) line))
         (pair (pair-split line "  -  ")))
    (make-pwsafe-entry :password password 
                       :name (car pair)
                       :user-name (cdr pair))))
         
(defun run-pwsafe-command (password options &optional argument)
  (let ((output (run-shell-command (pwsafe-command password options argument) t)))
    (when (cl-ppcre::scan "Passphrase is incorrect" output)
      (throw 'error "Passphrase is incorrect"))
    output))

(defun pwsafe-entries (password)
  "Get all the entries using master PASSWORD and spawning pwsafe
command"
  (let ((output (run-pwsafe-command password '("-l"))))
         (mapcar
          (lambda (line) (pwsafe-entry-from-line password line))
          (lines-from-string output))))

(defun assoc-entries (entries)
  "Create assoc from ENTRIES keyed by the name"
  (mapcar (lambda (entry) (cons (pwsafe-entry-name entry) entry)) entries))

(defun pwsafe-password-to-clipboard (entry)
  "Main function that will perform side action on ENTRY with all the
associated side effects like priting message and putting password into
xclipboard"
  (let* ((pwsafe-entry (pwsafe-entry-name entry))
         (output 
          (run-pwsafe-command (pwsafe-entry-password entry) 
                           ;; FIXME: dirty hack, attempted to be nice
                           ;; and keep the options in list, however
                           ;; `pwsafe-command' is not interleaving
                           ;; them with space
                           '("-q " "-p " "--echo " "-E ") pwsafe-entry))
         ;; FIXME: this one is a bit buggy, it should be really not
         ;; calling pair-split it might not work in all cases
         (entry-password (cdr (pair-split output "passphrase for.*: "))))
    (set-x-selection entry-password)
    (run-shell-command (with-xsel (format nil "echo \"~a\"" entry-password) "-ib") t)
    (message 
     (format nil "Username: ~a (password copied to clipboard)" 
             (pwsafe-entry-user-name entry)))))

(defcommand pwsafe-menu (password) ((:password "Pwsafe password: "))
  "Prompt for PASSWORD. Show menu with pwsafe database entries. Let
the user choose entry, put password to clipboard and notify user about
associated username"
  (let* ((entries (pwsafe-entries password))
         (entry (select-from-menu (current-screen) 
                                  (assoc-entries entries))))
    (unless entry
      (throw 'error :abort))
    (pwsafe-password-to-clipboard (cdr entry))))
                         
(define-stumpwm-type :pwsafe-entry (input prompt)
  "This is our type for prompting entry, and performing completion."
  (or (argument-pop input)
      (let ((password (read-one-line (current-screen) "Pwsafe password: " :password t)))
        (when password
          (let* ((entries (pwsafe-entries password))
                 (entries-assoc (assoc-entries entries))
                 (entry-name (completing-read (current-screen)
                                              prompt
                                              entries-assoc)))
            (cdr (assoc entry-name entries-assoc :test #'equal)))))))
             

(defcommand pwsafe-entry (entry) ((:pwsafe-entry "Pwsafe entry: "))
  "Prompt for ENTRY with completion, put password in clipboard and
notify user about associated username"
    (unless entry
      (throw 'error :abort))
    (pwsafe-password-to-clipboard entry))
