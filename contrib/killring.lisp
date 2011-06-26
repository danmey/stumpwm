(defpackage :stumpwm.contrib.killring
  (:use :common-lisp :stumpwm))

(in-package :stumpwm.contrib.killring)

(defvar *kill-ring* '())

(defcommand push-kill-ring () ()
  (let ((selection (get-x-selection)))
    (push selection *kill-ring*)))

(defcommand pop-kill-ring () ()
  (pop selection *kill-ring*))

(defcommand select-kill-ring () ()
  (stumpwm::select-from-menu (current-screen) *kill-ring*))


