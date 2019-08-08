;;cl-gtk-frank, an abstraction layer for Gtk in LISP
;;(based on a subset of code which was named cl-gtk-cv)
;;Copyright (C) 2012-2013, 2019 Patrik Magnusson

;;This program is free software: you can redistribute it and/or modify
;;it under the terms of the GNU General Public License as published by
;;the Free Software Foundation, either version 3 of the License, or
;;(at your option) any later version.

;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.

;;You should have received a copy of the GNU General Public License
;;along with this program.  If not, see <http://www.gnu.org/licenses/>.(in-package :cl-gtk-frank.builder)

(in-package :cl-gtk-frank.widgets)

(defparameter *widgets* (make-hash-table :test #'equal))

(defclass widget ()
  ((handler-ids :initform nil :accessor handler-ids);(list (cons signal-symbol handler-id) ...)
   (destroy-hook :initarg :destroy-hook :initform (lambda ()))))

(defmethod destroy ((w widget))
  (let (wk)
    (maphash (lambda (k v)
               (when (eql v w)
                 (setf wk k)))
             *widgets*)
    (remhash wk *widgets*))
  (funcall (slot-value w 'destroy-hook)))

(defun register-widget (widget instance-symbol static-symbol)
  (let ((instance-symbol (intern (symbol-name instance-symbol) (find-package :cl-gtk-frank)))
        (static-symbol (intern (symbol-name static-symbol) (find-package :cl-gtk-frank))))
    (setf (gethash (list instance-symbol static-symbol) *widgets*)
          widget)))

(defun make-callback (signal-symbol widget-id)
  (case signal-symbol
    (clicked (lambda (widget)
               (declare (ignore widget))
               (clicked (gethash widget-id *widgets*))))
    (changed (lambda (widget)
               (declare (ignore widget))
               (changed (gethash widget-id *widgets*))))
    (destroy (lambda (widget)
               (declare (ignore widget))
               (destroy (gethash widget-id *widgets*))))
    (row-activated (lambda (widget path view-column)
                     (declare (ignore widget))
                     (row-activated (gethash widget-id *widgets*) path view-column)))
    (value-changed (lambda (widget &optional value);;OBS
                     (declare (ignore widget))
                     (value-changed (gethash widget-id *widgets*) value)))
    (selection-changed (lambda (widget)
                     (declare (ignore widget))
                     (selection-changed (gethash widget-id *widgets*))))
    (file-activated (lambda (widget)
                      (declare (ignore widget))
                      (file-activated (gethash widget-id *widgets*))))
    (button-press-event (lambda (widget event)
                          (declare (ignore widget))
                          (button-press-event (gethash widget-id *widgets*) event)))
    (button-release-event (lambda (widget event)
                            (declare (ignore widget))
                            (button-release-event (gethash widget-id *widgets*) event)))
    (motion-notify-event (lambda (widget event)
                           (declare (ignore widget))
                           (motion-notify-event (gethash widget-id *widgets*) event)))
    (draw (lambda (widget cairo)
            (declare (ignore widget))
            (draw (gethash widget-id *widgets*) (pointer cairo))));;OBS (pointer cairo)
    (toggled (lambda (widget)
               (declare (ignore widget))
               (toggled (gethash widget-id *widgets*))))
    (t (error "not done"))));en av dessa för varje signal

(defun connect-toggled (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "toggled" (make-callback 'toggled widget-id))))
    (when widget
      (push (cons 'toggled handler-id) (handler-ids widget)))
    handler-id))

(defun connect-draw (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "draw" (make-callback 'draw widget-id))))
    (when widget
      (push (cons 'draw handler-id) (handler-ids widget)))
    handler-id))

(defun connect-button-press-event (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "button-press-event" (make-callback 'button-press-event widget-id))))
    (when widget
      (push (cons 'button-press-event handler-id) (handler-ids widget)))
    handler-id))

(defun connect-button-release-event (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "button-release-event" (make-callback 'button-release-event widget-id))))
    (when widget
      (push (cons 'button-release-event handler-id) (handler-ids widget)))
    handler-id))

(defun connect-motion-notify-event (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "motion-notify-event" (make-callback 'motion-notify-event widget-id))))
    (when widget
      (push (cons 'motion-notify-event handler-id) (handler-ids widget)))
    handler-id))

(defun connect-file-activated (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "file-activated" (make-callback 'file-activated widget-id))))
    (when widget
      (push (cons 'file-activated handler-id) (handler-ids widget)))
    handler-id))

(defun connect-clicked (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "clicked" (make-callback 'clicked widget-id))))
    (when widget
      (push (cons 'clicked handler-id) (handler-ids widget)))
    handler-id))

(defun connect-changed (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "changed" (make-callback 'changed widget-id))))
    (when widget
      (push (cons 'changed handler-id) (handler-ids widget)))
    handler-id))

(defun connect-destroy (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "destroy" (make-callback 'destroy widget-id))))
    (when widget
      (push (cons 'destroy handler-id) (handler-ids widget)))
    handler-id))

(defun connect-row-activated (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "row-activated" (make-callback 'row-activated widget-id))))
    (when widget
      (push (cons 'row-activated handler-id) (handler-ids widget)))
    handler-id))
  
(defun connect-value-changed (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "value-changed" (make-callback 'value-changed widget-id))))
    (when widget
      (push (cons 'value-changed handler-id) (handler-ids widget)))
    handler-id))

(defun connect-selection-changed (gtk-widget instance-id static-id &optional widget)
  (let* ((widget-id (list (intern (symbol-name instance-id)
                                  (find-package :cl-gtk-frank))
                          (intern (symbol-name static-id)
                                  (find-package :cl-gtk-frank))))
         (handler-id (g-signal-connect gtk-widget "selection-changed" (make-callback 'selection-changed widget-id))))
    (when widget
      (push (cons 'selection-changed handler-id) (handler-ids widget)))
    handler-id))

;;åtminstone tre sätt att hitta handler-id för att blockera. Man kan ha sparat det explicit, man kan ha en widget (som innehåller en cons-lista) och man kan ha instance-id/static-id för att slå upp widget från *widgets*. I det första fallet behöver man inte specfikt ange vilken signal det är; handler-id är specifikt för det.

(defun get-handler-id (hid-or-widget-widget-id-signal &rest spec-rest)
  (let ((in hid-or-widget-widget-id-signal))
    (typecase in
      (number in)
      (widget (let* ((signal-symbol (first spec-rest))
                     (hid-cons (find signal-symbol (handler-ids in) :key #'car)))
                (when hid-cons (cdr hid-cons))))
      (symbol (let* ((instance-id in)
                     (static-id (first spec-rest))
                     (signal-symbol (second spec-rest))
                     (widget-id (list (intern (symbol-name instance-id)
                                              (find-package :cl-gtk-frank))
                                      (intern (symbol-name static-id)
                                              (find-package :cl-gtk-frank))))
                     (widget (gethash widget-id *widgets*))
                     (hid-cons (find signal-symbol (handler-ids widget) :key #'car)))
                (when hid-cons (cdr hid-cons))))
      (t (error "get-handler-id typecase")))))

;;använd get-handler-id (numerisk kan användas direkt istället)
(defmacro with-signal-blocked (gtk-widget handler-id &body body)
  `(progn
     (when ,handler-id
       (g-signal-handler-block ,gtk-widget ,handler-id))
     ,@body
     (when ,handler-id
       (g-signal-handler-unblock ,gtk-widget ,handler-id))))
