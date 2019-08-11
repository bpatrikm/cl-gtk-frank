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
;;along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-gtk-frank.builder)

(defclass glade-object ()
  ((name :initform nil)
   (content :initform nil)))

(defun read-glade-xml (in)
  (flet ((read-tag (stream char)
           (declare (ignore char))
           (let ((*readtable* (copy-readtable))
                 (*package* (find-package :cl-gtk-frank.builder)))
             (set-macro-character #\> (lambda (stream char)(declare (ignore stream char)) '>))
             (let ((tag (read stream)))
               (set-macro-character #\= (lambda (stream char)(declare (ignore stream char)) '=))
               (set-macro-character #\/ (lambda (stream char)(declare (ignore stream char)) '/))
               (let ((read-in-tag
                      (do (read-in-tag ;; läs av allt t o m #\> och lägg i en lista
                           (inp (read stream) (read stream)))
                          ((eql inp '>)
                           (nreverse read-in-tag))
                        (push inp read-in-tag))))
                 ;;behöver bara titta specifikt på object, /object och signal (som slutar med en /> redan i taggen)
                 (cond ((eql tag 'object)
                        (let ((object (make-instance 'glade-object))
                              (id-pos (position 'id read-in-tag)))
                          (when id-pos (setf (slot-value object 'name)
                                             (elt read-in-tag (+ 2 id-pos))))
                          object))
                       ((eql tag 'signal)
                        (let ((id-pos (position 'name read-in-tag)))
                          (elt read-in-tag (+ 2 id-pos))))
                       ((eql tag '/object)
                        tag)
                       (t
                        'dummy)))))))
    (let ((*readtable* (copy-readtable))
          (*package* (find-package :cl-gtk-frank.builder)))
      (set-macro-character #\< #'read-tag)
      (set-macro-character #\| (lambda (stream char)(declare (ignore stream char)) 'nodlinje))
      (do (content (inp (read in nil nil)
                        (read in nil nil)))
          ((or (not inp)
               (eql inp '/object))
           (nreverse (remove 'dummy content)))
        (when (typep inp 'glade-object)
          (setf (slot-value inp 'content)
                (read-glade-xml in)))
        (when (or (typep inp 'glade-object)
                  (stringp inp))
          (push inp content))))))

(defun serialise-glade-xml (xml-scan-object)
  (let ((object-copy (make-instance 'glade-object)))
    (setf (slot-value object-copy 'name)
          (slot-value xml-scan-object 'name))
    (setf (slot-value object-copy 'content)
          (remove-if-not #'stringp (slot-value xml-scan-object 'content)))
    (append (list object-copy)
            (reduce #'append
                    (map 'list #'serialise-glade-xml
                         (remove-if #'stringp (slot-value xml-scan-object 'content)))))))

(defun build-interface (filename)
  (let ((builder (make-instance 'gtk-builder)))
    (gtk-builder-add-from-file builder filename)
    builder))

(defun build-interface-connect-signals (builder filename instance-id)
  (let ((xml-scan (remove-if-not (lambda (o)
                                   (slot-value o 'content))
                                 (reduce #'append
                                         (map 'list #'serialise-glade-xml
                                              (with-open-file (in filename)
                                                (read-glade-xml in)))))))
    ;;connect-result, as needed by with-build-interface
    (mapcar (lambda (object)
              (cons (slot-value object 'name)
                    (let ((widget (gtk-builder-get-object builder (slot-value object 'name)))
                          (widget-id (list (intern (symbol-name instance-id) (find-package :cl-gtk-frank))
                                           (intern (string-upcase (slot-value object 'name)) (find-package :cl-gtk-frank)))))
                      (mapcar (lambda (signal-name)
                                (let ((signal-symbol (intern (string-upcase signal-name) (find-package :cl-gtk-frank.generics))))
                                  (cons signal-symbol
                                        (g-signal-connect widget
                                                          signal-name
                                                          (make-callback signal-symbol widget-id)))))
                              (slot-value object 'content)))))
            xml-scan)))
;;defgeneric på signaler i cl-gtk-frank först

(defmacro with-build-interface ((filename instance-id builder &optional result-is-map-p) &body body)
  (if result-is-map-p

      (let ((widget-map (gensym));(list (cons static-id-string widget-instance) ...)
            (connect-result (gensym));(list (cons static-id-string (list (cons signal-symbol handler-id) ...)) ...)
            (bodyresult (gensym))
            (id-inst-cons (gensym))
            (e-filename (gensym)))
        `(let* ((,e-filename ,(eval filename))
                (,builder (build-interface ,e-filename)))
           (multiple-value-bind (,widget-map ,bodyresult)
               (progn
                 ,@body)
             (let ((,connect-result
                    (build-interface-connect-signals ,builder ,e-filename ,instance-id)))
               (dolist (,id-inst-cons ,widget-map)
                 (register-widget (cdr ,id-inst-cons)
                                  ,instance-id
                                  (intern (string-upcase (car ,id-inst-cons))))
                 (setf (handler-ids (cdr ,id-inst-cons))
                       (cdr (find (car ,id-inst-cons)
                                  ,connect-result :key #'car :test #'equal)))))
             ,bodyresult)))
      
      (let ((bodyresult (gensym))
            (e-filename (gensym)))
        `(let* ((,e-filename ,(eval filename))
                (,builder (build-interface ,e-filename))
                (,bodyresult (progn
                               ,@body)))
           (build-interface-connect-signals ,builder ,e-filename ,instance-id)
           ,bodyresult))))
