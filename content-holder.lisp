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

(in-package :cl-gtk-frank.content-holder)

;;content-holder content-holder-modifyable content-holder-selectable content-holder-selectable-modifyable

(defclass content-holder ()
  ((content :initarg :content :reader content)
   (selection :initform nil :reader selection)
   (viewers :initarg :viewers :initform nil :accessor viewers)))

(defclass content-holder-modifyable (content-holder)
  ((content :initform nil :initarg :content :accessor content)))

(defclass content-holder-selectable-m ()
  ((selection :initform nil :accessor selection)))

(defclass content-holder-selectable (content-holder content-holder-selectable-m)
  ())

(defclass content-holder-selectable-modifyable (content-holder-modifyable content-holder-selectable-m)
  ())

(defmethod add-to-selection ((ch content-holder-selectable-m) items)
  (let ((owned-not-selected (set-difference (intersection items (content ch))
                                            (selection ch))))
    (when owned-not-selected
      (setf (selection ch)
            (append (selection ch) owned-not-selected))
      owned-not-selected)))

(defmethod remove-from-selection ((ch content-holder-selectable-m) items)
  (let ((owned-selected (intersection (selection ch) items)))
    (when owned-selected
      (setf (selection ch)
            (set-difference (selection ch) owned-selected))
      owned-selected)))

(defmethod add-to-content ((ch content-holder-modifyable) items)
  (setf (content ch)
        (union (content ch) items)))

(defmethod remove-from-content ((ch content-holder-modifyable) items)
  (setf (content ch)
        (set-difference (content ch) items)))

(defmethod remove-from-content ((ch content-holder-selectable-modifyable) items)
  (setf (content ch)
        (set-difference (content ch) items))
  (setf (selection ch)
        (set-difference (selection ch) items)))

(defmethod replace-content ((ch content-holder-modifyable) items)
  (setf (content ch)
        items))

(defmethod replace-content ((ch content-holder-selectable-modifyable) items)
  (setf (content ch)
        items)
  (setf (selection ch)
        (intersection (selection ch) items)))

(defmethod list-viewers ((ch content-holder))
  (viewers ch))

(defmethod add-to-viewer ((ch content-holder) viewer)
  (push viewer (viewers ch)))

;;this is for when the layer is being removed from a viewer
(defmethod remove-from-viewer ((ch content-holder) viewer)
  (setf (viewers ch)
        (remove viewer (viewers ch)))
  (remove-content-layer viewer ch))

;;this is for when the viewer is being removed
(defmethod remove-viewer ((ch content-holder) viewer)
  (setf (viewers ch)
        (remove viewer (viewers ch))))
