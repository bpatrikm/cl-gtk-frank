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

(in-package :cl-gtk-frank.viewer-update-requests)

(defclass viewer (widget)
  ())

;;every layer must have method for list-viewers, which returns the viewer(s) that must be updated if the layer has been changed (as a list)
(defgeneric list-viewers (changed-layer)
  )

;;update-all-viewers -> update-viewer genom maskineri som även omfattar andra vy-typer (som kan ha vyer av symbol-lagren)
(defgeneric update-viewer (viewer changed-layers)
  )

;;För att göra viewern klar efter initiering. Ungefär samma process som senare en uppdatering skulle föranleda
(defgeneric draw-viewer (viewer)
  )

(defgeneric remove-content-layer (viewer layer)
  )
;;varje viewer skall också specialisera en metod remove-content-holder, som ch anropar en viewer med när den blir borttagen från denna (och därför inte kan rapportera den i list-viewers-anropet).

;;i det här fallet skall ch alltid vara ett item-layer
;;i den metoden skall content-holder tas bort från vyn, och en uppdatering genomföras (out of band)

(defun update-all-viewers (changed-layers)
  (let ((changed-layers (remove-duplicates changed-layers)));of layers
    (dolist (viewer (remove-duplicates;of viewers
                     (reduce #'append (mapcar #'list-viewers changed-layers))))
      (let ((changed-layers (remove-if-not (lambda (layer)
                                             (find viewer (list-viewers layer)))
                                           changed-layers)))
        (update-viewer viewer changed-layers)))))

;;every viewer must specialise
(defgeneric list-viewed-layers (viewer)
  )

;;every viewed layer must specialise
(defgeneric remove-viewer (layer viewer)
  )

(defmethod destroy ((v viewer))
  (dolist (layer (list-viewed-layers v))
    (remove-viewer layer v))
  (call-next-method))
