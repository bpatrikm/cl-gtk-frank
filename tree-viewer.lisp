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

(in-package :cl-gtk-frank.tree-viewer)

;;contents subclassed from tree-row
(defclass list-layer (store-content-layer content-holder)
  ())

(defmethod all-store-contents ((list-layer list-layer))
  (content list-layer))

(defclass tree-viewer-selection-mirrored (tree-store)
  ())
;;dessa använder gtk's selection som ekvivalent till scl's selection, alltså skall de inte hålla något verktygsspecifikt där, och skall vara beredda att göra present-selection utåt - ifall selection ändrats av någon annan vyn kopplad till samma scl

(defclass tree-viewer-excl (tree-viewer-selection-mirrored)
  ;;i default changed-metod uppdateras alltid selection i scl, men ifall single-p anropas bara changed-enriched när selected-rows är 1 element
  ((single-p :initarg :single-p :initform nil :reader single-p)));enda ägaren till store-content-layer. Detta utgör ett specialfall där vyn/verktygen kan använda selection i store-content-layer för att styra vad gtk visar som selection, även om de inte själva ger den specifika betydelsen till raderna som kommer in. I det allmänna fallet kan det finnas fler än en vy per lager - de som vill göra gtk's selection ekvivalent med scl's selection måste spela efter de reglerna, och övriga vyer skall inte röra scl's selection, utan ev ha någon egen spegling av gtk's selection
(defmethod selection ((viewer tree-viewer-selection-mirrored))
  (if (single-p viewer)
      (when (selection (store-content-layer viewer))
        (car (selection (store-content-layer viewer))))
      (selection (store-content-layer viewer))))

;;för en tree-viewer-excl är den enda källan till det här anropet en update-all-viewers som den själv gjort. Ifall den har en modifyable scl, men bara ändrat selection, kan den välja explicit anrop till present-selection istället
(defmethod update-viewer ((viewer tree-viewer-selection-mirrored) changed-layer)
  (typecase (store-content-layer viewer)
    ((or content-holder-modifyable tree-layer)
     (repopulate-tree-store viewer)))
  (with-store-signal-blocked viewer
    (present-selection viewer (selection (store-content-layer viewer)))))

;;only formally needed
(defmethod draw-viewer ((viewer tree-store))
  (update-viewer viewer (list (store-content-layer viewer))))
  
;;bättre att bara göra destroy på hela store. Om man vill lägga in en ny scl måste den vara kompatibel med nuvarande
#|
(defmethod remove-content-layer ((lv list-viewer) (ll list-layer))
  (setf (store-content-layer lv) nil)
  (update-viewer lv))
|#

;;caller should change-class the resulting tree-store into target tree-viewer-class
(defmethod make-tree-viewer ((scl store-content-layer) instance-symbol static-symbol connect-changed-p connect-destroy-p connect-row-activated-p)
  (let ((tree-store (make-tree-store scl))
        (widget-id (list (intern (symbol-name instance-symbol)
                                 (find-package :cl-gtk-frank))
                         (intern (symbol-name static-symbol)
                                 (find-package :cl-gtk-frank)))))
    (add-to-viewer scl tree-store)
    (when connect-changed-p
      ;;As this command executes, there may be changed-signals emitted
      (connect-tree-view-selection-changed tree-store instance-symbol static-symbol))
    (when connect-destroy-p
      (push (cons 'destroy
                  (g-signal-connect (gtk-view tree-store)
                                    "destroy"
                                    (make-callback 'destroy widget-id)))
            (handler-ids tree-store)))
    (when connect-row-activated-p
      (push (cons 'row-activated
                  (g-signal-connect (gtk-view tree-store)
                                    "row-activated"
                                    (make-callback 'row-activated widget-id)))
            (handler-ids tree-store)))
    tree-store))

(defgeneric changed-enriched (viewer gtk-selected-rows)
  )

(defmethod changed-enriched (viewer gtk-selected-rows)
  )

(defmethod changed ((viewer tree-viewer-selection-mirrored))
  (let ((selected-rows (get-gtk-selection viewer)))
    (setf (selection (store-content-layer viewer)) selected-rows)
    (changed-enriched viewer selected-rows)))

(defmethod changed ((viewer tree-viewer-excl))
  (let ((selected-rows (get-gtk-selection viewer)))
    (if (single-p viewer)
        (when (and selected-rows
                   (not (cdr selected-rows)))
          (setf (selection (store-content-layer viewer)) selected-rows)
          (changed-enriched viewer (car selected-rows)))
        (progn
          (setf (selection (store-content-layer viewer)) selected-rows)
          (changed-enriched viewer selected-rows)))))

;;efter att ha kört med  with-store-signal-blocked (implicit i uppdateringar)
(defmethod verify-selection ((viewer tree-viewer-selection-mirrored))
  (let ((selected-rows (get-gtk-selection viewer)))
    (unless (equal selected-rows (selection (store-content-layer viewer)))
      (setf (selection (store-content-layer viewer)) selected-rows)
      (changed-enriched viewer selected-rows))))

(defmethod verify-selection ((viewer tree-viewer-excl))
  (let ((selected-rows (get-gtk-selection viewer)))
    (unless (equal selected-rows (selection (store-content-layer viewer)))
      (if (single-p viewer)
          (when (and selected-rows
                     (not (cdr selected-rows)))
            (setf (selection (store-content-layer viewer)) selected-rows)
            (changed-enriched viewer (car selected-rows)))
          (progn
            (setf (selection (store-content-layer viewer)) selected-rows)
            (changed-enriched viewer selected-rows))))))
