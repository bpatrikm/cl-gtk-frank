;;cl-gtk-frank, an abstraction layer for Gtk in LISP
;;Copyright (C) 2019 Patrik Magnusson

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

(in-package :cl-gtk-frank.tree-layer)
;;alternativ till list-layer, subklassar inte content-holder, presenteras också i list-viewer
;;innehållet har trädstruktur, passar till scl-with-struct

;;har replace-content, parallellt med content-holder, som också måste följas med update-viewer någonstans
;;har direkta ersättnings metoder, som inte direkt relaterar till trädstrukturen, utan snarare till att gtk-vyn inte skall hoppa till annan plats pga ersättning med helt nytt innehåll. Istället gradvisa ändringar, som var och en måste direkt ut till gtk-vyn (eftersom man förlorar nödvändiga datan för att få in den på rätt plats annars). Dessa metoder skulle också kunna vara användbara för list-layer, men inte för content-holder generellt.

(defclass tree-layer (scl-with-struct);kan också göra mixin med scl-with-flex (men det relaterar inte direkt till funktionerna här)
  ((root-content :initarg :root-content :initform nil :accessor root-content)
   ;;till skillnad från content-holder, finns inga add-to-/remove-from-content. Men replace-content finns
   (selection :initarg :selection :initform nil :accessor selection)
   ;;till skillnad från content-holder, kan man här bara sätta/läsa selection med accessorn direkt
   (tree-stores :initarg :tree-stores :initform nil :accessor tree-stores)))

(defmethod all-store-contents ((tl tree-layer))
  (root-content tl))

(defmethod replace-content ((tl tree-layer) root-tree-rows)
  (setf (root-content tl) root-tree-rows))

;;Notera att anroparen separat måste byta ut innehållet i trädet (eftersom det är en alltför generell uppgift att göra här). Detta byter bara ut innehållet i rad-vyn till att matcha den nya raden. Gäller alla nedanstående direct-*
(defmethod direct-replace-row ((tl tree-layer) (old-row tree-row) (new-row tree-row))
  (dolist (tree-store (tree-stores tl));ännu bara principiellt möjligt att ha fler än en viewer/tree-store per tree-layer
    (update-row tree-store old-row new-row))
  (setf (iter new-row) (iter old-row)))

(defmethod direct-remove-row ((tl tree-layer) (target tree-row))
  (dolist (ts (tree-stores tl))
    (remove-row ts target)))

;;parent=nil->rotnivå
(defmethod direct-insert-row ((tl tree-layer) parent (new tree-row))
  (declare ((or null tree-row) parent))
  ;;lämplig iter till new före anrop till insert-row
  (dolist (ts (tree-stores tl))
    (insert-row ts parent new)))

(defmethod list-viewers ((tl tree-layer))
  (tree-stores tl))

(defmethod add-to-viewer ((tl tree-layer) (viewer tree-store))
  (push viewer (tree-stores tl)))

;;this is for when the viewer is being removed
(defmethod remove-viewer ((tl tree-layer) (viewer tree-store))
  (setf (tree-stores tl)
        (remove viewer (tree-stores tl))))

;;this is for when the layer is being removed from a viewer
(defmethod remove-from-viewer ((tl tree-layer) (viewer tree-store))
  (setf (tree-stores tl)
        (remove viewer (tree-stores tl)))
  (remove-content-layer viewer tl))
