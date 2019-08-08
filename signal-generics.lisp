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

(in-package :cl-gtk-frank.generics)

(defgeneric button-press-event (widget event)
  )

(defgeneric button-release-event (widget event)
  )

(defgeneric destroy (widget)
  )

(defgeneric clicked (widget)
  )

(defgeneric toggled (widget)
  )

(defgeneric activate (widget)
  )

(defgeneric motion-notify-event (widget event)
  )

(defgeneric value-changed (widget value)
  );i doc för spin-button saknas denna, men den finns för scale-button

(defgeneric selection-changed (widget)
  );file-chooser

(defgeneric file-activated (widget)
  );file-chooser

(defgeneric row-activated (widget path view-column)
  )

(defgeneric changed (widget)
  );gtk-tree-selection-widget

(defgeneric draw (widget cairo)
  )
