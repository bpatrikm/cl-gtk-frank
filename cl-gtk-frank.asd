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

(defsystem "cl-gtk-frank"
  :serial t
  :components ((:file "packages")
               (:file "signal-generics")
               (:file "widgets")
               (:file "builder")
               (:file "tree-store-basic")
               (:file "viewer-update-requests")
               (:file "content-holder")
               (:file "tree-layer")
               (:file "tree-viewer"))
  :depends-on (cl-cffi-gtk))
