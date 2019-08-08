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

(defpackage cl-gtk-frank
  (:use :cl :gtk :glib :gobject));osv

(defpackage cl-gtk-frank.generics
  (:export clicked toggled activate value-changed selection-changed file-activated row-activated changed)
  (:import-from :gtk button-press-event button-release-event destroy motion-notify-event draw)
  (:use :cl))

(defpackage cl-gtk-frank.widgets
  (:export *widgets*
           widget
           handler-ids
           register-widget
           make-callback
           connect-clicked connect-changed connect-destroy connect-row-activated connect-value-changed connect-button-press-event connect-button-release-event connect-motion-notify-event connect-draw connect-toggled
           with-signal-blocked get-handler-id)
  (:import-from :gtk button-press-event button-release-event destroy motion-notify-event draw)
  (:use :cl :cl-gtk-frank.generics :gobject))

(defpackage cl-gtk-frank.builder
  (:export with-build-interface)
  (:import-from :cl-gtk-frank.widgets register-widget handler-ids make-callback)
  (:use :cl :gtk :gobject :cl-gtk-frank.generics))

(defpackage cl-gtk-frank.iter
  (:export iter))

(defpackage cl-gtk-frank.tree-store-basic
  (:export tree-row
           store-content-layer
           scl-with-flex
           scl-with-struct
           get-tree-children
           all-store-contents
           tree-store
           gtk-store
           gtk-view
           gtk-selection-widget
           populate-tree-store-simple
           populate-tree-store-medium
           repopulate-tree-store
           tree-view-selection
           get-gtk-selection
           present-selection
           make-tree-store
           connect-tree-view-selection-changed
           with-store-signal-blocked
           verify-selection
           update-row
           remove-row
           insert-row
           query-tree-row
           query-view-column-index)
  (:import-from :cl-gtk-frank.generics changed)
  (:import-from :cl-gtk-frank.iter iter)
  (:import-from :cl-gtk-frank.widgets widget register-widget make-callback)
  (:use :cl :gtk :gobject))

(defpackage cl-gtk-frank.viewer-update-requests
  (:export viewer
           list-viewers
           update-viewer
           draw-viewer
           update-all-viewers
           remove-content-layer
           list-viewed-layers
           remove-viewer)
  (:import-from :cl-gtk-frank destroy)
  (:import-from :cl-gtk-frank.widgets widget destroy)
  (:use :cl))

(defpackage cl-gtk-frank.content-holder
  (:export content-holder
           viewers
           content-holder-modifyable
           content-holder-selectable
           content-holder-selectable-modifyable
           content
           selection
           add-to-selection
           remove-from-selection
           add-to-content
           remove-from-content
           replace-content
           add-to-viewer
           remove-from-viewer)
  (:import-from :cl-gtk-frank.viewer-update-requests list-viewers remove-content-layer remove-viewer)
  (:use :cl))

(defpackage cl-gtk-frank.tree-layer
  (:import-from :cl-gtk-frank.iter iter)
  (:use :cl :cl-gtk-frank.tree-store-basic)
  (:import-from :cl-gtk-frank.content-holder selection viewers replace-content add-to-viewer remove-from-viewer);parallell implementering med samma namn
  (:import-from :cl-gtk-frank.widgets widget)
  (:import-from :cl-gtk-frank.viewer-update-requests update-viewer draw-viewer remove-content-layer list-viewers remove-viewer)
  (:export tree-layer
           root-content
           direct-replace-row
           direct-remove-row
           direct-insert-row))

(defpackage cl-gtk-frank.tree-viewer
  (:use :cl :gobject :cl-gtk-frank.tree-store-basic)
  (:import-from :cl-gtk-frank.tree-layer tree-layer)
  (:import-from :cl-gtk-frank.content-holder content-holder content-holder-modifyable content selection viewers add-to-viewer)
  (:import-from :cl-gtk-frank.widgets widget make-callback handler-ids)
  (:import-from :cl-gtk-frank.viewer-update-requests update-viewer draw-viewer remove-content-layer)
  (:import-from :gtk destroy)
  (:import-from :cl-gtk-frank.generics row-activated changed)
  (:export list-layer
           tree-viewer-selection-mirrored
           tree-viewer-excl
           make-tree-viewer
           changed-enriched))
(defpackage cl-gtk-frank.generics
  (:export clicked toggled activate value-changed selection-changed file-activated row-activated changed)
  (:import-from :gtk button-press-event button-release-event destroy motion-notify-event draw)
  (:use :cl))

(import '(gtk:button-press-event
          gtk:button-release-event
          gtk:destroy
          gtk:motion-notify-event
          gtk:draw
          cl-gtk-frank.generics:clicked
          cl-gtk-frank.generics:toggled
          cl-gtk-frank.generics:activate
          cl-gtk-frank.generics:value-changed
          cl-gtk-frank.generics:selection-changed
          cl-gtk-frank.generics:file-activated
          cl-gtk-frank.generics:row-activated
          cl-gtk-frank.generics:changed
          cl-gtk-frank.widgets:widget
          cl-gtk-frank.widgets:register-widget
          cl-gtk-frank.widgets:connect-clicked
          cl-gtk-frank.widgets:connect-changed
          cl-gtk-frank.widgets:connect-destroy
          cl-gtk-frank.widgets:connect-row-activated
          cl-gtk-frank.widgets:connect-value-changed
          cl-gtk-frank.widgets:connect-button-press-event
          cl-gtk-frank.widgets:connect-button-release-event
          cl-gtk-frank.widgets:connect-motion-notify-event
          cl-gtk-frank.widgets:connect-draw
          cl-gtk-frank.widgets:connect-toggled
          cl-gtk-frank.widgets:with-signal-blocked
          cl-gtk-frank.widgets:get-handler-id
          cl-gtk-frank.builder:with-build-interface
          cl-gtk-frank.content-holder:selection
          cl-gtk-frank.content-holder:content-holder-selectable
          cl-gtk-frank.content-holder:content-holder-modifyable
          cl-gtk-frank.content-holder:content-holder-selectable-modifyable
          cl-gtk-frank.content-holder:viewers
          cl-gtk-frank.content-holder:replace-content
          cl-gtk-frank.content-holder:content
          cl-gtk-frank.content-holder:add-to-content
          cl-gtk-frank.content-holder:add-to-selection
          cl-gtk-frank.content-holder:remove-from-content
          cl-gtk-frank.viewer-update-requests:draw-viewer
          cl-gtk-frank.viewer-update-requests:update-all-viewers
          cl-gtk-frank.tree-viewer:list-layer
          cl-gtk-frank.tree-viewer:tree-viewer-selection-mirrored
          cl-gtk-frank.tree-viewer:tree-viewer-excl
          cl-gtk-frank.tree-viewer:make-tree-viewer
          cl-gtk-frank.tree-viewer:changed-enriched
          cl-gtk-frank.tree-store-basic:gtk-view
          cl-gtk-frank.tree-store-basic:gtk-selection-widget
          cl-gtk-frank.tree-store-basic:tree-row
          cl-gtk-frank.tree-store-basic:store-content-layer
          cl-gtk-frank.tree-store-basic:scl-with-flex
          cl-gtk-frank.tree-store-basic:scl-with-struct
          cl-gtk-frank.tree-store-basic:all-store-contents
          cl-gtk-frank.tree-store-basic:get-tree-children
          cl-gtk-frank.tree-store-basic:repopulate-tree-store
          cl-gtk-frank.tree-store-basic:verify-selection
          cl-gtk-frank.tree-store-basic:query-tree-row
          cl-gtk-frank.tree-store-basic:query-view-column-index
          cl-gtk-frank.tree-layer:tree-layer
          cl-gtk-frank.tree-layer:root-content
          cl-gtk-frank.tree-layer:direct-replace-row
          cl-gtk-frank.tree-layer:direct-remove-row
          cl-gtk-frank.tree-layer:direct-insert-row)
        (find-package :cl-gtk-frank))

(export '(gtk:button-press-event
          gtk:button-release-event
          gtk:destroy
          gtk:motion-notify-event
          gtk:draw
          cl-gtk-frank.generics:clicked
          cl-gtk-frank.generics:toggled
          cl-gtk-frank.generics:activate
          cl-gtk-frank.generics:value-changed
          cl-gtk-frank.generics:selection-changed
          cl-gtk-frank.generics:file-activated
          cl-gtk-frank.generics:row-activated
          cl-gtk-frank.generics:changed
          cl-gtk-frank.widgets:widget
          cl-gtk-frank.widgets:register-widget
          cl-gtk-frank.widgets:connect-clicked
          cl-gtk-frank.widgets:connect-changed
          cl-gtk-frank.widgets:connect-destroy
          cl-gtk-frank.widgets:connect-row-activated
          cl-gtk-frank.widgets:connect-value-changed
          cl-gtk-frank.widgets:connect-button-press-event
          cl-gtk-frank.widgets:connect-button-release-event
          cl-gtk-frank.widgets:connect-motion-notify-event
          cl-gtk-frank.widgets:connect-draw
          cl-gtk-frank.widgets:connect-toggled
          cl-gtk-frank.widgets:with-signal-blocked
          cl-gtk-frank.widgets:get-handler-id
          cl-gtk-frank.builder:with-build-interface
          cl-gtk-frank.content-holder:selection
          cl-gtk-frank.content-holder:content-holder-selectable
          cl-gtk-frank.content-holder:content-holder-modifyable
          cl-gtk-frank.content-holder:content-holder-selectable-modifyable
          cl-gtk-frank.content-holder:viewers
          cl-gtk-frank.content-holder:replace-content
          cl-gtk-frank.content-holder:content
          cl-gtk-frank.content-holder:add-to-content
          cl-gtk-frank.content-holder:add-to-selection
          cl-gtk-frank.content-holder:remove-from-content
          cl-gtk-frank.viewer-update-requests:draw-viewer
          cl-gtk-frank.viewer-update-requests:update-all-viewers
          cl-gtk-frank.tree-viewer:list-layer
          cl-gtk-frank.tree-viewer:tree-viewer-selection-mirrored
          cl-gtk-frank.tree-viewer:tree-viewer-excl
          cl-gtk-frank.tree-viewer:make-tree-viewer
          cl-gtk-frank.tree-viewer:changed-enriched
          cl-gtk-frank.tree-store-basic:gtk-view
          cl-gtk-frank.tree-store-basic:gtk-selection-widget
          cl-gtk-frank.tree-store-basic:tree-row
          cl-gtk-frank.tree-store-basic:store-content-layer
          cl-gtk-frank.tree-store-basic:scl-with-flex
          cl-gtk-frank.tree-store-basic:scl-with-struct
          cl-gtk-frank.tree-store-basic:all-store-contents
          cl-gtk-frank.tree-store-basic:get-tree-children
          cl-gtk-frank.tree-store-basic:repopulate-tree-store
          cl-gtk-frank.tree-store-basic:verify-selection
          cl-gtk-frank.tree-store-basic:query-tree-row
          cl-gtk-frank.tree-store-basic:query-view-column-index
          cl-gtk-frank.tree-layer:tree-layer
          cl-gtk-frank.tree-layer:root-content
          cl-gtk-frank.tree-layer:direct-replace-row
          cl-gtk-frank.tree-layer:direct-remove-row
          cl-gtk-frank.tree-layer:direct-insert-row)
        (find-package :cl-gtk-frank))
