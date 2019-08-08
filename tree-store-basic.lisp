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

(in-package :cl-gtk-frank.tree-store-basic)

(defclass tree-row ()
  ((iter :initform nil :accessor iter)));en lista över (cons tree-store gtk-tree-iter). De uppdateras vid ny populate, men tas inte bort när raden lämnar vyn. Utgår från att tree-row i scl faktiskt finns i gtk tree view när en begäran kommer om att ta bort/ändra, och att det inte finns något i content-holder som inte finns i vyn, förutom vid (re)populate.

(defun iter-equal (tree-store gtk-tree-iter-1 tree-row-iter-2)
  (let ((gtk-tree-iter-2 (cdr (find tree-store tree-row-iter-2 :key #'car))))
    ;;presumes they are already known to come from the same gtk model
    (eq (gtk-tree-iter-user-data gtk-tree-iter-1)
        (gtk-tree-iter-user-data gtk-tree-iter-2))))

(defclass store-content-layer ()
  ((column-readers :initarg :column-readers :reader column-readers)
   (headers :initarg :headers :reader headers)))
;;subclassed layers should contain items subclassed to tree-row
;;for each column the store is displaying there should be a column-reader-function, which takes the tree-row as input a returns a string to be displayed in that column

;;Notera att ett store-content-layer principiellt kan tillhöra flera viewers, man med nuvarande upplägg så sätt iter i populate vilket knyter innehållet till en specifik store. Det skulle kunna åtgärdas genom en fristående tilldelning av iters, som populate sedan bara använder.

;;a subclass to store-content-layer implements a method of this to return all tree-rows.
;;if it also subclasses content-holder, this would be a call to #'contents.
;;(content-holder låg tidigare i cl-gtk-cv)
(defgeneric all-store-contents (store-content-layer)
  )

;;In our usage of gtk, we conceptually compound tree-store and tree-view into a single presentation entity
(defclass tree-store (widget)
  ((store-content-layer :initarg :store-content-layer :accessor store-content-layer :type 'store-content-layer)
   (gtk-store :initarg :gtk-store :reader gtk-store)
   (gtk-view :accessor gtk-view)
   (gtk-view-columns :initform nil :accessor gtk-view-columns)
   (gtk-selection-widget :accessor gtk-selection-widget)
   (changed-handler-id :accessor changed-handler-id)))

(defmethod attach-text-view ((tree-store tree-store) no-of-cols headers)
  (let (view-columns
        (view (make-instance 'gtk-tree-view :model (gtk-store tree-store))))
    (dotimes (i no-of-cols)
      (let ((renderer (gtk-cell-renderer-text-new))
            (column (gtk-tree-view-column-new)))
        (push column view-columns)
        (gtk-tree-view-column-set-title column (elt headers i))
        (gtk-tree-view-column-set-resizable column t)
        (gtk-tree-view-column-pack-start column
                                         renderer)
        (gtk-tree-view-column-add-attribute column
                                            renderer
                                            "text"
                                            i)
        (gtk-tree-view-append-column view column)
        (gtk-tree-view-column-set-sort-column-id column i)))
    (setf (gtk-view-columns tree-store)
          (nreverse view-columns))
    (setf (gtk-view tree-store)
          view)
    (setf (gtk-selection-widget tree-store)
          (gtk-tree-view-get-selection (gtk-view tree-store)))))

(defgeneric attach-text-view-flex (store column-attribute-source header)
  )

(defmethod attach-text-view-flex ((tree-store tree-store) column-attribute-source headers)
  ;;ett element i column-attribute-source (som är en lista) per vy-kolumn. Varje sådant element är en lista över cons, där car är kolumn-id (i modellen) och en sträng som visar vilket attribut i vy-kolumnen som sätts från det värdet i modell-kolumnen. Tänk på att datatypen måste stämma med attributet som sätts. Modell-kolumner numreras från 0
  (let (view-columns
        (view (gtk-tree-view-new-with-model (gtk-store tree-store))))
    (dotimes (i (length column-attribute-source))
      (let ((renderer (gtk-cell-renderer-text-new))
            (column (gtk-tree-view-column-new)))
        (push column view-columns)
        (gtk-tree-view-column-set-title column (elt headers i))
        (gtk-tree-view-column-set-resizable column t)
        (gtk-tree-view-column-pack-start column
                                         renderer)
        (dolist (attribute-source (elt column-attribute-source i))
          (gtk-tree-view-column-add-attribute column
                                              renderer
                                              (cdr attribute-source)
                                              (car attribute-source)))
        (gtk-tree-view-append-column view column)
        (gtk-tree-view-column-set-sort-column-id column i)))
    (setf (gtk-view-columns tree-store)
          (nreverse view-columns))
    (setf (gtk-view tree-store)
          view)
    (setf (gtk-selection-widget tree-store)
          (gtk-tree-view-get-selection (gtk-view tree-store)))))

(defmethod connect-tree-view-selection-changed ((tree-store tree-store) instance-symbol static-symbol)
  (register-widget tree-store instance-symbol static-symbol)
  (let ((handler-id (g-signal-connect (gtk-selection-widget tree-store)
                                      "changed"
                                      (make-callback 'changed
                                                     (list (intern (symbol-name instance-symbol)
                                                                   (find-package :cl-gtk-frank))
                                                           (intern (symbol-name static-symbol)
                                                                   (find-package :cl-gtk-frank)))))))
    (setf (changed-handler-id tree-store) handler-id)))

;;ev. blockera signal
(defmethod populate-tree-store-simple ((tree-store tree-store))
  (when (store-content-layer tree-store)
    (let ((column-readers (column-readers (store-content-layer tree-store))))
      (dolist (tree-row (all-store-contents (store-content-layer tree-store)))
        (let ((iter (gtk-tree-store-append (gtk-store tree-store) nil)))
          (apply #'gtk-tree-store-set
                 (gtk-store tree-store)
                 iter
                 (mapcar (lambda (col-reader)
                           (funcall col-reader tree-row))
                         column-readers))
          (setf (iter tree-row)
                (cons
                 (cons tree-store iter)
                 (remove tree-store (iter tree-row) :key #'car))))))))

;;se cl-rss-sql-client för exempel på användning av dessa
(defclass scl-with-flex (store-content-layer)
  ((column-attribute-source :initarg :column-attribute-source :reader column-attribute-source)))

(defclass scl-with-struct (store-content-layer)
  ());varje tree-row skall ha en get-children-metod, all-store-contents skall ge rot-nivå-innehållet
(defgeneric get-tree-children (tree-row)
  )

(defmethod populate-tree-store-medium ((tree-store tree-store))
  (let ((contents (all-store-contents (store-content-layer tree-store)))
        (column-readers (column-readers (store-content-layer tree-store))))
    (labels ((proc-tree-row (tree-row parent-iter)
               (let ((child-rows (get-tree-children tree-row))
                     (col-values (mapcar (lambda (col-reader)
                                           (funcall col-reader tree-row))
                                         column-readers))
                     (iter (gtk-tree-store-append (gtk-store tree-store)
                                                  parent-iter)))
                 (setf (iter tree-row)
                       (cons
                        (cons tree-store iter)
                        (remove tree-store (iter tree-row) :key #'car)))
                 (if child-rows
                     (progn
                       (apply #'gtk-tree-store-set
                              (gtk-store tree-store)
                              iter
                              col-values)
                       (dolist (child-row child-rows)
                         (proc-tree-row child-row iter)))
                     (apply #'gtk-tree-store-set
                            (gtk-store tree-store)
                            iter
                            col-values)))))
      (dolist (content contents)
        (proc-tree-row content nil)))))

(defmethod make-tree-store ((scl store-content-layer))
  (let* ((no-of-view-cols (length (headers scl)))
         (no-of-model-cols (length (column-readers scl)))
         (col-types (loop for i from 1 to no-of-model-cols collecting "gchararray"))
         (gtk-store (make-instance 'gtk-tree-store
                                   :column-types col-types))
         (tree-store (make-instance 'tree-store
                                    :gtk-store gtk-store
                                    :store-content-layer scl)))
    (if (typep scl 'scl-with-flex)
        (attach-text-view-flex tree-store (column-attribute-source scl) (headers scl))
        (attach-text-view tree-store no-of-view-cols (headers scl)))
    (if (typep scl 'scl-with-struct)
        (populate-tree-store-medium tree-store)
        (populate-tree-store-simple tree-store))
    tree-store))

(defmethod get-store-contents ((scl store-content-layer))
  (all-store-contents scl))

(defmethod get-store-contents ((scl scl-with-struct))
  (labels ((collect-children (tree-row)
             (cons tree-row
                   (reduce #'append
                           (mapcar #'collect-children
                                   (get-tree-children tree-row))))))
    (reduce #'append (mapcar #'collect-children (all-store-contents scl)))))

;;use in a 'changed'-event. Returns the current selection, by querying rowids from the client
(defmethod get-gtk-selection ((tree-store tree-store))
  (let ((selection-iters (mapcar (lambda (path)
                                    (gtk-tree-model-get-iter (gtk-store tree-store) path))
                                 (gtk-tree-selection-get-selected-rows (gtk-selection-widget tree-store)))))
    (intersection (get-store-contents (store-content-layer tree-store))
                  selection-iters
                  :test (lambda (tree-row iter)
                          (iter-equal tree-store iter (iter tree-row))))))

;;'re-render' selection (to update the view of a non-modifyable content-holder e g)
(defmethod present-selection ((tree-store tree-store) selection)
  (let ((selection (mapcar #'iter
                           (intersection selection
                                         (get-store-contents (store-content-layer tree-store))))))
    ;;eftersom följande kommer att generera många signaler - kör inom with-store-signal-blocked
    (gtk-tree-selection-unselect-all (gtk-selection-widget tree-store))
    ;;för varje tree-row i selection, extrahera den gtk-tree-iter i (iter tree-row) som motsvarar denna tree-store, och sätt select
    (dolist (tree-row selection)
      (let ((gtk-tree-iter (cdr (find tree-store (iter tree-row) :key #'car))))
        (gtk-tree-selection-select-iter (gtk-selection-widget tree-store) gtk-tree-iter)))))

(defmacro with-store-signal-blocked (tree-store &body body)
  `(progn
     (g-signal-handler-block (gtk-selection-widget ,tree-store)
                             (changed-handler-id ,tree-store))
     ,@body
     (g-signal-handler-unblock (gtk-selection-widget ,tree-store)
                               (changed-handler-id ,tree-store))))

(defgeneric verify-selection (tree-store)
  ;;metoder i list-viewer och tree-layer
  )

(defmethod repopulate-tree-store ((tree-store tree-store))
  (with-store-signal-blocked tree-store
    (gtk-tree-store-clear (gtk-store tree-store))
    (if (typep (store-content-layer tree-store) 'scl-with-struct)
        (populate-tree-store-medium tree-store)
        (populate-tree-store-simple tree-store))))
;;update-viewer-metod anropar denna, följt av present-selection på lämplig mängd (inte nödvändigtvis sin selection - gtk's idé om selection kanske är något som viewern bara använder som del av sin verktygsfunktion)

(defmethod update-row ((ts tree-store) (old-row tree-row) (new-row tree-row))
  (flet ((map-column-readers (tree-row)
           (mapcar (lambda (column-reader)
                     (funcall column-reader tree-row))
                   (column-readers (store-content-layer ts)))))
    (let ((old-column-strings (map-column-readers old-row))
          (new-column-strings (map-column-readers new-row)))
      (let ((equal-column-string-ps (mapcar #'equal
                                            old-column-strings
                                            new-column-strings)))
        (when (some #'null equal-column-string-ps)
          (with-store-signal-blocked ts
            (apply
             #'gtk-tree-store-set
             (gtk-store ts)
             (cdr (find ts (iter old-row) :key #'car))
             new-column-strings)))))))

(defmethod remove-row ((ts tree-store) (target tree-row))
  (with-store-signal-blocked ts
    (gtk-tree-store-remove (gtk-store ts)
                           (cdr (find ts (iter target) :key #'car)))))

(defmethod insert-row ((ts tree-store) parent (new tree-row))
  (declare ((or null tree-row) parent))
  (let ((col-values (mapcar (lambda (column-reader)
                              (funcall column-reader new))
                            (column-readers (store-content-layer ts)))))
    (with-store-signal-blocked ts
      (let ((gtk-tree-iter (gtk-tree-store-append (gtk-store ts)
                                                  (when parent
                                                    (cdr (find ts (iter parent) :key #'car))))))
        (setf (iter new)
              (cons
               (cons ts gtk-tree-iter)
               (remove ts (iter new) :key #'car)))
        (apply
         #'gtk-tree-store-set
         (gtk-store ts)
         gtk-tree-iter
         col-values)))))

(defun query-tree-row (tree-store gtk-tree-path)
  (let ((gtk-tree-iter (gtk-tree-model-get-iter tree-store gtk-tree-path)))
    (flet ((tree-row-match-p (tree-row)
             (iter-equal tree-store gtk-tree-iter (iter tree-row))))
      (find-if #'tree-row-match-p
               (get-store-contents (store-content-layer tree-store))))))

(defun query-view-column-index (tree-store gtk-view-column)
  (position gtk-view-column (gtk-view-columns tree-store)))

