# cl-gtk-frank

An abstraction layer for Gtk in LISP.

Depends on cl-cffi-gtk.

## Overview

This package is used by cl-rss-sql. Please have a look in that code for examples of usage. The description below skips many details, and is only intended as a primer to understanding the code.

A redirection is built between Gtk widgets and CLOS objects. The effect of this, is that when a signal is triggered in a Gtk widget, a function call is issued with the corresponding object as the first argument. The function called corresponds, by name, to the signal. So, if you register an object called 'button' to a Gtk button widget, clicking the button in the Gui is like executing "(clicked button)" at the REPL. Additional arguments correspond to the signal definition in Gtk, after removing the generic 'user_data' pointer.

The justification for this redirection, is to avoid having the full Gtk widget interface present by default in the event handler.

The CLOS class 'widget' is intended to be the base class of every object which corresponds to a Gtk widget. So, 'button' could be of a subclass of 'widget' with a specialised method for 'clicked', e g. 'widget' one method already; for 'destroy', which is to remove the object from the registry and to funcall the destroy-hook-slot of the object, if there is one.

## Glade integration

There is macro, 'with-builder', for using Glade files conveniently. In Glade, you can build the static part of the user interface. Gtk already provides a function for reading those files, and generating widgets from it. 'with-builder' is a wrapper for that, and also provides functions to connect signals to widgets and to register the corresponding CLOS objects, while storing the handler ids of the connected signals so that they can be blocked and unblocked.

Registering a widget uses the instance id set in Glade, so each widget instance that you interact with in LISP needs to be given a unique id in Glade. This instance id can also be used to retrieve the (pointer to the) widget within with-builder, so that it can be stored somewhere and used later in calls to Gtk. This isn't always needed. To receive clicks from a button, it is enough to register the widget. But to, say, dynamically make it unclickable, the widget itself must be retrieved and stored so that it can be used in a call to gtk-widget-set-sensitive.

If a user interface needs to be modified dynamically, this might involve having a variable number of instances of a static subset of widgets. This static subset can then also be built in Glade. 'with-builder' takes an identifying symbol as one of its arguments. This is used together with the instance id from the glade file to uniquely identify widgets with regards to registration.

In order to define in the glade file that a signal is to be connected by with-builder, put something in the 'Handler' column of the specific widget, on the row of the specific signal. It shouldn't matter what text you put - I always just put an 'x'.

## Tree-viewer

A framework is provided for using Gtk tree views, which would be inconvenient to do in Glade. The intended usage is to create everything up to the viewport/scrolled-window in Glade, and then gtk-container-add the (gtk-view ) of the tree-viewer returned by make-tree-viewer.

Two kinds of lists are available. The first is a flat list, without any hierarchy among rows. When the contents are changed, it can be graphically updated by calling update-all-viewers. The second kind provides hierarchy, but updating must be done manually.

Every row has to be a CLOS object which subclasses tree-row. These are placed in either a list-layer or a tree-layer, for the two kinds of lists respectively. The list/tree-layer is then provided to make-tree-viewer, which will return a tree-store object. As mentioned, (gtk-view tree-store) needs to be gtk-container-add:ed into a viewport or scrolled-window. It is also the object that receives signals. The idea is to create a utility subclass of tree-viewer-excl (which is a subclass of tree-store), change-class the return value of make-tree-viewer to that class, and to specialise methods for changed-enriched and/or row-activated. changed-enriched is like changed, but the currently selected rows are given as the second argument. If :single-p of tree-viewer-excl is t, changed-enriched is only called if there is precisely one selected row, which is then given as the second argument. Otherwise the second argument is a list of the selected rows, possibly empty.