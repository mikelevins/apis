;;;; ***********************************************************************
;;;;
;;;; Name:          menus.lisp
;;;; Project:       Apis: the hive application
;;;; Purpose:       menu-building utilities
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :apis)

;;; ---------------------------------------------------------------------
;;; menu utils
;;; ---------------------------------------------------------------------

(defun make-menu (menu-title)
  (#/autorelease (#/initWithTitle: (#/alloc (objc:@class ns:ns-menu)) menu-title)))

(defun add-menu-separator (menu)
  (#/addItem: menu (#/separatorItem ns:ns-menu-item)))

(defun add-menu-item (menu item-title 
                      &key
                      (action (%null-ptr))
                      (target (%null-ptr)) ; by default target the First Responder
                      (key-equivalent #@"")
                      (key-equivalent-modifier-mask nil)
                      (tag nil))
  (let ((item (#/addItemWithTitle:action:keyEquivalent: menu item-title action key-equivalent)))
    (when key-equivalent-modifier-mask
      (#/setKeyEquivalentModifierMask: item key-equivalent-modifier-mask))
    (when tag
      (#/setTag: item tag))
    (#/setTarget: item target)
    item))

