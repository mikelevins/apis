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

;;; ---------------------------------------------------------------------
;;; application menu
;;; ---------------------------------------------------------------------

(defun populate-application-menu (main-menu)
  (let* ((main-item (add-menu-item main-menu #@"Apple"))
         (menu (make-menu #@"Apple")))
    (perform-selector (ccl::nsapp)
                      (objc:@selector "setAppleMenu:")
                      :with-object menu)

    (add-menu-item menu #@"About Apis"
                   :action (objc:@selector "orderFrontStandardAboutPanel:")
                   :target (ccl::nsapp))
    (add-menu-separator menu)

    (add-menu-item menu #@"Preferences..."
                   :key-equivalent #@","
                   :target (ccl::nsapp))

    (let ((services-menu (make-menu #@"Services"))
          (services-item (add-menu-item menu #@"Services"
                                        :target (ccl::nsapp))))
      (#/setSubmenu:forItem: menu services-menu services-item)
      (#/setServicesMenu: (ccl::nsapp) services-menu))
    (add-menu-separator menu)

    (add-menu-item menu #@"Hide Apis"
                   :action (objc:@selector "hide:")
                   :target (ccl::nsapp)
                   :key-equivalent #@"h")
    (add-menu-item menu #@"Hide Others"
                   :action (objc:@selector "hideOtherApplications:")
                   :target (ccl::nsapp)
                   :key-equivalent #@"h"
                   :key-equivalent-modifier-mask (logior #$NSCommandKeyMask #$NSAlternateKeyMask))
    (add-menu-item menu #@"Show All"
                   :action (objc:@selector "unhideAllApplications:")
                   :target (ccl::nsapp))
    (add-menu-separator menu)

    (add-menu-item menu #@"Quit"
                   :action (objc:@selector "terminate:")
                   :target (ccl::nsapp)
                   :key-equivalent #@"q")
    
    (#/setSubmenu:forItem: main-menu menu main-item)))

;;; ---------------------------------------------------------------------
;;; all menus
;;; ---------------------------------------------------------------------

(defun setup-menus ()
  (let ((main-menu (#/autorelease (#/initWithTitle: (#/alloc (objc:@class ns:ns-menu)) #@"MainMenu"))))
    (populate-application-menu main-menu)
    (#/setMainMenu: (ccl::nsapp) main-menu)))
