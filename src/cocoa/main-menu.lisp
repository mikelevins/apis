;;;; ***********************************************************************
;;;;
;;;; Name:          main-menu.lisp
;;;; Project:       Apis: the hive application
;;;; Purpose:       set up the main menu
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :apis)

;;; ---------------------------------------------------------------------
;;; application menu
;;; ---------------------------------------------------------------------

(defun populate-application-menu (main-menu)
  (let* ((main-item (add-menu-item main-menu #@"Apple"))
         (menu (make-menu #@"Apple")))
    (perform-selector (ccl::nsapp)
                      (objc:@selector "setAppleMenu:")
                      :with-object menu)

    (add-menu-item menu #@"About Apis" :action (objc:@selector "orderFrontStandardAboutPanel:") :target (ccl::nsapp))
    (add-menu-separator menu)

    (add-menu-item menu #@"Preferences..." :key-equivalent #@"," :target (ccl::nsapp))
    (let ((services-menu (make-menu #@"Services"))
          (services-item (add-menu-item menu #@"Services" :target (ccl::nsapp))))
      (#/setSubmenu:forItem: menu services-menu services-item)
      (#/setServicesMenu: (ccl::nsapp) services-menu))
    (add-menu-separator menu)

    (add-menu-item menu #@"Hide Apis" :action (objc:@selector "hide:") :target (ccl::nsapp) :key-equivalent #@"h")
    (add-menu-item menu #@"Hide Others" :action (objc:@selector "hideOtherApplications:") :target (ccl::nsapp)
                   :key-equivalent #@"h" :key-equivalent-modifier-mask (logior #$NSCommandKeyMask #$NSAlternateKeyMask))
    (add-menu-item menu #@"Show All" :action (objc:@selector "unhideAllApplications:") :target (ccl::nsapp))
    (add-menu-separator menu)

    (add-menu-item menu #@"Quit" :action (objc:@selector "terminate:") :target (ccl::nsapp) :key-equivalent #@"q")
    
    (#/setSubmenu:forItem: main-menu menu main-item)))

;;; ---------------------------------------------------------------------
;;; file menu
;;; ---------------------------------------------------------------------

(defun populate-file-menu (main-menu)
  (let* ((main-item (add-menu-item main-menu #@"File"))
         (menu (make-menu #@"File")))
    
    (add-menu-item menu #@"New" :action (objc:@selector "newDocument:") :key-equivalent #@"n")
    (add-menu-item menu #@"Open..." :action (objc:@selector "openDocument:") :key-equivalent #@"o")
    (let ((recent-menu (make-menu #@"Open Recent"))
          (recent-item (add-menu-item menu #@"Open Recent")))
      (perform-selector recent-menu
                        (objc:@selector "_setMenuName:")
                        :with-object #@"NSRecentDocumentsMenu")
      (#/setSubmenu:forItem: menu recent-menu recent-item))
    (add-menu-separator menu)

    (add-menu-item menu #@"Close" :action (objc:@selector "performClose:") :key-equivalent #@"w")
    (add-menu-item menu #@"Save" :action (objc:@selector "saveDocument:") :key-equivalent #@"s")
    (add-menu-item menu #@"Save As..." :key-equivalent #@"S")
    (add-menu-item menu #@"Revert" :action (objc:@selector "revertDocumentToSaved:"))
    (add-menu-separator menu)

    (add-menu-item menu #@"Page Setup..." :action (objc:@selector "runPageLayout:") :target (ccl::nsapp) :key-equivalent #@"P")
    (add-menu-item menu #@"Print..." :action (objc:@selector "printDocument:") :target (ccl::nsapp) :key-equivalent #@"p")

    (#/setSubmenu:forItem: main-menu menu main-item)))

;;; ---------------------------------------------------------------------
;;; edit menu
;;; ---------------------------------------------------------------------

(defun populate-edit-menu (main-menu)
  (let* ((main-item (add-menu-item main-menu #@"Edit"))
         (menu (make-menu #@"Edit")))

    (add-menu-item menu #@"Undo" :action (objc:@selector "undo:") :key-equivalent #@"z")
    (add-menu-item menu #@"Redo" :action (objc:@selector "redo:") :key-equivalent #@"Z")
    (add-menu-separator menu)

    (add-menu-item menu #@"Cut" :action (objc:@selector "cut:") :key-equivalent #@"x")
    (add-menu-item menu #@"Copy" :action (objc:@selector "copy:") :key-equivalent #@"c")
    (add-menu-item menu #@"Paste" :action (objc:@selector "paste:") :key-equivalent #@"v")
    (add-menu-item menu #@"Paste and Match Style" :action (objc:@selector "pasteAsPlainText:") :key-equivalent #@"V")
    (add-menu-item menu #@"Delete" :action (objc:@selector "delete:"))
    (add-menu-item menu #@"Select All" :action (objc:@selector "selectAll:") :key-equivalent #@"a")
    (add-menu-separator menu)

    (let ((find-menu (make-menu #@"Find"))
          (find-item (add-menu-item menu #@"Find")))
      (add-menu-item find-menu #@"Find..." :action (objc:@selector "performFindPanelAction:") 
                     :key-equivalent #@"f" :tag #$NSFindPanelActionShowFindPanel)
      (add-menu-item find-menu #@"Find Next" :action (objc:@selector "performFindPanelAction:") 
                     :key-equivalent #@"g" :tag #$NSFindPanelActionNext)
      (add-menu-item find-menu #@"Find Previous" :action (objc:@selector "performFindPanelAction:") 
                     :key-equivalent #@"G" :tag #$NSFindPanelActionPrevious)
      (add-menu-item find-menu #@"Use Selection for Find" :action (objc:@selector "performFindPanelAction:") 
                     :key-equivalent #@"e" :tag #$NSFindPanelActionSetFindString)
      (add-menu-item find-menu #@"Jump To Selection" :action (objc:@selector "centerSelectionInVisibleArea:") :key-equivalent #@"j")
      (#/setSubmenu:forItem: menu find-menu find-item))

    (let ((spelling-menu (make-menu #@"Spelling"))
          (spelling-item (add-menu-item menu #@"Spelling")))
      (add-menu-item spelling-menu #@"Spelling..." :action (objc:@selector "showGuessPanel:") :target (ccl::nsapp) :key-equivalent #@":")
      (add-menu-item spelling-menu #@"Check Spelling" :action (objc:@selector "checkSpelling:") :target (ccl::nsapp) :key-equivalent #@";")
      (add-menu-item spelling-menu #@"Check Spelling as You Type" :action (objc:@selector "toggleContinuousSpellChecking:") :target (ccl::nsapp))
      (#/setSubmenu:forItem: menu spelling-menu spelling-item))

    (#/setSubmenu:forItem: main-menu menu main-item)))

;;; ---------------------------------------------------------------------
;;; window menu
;;; ---------------------------------------------------------------------

(defun populate-window-menu (main-menu)
  (let* ((main-item (add-menu-item main-menu #@"Window"))
         (menu (make-menu #@"Window")))

    (add-menu-item menu #@"Minimize" :action (objc:@selector "performMinimize:") :target (ccl::nsapp) :key-equivalent #@"m")
    (add-menu-item menu #@"Zoom" :action (objc:@selector "performZoom:") :target (ccl::nsapp))
    (add-menu-separator menu)

    (add-menu-item menu #@"Bring All to Front" :action (objc:@selector "arrangeInFront:") :target (ccl::nsapp))

    (#/setSubmenu:forItem: main-menu menu main-item)))

;;; ---------------------------------------------------------------------
;;; help menu
;;; ---------------------------------------------------------------------

(defun populate-help-menu (main-menu)
  (let* ((main-item (add-menu-item main-menu #@"Help"))
         (menu (make-menu #@"Help")))

    (add-menu-item menu #@"Apis Help" :action (objc:@selector "showHelp:") :target (ccl::nsapp) :key-equivalent #@"?")

    (#/setSubmenu:forItem: main-menu menu main-item)))

;;; ---------------------------------------------------------------------
;;; all menus
;;; ---------------------------------------------------------------------

(defun setup-menus ()
  (let ((main-menu (#/autorelease (#/initWithTitle: (#/alloc (objc:@class ns:ns-menu)) #@"MainMenu"))))
    (populate-application-menu main-menu)
    (populate-file-menu main-menu)
    (populate-edit-menu main-menu)
    (populate-window-menu main-menu)
    (populate-help-menu main-menu)
    (#/setMainMenu: (ccl::nsapp) main-menu)))


