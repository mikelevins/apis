;;;; ***********************************************************************
;;;;
;;;; Name:          model-entity.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       simple entities represented as plists
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:apis)

(defmethod entity? (x) nil)

(defmethod entity? ((x cons))
  (list (symbolp (car x))
        (listp (cdr x))))

(deftype entity ()
  `(and list (satisfies entity?)))

(defmethod entity ((type symbol) &rest properties)
  (cons type properties))

(defmethod entity-type ((e list))
  (assert (typep e 'entity)() "~S is not an entity" e)
  (car e))

(defmethod entity-properties ((e list))
  (assert (typep e 'entity)() "~S is not an entity" e)
  (cdr e))

(defmethod entity-keys ((e list))
  (assert (typep e 'entity)() "~S is not an entity" e)
  (loop for tail on (entity-properties e) by #'cddr
       collect (car tail)))

(defmethod entity-values ((e list))
  (assert (typep e 'entity)() "~S is not an entity" e)
  (loop for tail on (entity-properties e) by #'cddr
       collect (cadr tail)))

(defmethod entity-get-key ((e list)(key symbol) &key (default nil))
  (assert (typep e 'entity)() "~S is not an entity" e)
  (getf (entity-properties e) key default))

(defmethod entity-put-key ((e list)(key symbol) val)
  (assert (typep e 'entity)() "~S is not an entity" e)
  (let* ((type (entity-type e))
         (properties (entity-properties e))
         (key-pos (position key properties)))
    (if key-pos
        (let ((hd (subseq properties 0 key-pos))
              (tl (subseq properties (+ 2 key-pos))))
          (cons type
                (append hd
                        (cons key
                              (cons val tl)))))
        (cons type
              (cons key
                    (cons val properties))))))

