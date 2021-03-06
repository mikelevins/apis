;;;; ***********************************************************************
;;;;
;;;; Name:          diffie-hellman.lisp
;;;; Project:       the apis message-passing system
;;;; Purpose:       an implementation of diffie-hellman secure key exchange
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)


(defpackage #:diffie-hellman
  (:nicknames :dhx)
  (:use #:cl))

(in-package :dhx)

(defparameter +modp-group-2-prime-modulus+
  #xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF)

(defparameter +modp-group-2-generator+ 2)

(defmethod encode-secret ((n integer))
  (rem (expt +modp-group-2-generator+ n) +modp-group-2-prime-modulus+))

(defmethod combine-secrets (received-encoding local-secret)
  (rem (expt received-encoding local-secret) +modp-group-2-prime-modulus+))

;;; testing:
;;; g = +modp-group-2-generator+
;;; p = +modp-group-2-prime-modulus+
;;; Alice a = 6
;;; Bob b = 15
;;; Alice A = g^a mod p
;;; (encode-secret 6)
;;; Bob B = g^b mod p
;;; (encode-secret 15)
;;; Alice s1 = B^a mod p
;;; (combine-secrets (encode-secret 15) 6)
;;; Bob s2 = A^b mod p
;;; (combine-secrets (encode-secret 6) 15)
;;; s1 == s2?
;;; (time (= (combine-secrets (encode-secret 15) 6) (combine-secrets (encode-secret 6) 15)))

