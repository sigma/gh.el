;;; pcache.el --- persistent caching for Emacs

;; Copyright (C) 2011  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:
;; Version: 0.1
;; Package-Requires: ((eieio "1.4"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (progn
    (when (< emacs-major-version 24)
      ;; byte-compilation of eieio code is broken in emacs 23.x
      (put 'defmethod 'byte-hunk-handler nil))
    (require 'cl)))

(require 'eieio)
(require 'eieio-base)

(defvar pcache-directory
  (let ((dir (concat user-emacs-directory "var/pcache/")))
    (make-directory dir t)
    dir))

(defvar *pcache-repositories* (make-hash-table :test 'equal))

(defconst pcache-default-save-delay 300)

(defclass pcache-repository (eieio-persistent)
  ((version :initarg :version :initform 0.1)
   (entries :initarg :entries :initform (make-hash-table))
   (entry-cls :initarg :entry-cls :initform pcache-entry)
   (timestamp :initarg :timestamp :initform (float-time (current-time)))
   (save-delay :initarg :save-delay)))

(oset-default 'pcache-repository :save-delay pcache-default-save-delay)

(defmethod constructor :static ((cache pcache-repository) newname &rest args)
  (let ((e (gethash newname *pcache-repositories*))
        (path (concat pcache-directory newname)))
    (or e
        (and (not (boundp 'pcache-avoid-recursion))
             (file-exists-p path)
             (let* ((pcache-avoid-recursion t)
                    (obj (eieio-persistent-read path)))
               (puthash newname obj *pcache-repositories*)
               obj))
        (let ((obj (call-next-method)))
          (oset obj :file path)
          (puthash newname obj *pcache-repositories*)
          obj))))

(defclass pcache-entry ()
  ((timestamp :initarg :timestamp
              :initform (float-time (current-time)))
   (ttl :initarg :ttl)
   (value :initarg :value :initform nil)))

(defmethod pcache-entry-valid-p ((entry pcache-entry))
  (let ((ttl (oref entry :ttl)))
    (or (null ttl))
    (let ((time (float-time (current-time))))
      (< time (+ ttl (oref entry :timestamp))))))

(defmethod pcache-get ((cache pcache-repository) key &optional default)
  (let* ((time (float-time (current-time)))
         (table (oref cache :entries))
         (entry (gethash key table)))
    (if entry
        (if (pcache-entry-valid-p entry)
            (oref entry :value)
          (remhash key table)
          default)
      default)))

(defmethod pcache-put ((cache pcache-repository) key value &optional ttl)
  (let ((table (oref cache :entries))
        (entry (or (and (eieio-object-p value)
                        (object-of-class-p value 'pcache-entry)
                        value)
                   (make-instance (oref cache :entry-cls) :value value))))
    (when ttl
      (oset entry :ttl ttl))
    (prog1
        (puthash key entry table)
      (pcache-save cache))))

(defmethod pcache-invalidate ((cache pcache-repository) key)
  (let ((table (oref cache :entries)))
    (remhash key table)))

(defmethod pcache-clear ((cache pcache-repository))
  (oset cache :entries (make-hash-table))
  (pcache-save cache))

(defmethod pcache-purge-invalid ((cache pcache-repository))
  (let ((table (oref cache :entries)))
    (maphash #'(lambda (k e)
                 (unless (pcache-entry-valid-p e)
                   (remhash k table)))
             table)
    (pcache-save cache)))

(defmethod pcache-save ((cache pcache-repository) &optional force)
  (let ((timestamp (oref cache :timestamp))
        (delay (oref cache :save-delay))
        (time (float-time (current-time))))
    (when (or force (> time (+ timestamp delay)))
      (oset cache :timestamp time)
      (eieio-persistent-save cache))))

(defun pcache-kill-emacs-hook ()
  (maphash #'(lambda (k v)
               (condition-case nil
                   (pcache-save v t)
                 (error nil)))
           *pcache-repositories*))

(add-hook 'kill-emacs-hook 'pcache-kill-emacs-hook)

(provide 'pcache)
;;; pcache.el ends here
