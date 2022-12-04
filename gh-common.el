;;; gh-common.el --- common objects for gh.el -*- lexical-binding: t; no-byte-compile: t-*-

;; Copyright (C) 2011  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:

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

(require 'eieio)
(require 'marshal)

(autoload 'gh-profile-current-profile "gh-profile")
(autoload 'gh-profile-default-profile "gh-profile")

(defgroup gh nil
  "Github API client libraries."
  :group 'applications)

(defcustom gh-use-local-git-config nil
  "If `t' use git configuration from the machine running
Emacs. This makes a difference when running with TRAMP."
  :type 'boolean
  :group 'gh)

;;; Helper functions

(defun gh-read (obj field)
  (cdr (assoc field obj)))

(defun gh-namespaced-key (key)
  (let ((profile (gh-profile-current-profile)))
    (concat "github."
            (if (string= profile (gh-profile-default-profile))
                ""
              (concat profile "."))
            key)))

(defun gh-config (key)
  "Returns a GitHub specific value from the global Git config."
  (let ((strip (lambda (string)
                 (if (> (length string) 0)
                     (substring string 0 (- (length string) 1))))))
    (funcall strip (gh-command-to-string "config" (gh-namespaced-key key)))))

(defun gh-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (gh-command-to-string "config" "--global" (gh-namespaced-key key) value))

(defun gh-command-to-string (&rest args)
  (let ((git (executable-find "git"))
        (runner (if gh-use-local-git-config
                    'call-process
                  'process-file)))
    (with-output-to-string
      (apply runner git nil standard-output nil args))))

(defun gh-sanitize-content (content)
  "Remove all non-unicode characters. This can be used to
sanitize API calls that need to handle potentially dirty data."
  (let (res skipped)
    (mapc (lambda (c)
            (if (eq 'Cn (get-char-code-property c 'general-category))
                (setq skipped t)
              (push c res)))
          content)
    (when skipped
      (warn "Content contained non-unicode characters"))
    (apply #'string (nreverse res))))

;;; Base classes for common objects

(defun gh-marshal-default-spec (slot)
  (let ((slot-name (symbol-name slot)))
    (list (cons 'alist
                (intern (replace-regexp-in-string "-" "_" slot-name))))))

(defmacro gh-defclass (name superclass slots &rest options-and-doc)
  `(marshal-defclass ,name ,superclass ,slots ,@options-and-doc
                     :marshal-default-spec gh-marshal-default-spec))

(gh-defclass gh-object ()
  ())

(cl-defmethod gh-object-read ((cls (subclass gh-object)) data)
  (let ((target (make-instance cls)))
    (when data
      (gh-object-read-into target data))
    target))

(cl-defmethod gh-object-read ((obj gh-object) data)
  (when data
    (gh-object-read-into obj data)))

(cl-defmethod gh-object-reader ((obj gh-object))
  (apply-partially 'gh-object-read obj))

(cl-defmethod gh-object-reader ((cls (subclass gh-object)))
  (apply-partially 'gh-object-read cls))

(cl-defmethod gh-object-list-read ((obj gh-object) data)
  (mapcar (gh-object-reader obj) data))

(cl-defmethod gh-object-list-read ((cls (subclass gh-object)) data)
  (mapcar (gh-object-reader cls) data))

(cl-defmethod gh-object-list-reader ((obj gh-object))
  (apply-partially 'gh-object-list-read obj))

(cl-defmethod gh-object-list-reader ((cls (subclass gh-object)))
  (apply-partially 'gh-object-list-read cls))

(cl-defmethod gh-object-read-into ((obj gh-object) data)
  (unmarshal obj data 'alist))

(cl-defmethod slot-unbound ((obj gh-object) cls slot-name fn)
  (if (eq fn 'oref) nil
      (cl-call-next-method)))

(gh-defclass gh-ref-object (gh-object)
  ((id :initarg :id)
   (url :initarg :url)
   (html-url :initarg :html-url)))

(cl-defmethod gh-ref-object-base ((obj gh-ref-object))
  (let ((url (oref obj :url)))
    (concat "/"
            (mapconcat #'identity
                       (cddr (split-string url "/" t))
                       "/"))))

(cl-defmethod gh-ref-object-base (obj)
  (if (stringp obj) obj
    (error "illegal input for `gh-ref-object-base'")))

(gh-defclass gh-user (gh-ref-object)
  ((login :initarg :login)
   (gravatar-url :initarg :gravatar-url))
  "Github user object")

(gh-defclass gh-comment (gh-ref-object)
  ((body :initarg :body)
   (user :initarg :user :initform nil :marshal-type gh-user)
   (created-at :initarg :created_at)
   (updated-at :initarg :updated_at))
  "Github comment object")

(cl-defmethod gh-comment-req-to-update ((req gh-comment))
  `(("body" . ,(oref req :body))))

(provide 'gh-common)
;;; gh-common.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
