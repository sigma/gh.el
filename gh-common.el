;;; gh-common.el --- common objects for gh.el

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

(defclass gh-user ()
  ((login :initarg :login)
   (id :initarg :id)
   (gravatar-url :initarg :gravatar-url)
   (url :initarg :url))
  "Github user object")

(defun gh-user-read (user &optional into)
  (let ((target (or into (gh-user "user"))))
    (with-slots (login id gravatar-url url)
        target
      (setq login (gh-read user 'login)
            id (gh-read user 'id)
            gravatar-url (gh-read user 'gravatar_url)
            url (gh-read user 'url)))
    target))

(defun gh-user-read-list (users)
  (mapcar 'gh-user-read users))

(defun gh-read (obj field)
  (cdr (assoc field obj)))

(defun gh-config (key)
  "Returns a GitHub specific value from the global Git config."
  (let ((strip (lambda (string)
                 (if (> (length string) 0)
                     (substring string 0 (- (length string) 1)))))
        (git (executable-find "git")))
  (funcall strip (shell-command-to-string
                  (concat git " config --global github." key)))))

(defun gh-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (let ((git (executable-find "git")))
    (shell-command-to-string
     (concat git " config --global github." key " " value))))

(provide 'gh-common)
;;; gh-common.el ends here
