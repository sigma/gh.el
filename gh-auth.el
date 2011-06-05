;;; gh-auth.el --- authentication for gh.el

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
(require 'gh-common)

(defvar gh-auth-username nil)
(defvar gh-auth-password nil)

(defun gh-auth-get-username ()
  ;; hack to skip initialization at class def time
  (unless eieio-skip-typecheck
    (let ((user (or gh-auth-username (gh-config "user"))))
      (when (not user)
        (setq user (read-string "GitHub username: "))
        (setq gh-auth-username user)
        (gh-set-config "user" user))
      user)))

(defun gh-auth-get-password ()
  ;; hack to skip initialization at class def time
  (unless eieio-skip-typecheck
    (let ((pass (or gh-auth-password (gh-config "password"))))
      (when (not pass)
        (setq pass (read-passwd "GitHub password: "))
        (setq gh-auth-password pass)
        (gh-set-config "password" pass))
      pass)))

;;;###autoload
(defclass gh-authenticator ()
  ((username :initarg :username :initform (gh-auth-get-username)))
  "Abstract authenticator")

;;;###autoload
(defclass gh-password-authenticator (gh-authenticator)
  ((password :initarg :password :initform (gh-auth-get-password)))
  "Password-based authenticator")

(defmethod gh-auth-modify-request ((auth gh-authenticator) req))

(defmethod gh-auth-modify-request ((auth gh-password-authenticator) req)
  (object-add-to-list req :headers 
                      (cons "Authorization" 
                            (concat "Basic "
                                    (base64-encode-string
                                     (format "%s:%s" (oref auth :username) 
                                             (encode-coding-string 
                                              (oref auth :password) 'utf-8))))))
  req)

;;;###autoload
(defclass gh-oauth-authenticator (gh-authenticator)
  ((token :initarg :token))
  "Oauth-based authenticator")

(defmethod gh-auth-modify-request ((auth gh-oauth-authenticator) req)
  (object-add-to-list req :headers 
                      (cons "Authorization" 
                            (format "token %s" (oref auth :token))))  
  req)

(provide 'gh-auth)
;;; gh-auth.el ends here
