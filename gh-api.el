;;; gh-api.el --- api definition for gh.el

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

(require 'cl)
(require 'json)
(require 'gh-auth)

(defgroup gh-api nil
  "Github API."
  :group 'gh)

;;;###autoload
(defclass gh-api ()
  ((sync :initarg :sync :initform t)
   (base :initarg :base :type string)
   (auth :initarg :auth :initform nil)
   (data-format :initarg :data-format))
  "Github API")

(defmethod gh-api-expand-resource ((api gh-api)
                                   resource)
  resource)

(defmethod gh-api-get-username ((api gh-api))
  (oref (oref api :auth) :username))

;;;###autoload
(defclass gh-api-v2 (gh-api)
  ((base :initarg :base :initform "https://github.com/api/v2/json")
   (data-format :initarg :data-format :initform :json)))

(defcustom gh-api-v2-authenticator 'gh-oauth-authenticator
  "Authenticator for Github API v2"
  :type '(choice (const :tag "Password" gh-password-authenticator)
                 (const :tag "OAuth" gh-oauth-authenticator))
  :group 'gh-api)

(defmethod constructor :static ((api gh-api-v2) newname &rest args)
  (let ((obj (call-next-method)))
    (or (oref obj :auth)
        (oset obj :auth (funcall gh-api-v2-authenticator "auth")))
    obj))

;;;###autoload
(defclass gh-api-v3 (gh-api)
  ((base :initarg :base :initform "https://api.github.com")
   (data-format :initarg :data-format :initform :json))
  "Github API v3")

(defcustom gh-api-v3-authenticator 'gh-oauth-authenticator
  "Authenticator for Github API v3"
  :type '(choice (const :tag "Password" gh-password-authenticator)
                 (const :tag "OAuth" gh-oauth-authenticator))
  :group 'gh-api)

(defmethod constructor :static ((api gh-api-v3) newname &rest args)
  (let ((obj (call-next-method)))
    (or (oref obj :auth)
        (oset obj :auth (funcall gh-api-v3-authenticator "auth")))
    obj))

(defclass gh-api-request ()
  ((method :initarg :method :type string)
   (url :initarg :url :type string)
   (headers :initarg :headers)
   (data :initarg :data :initform "" :type string)))

(defclass gh-api-response ()
  ((data :initarg :data :initform nil)
   (callbacks :initarg :callbacks :initform nil))
  "Class for API responses")

(defun gh-api-json-decode (repr)
  (if (or (null repr) (string= repr ""))
      'empty
    (let ((json-array-type 'list))
      (json-read-from-string repr))))

(defun gh-api-json-encode (json)
  (json-encode-list json))

(defun gh-api-form-encode (form)
  (mapconcat (lambda (x) (format "%s=%s" (car x) (cdr x)))
             form "&"))

(defun gh-api-params-encode (form)
  (concat "?" (gh-api-form-encode form)))

(defmethod gh-api-response-init ((resp gh-api-response)
                                 buffer &optional transform)
  (declare (special url-http-end-of-headers))
  (with-current-buffer buffer
    (goto-char (1+ url-http-end-of-headers))
    (oset resp :data (let ((raw (buffer-substring (point) (point-max))))
                       (if transform
                           (funcall transform (gh-api-json-decode raw))
                         raw))))
  (kill-buffer buffer)
  (gh-api-response-run-callbacks resp)
  resp)

(defun gh-api-set-response (status resp transform)
  (gh-api-response-init resp (current-buffer) transform))

(defmethod gh-api-response-run-callbacks ((resp gh-api-response))
  (let ((data (oref resp :data)))
    (when data
      (dolist (cb (copy-list (oref resp :callbacks)))
        (funcall cb data)
        (object-remove-from-list resp :callbacks cb)))))

(defmethod gh-api-add-response-callback ((resp gh-api-response) callback)
  (object-add-to-list resp :callbacks callback t)
  (gh-api-response-run-callbacks resp))

(defmethod gh-api-authenticated-request
  ((api gh-api) transformer method resource &optional data params)
  (let* ((fmt (oref api :data-format))
         (req (gh-auth-modify-request
               (oref api :auth)
               (gh-api-request "request"
                               :method method
                               :url (concat (oref api :base)
                                            (gh-api-expand-resource
                                             api resource)
                                            (if params
                                                (gh-api-params-encode data)
                                              ""))
                               :headers (if (eq fmt :form)
                                            '(("Content-Type" . "application/x-www-form-urlencoded")))
                               :data (or (and (eq fmt :json)
                                              (gh-api-json-encode data))
                                         (and (eq fmt :form)
                                              (gh-api-form-encode data))
                                         "")))))
    (let ((url-request-method (oref req :method))
          (url-request-data (oref req :data))
          (url-request-extra-headers (oref req :headers))
          (url (oref req :url)))
      (if (oref api :sync)
          (let ((resp (gh-api-response "sync")))
            (gh-api-response-init resp
                                  (url-retrieve-synchronously url)
                                  transformer)
            resp)
        (let ((resp (gh-api-response "async")))
          (url-retrieve url 'gh-api-set-response (list resp transformer))
          resp)))))

(provide 'gh-api)
;;; gh-api.el ends here
