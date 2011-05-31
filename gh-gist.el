;;; gh-gist.el --- gist module for gh.el

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

(require 'gh-api)
(require 'gh-auth)
(require 'gh-common)

(defclass gh-gist-api (gh-api-v3)
  ()
  "Gist API")

(defclass gh-gist-gist ()
  ((date :initarg :date)
   (push-url :initarg :push-url)
   (pull-url :initarg :pull-url)
   (comments :initarg :comments)
   (files :initarg :files :type list)
   (user :initarg :user :type gh-user)
   (public :initarg :public)
   (description :initarg :description)
   (id :initarg :id :type string)
   (url :initarg :url :type string))
  "Gist object")

(defclass gh-gist-gist-file ()
  ((filename :initarg :filename)
   (size :initarg :size)
   (url :initarg :url)
   (content :initarg :content)))

(defun gh-gist-read-list (gists)
  (mapcar 'gh-gist-read gists))

(defun gh-gist-file-read (file)
  (let ((label (car file))
        (file (cdr file)))
    (gh-gist-gist-file label
                       :filename (gh-read file 'filename)
                       :size (gh-read file 'size)
                       :url (gh-read file 'raw_url)
                       :content (gh-read file 'content))))

(defun gh-gist-files-read (files)
  (mapcar 'gh-gist-file-read files))

(defun gh-gist-read (gist)
  (gh-gist-gist "gist" 
                :date (gh-read gist 'created_at)
                :push-url (gh-read gist 'git_push_url)
                :pull-url (gh-read gist 'git_pull_url)
                :comments (gh-read gist 'comments)
                :files (gh-gist-files-read (gh-read gist 'files))
                ;; :user (gh-user-read (gh-read gist 'user))
                :public (gh-read gist 'public)
                :description (gh-read gist 'description)
                :id (gh-read gist 'id)
                :url (gh-read gist 'url)))

(defmethod gh-gist-gist-to-obj ((gist gh-gist-gist))
  `(("description" . ,(oref gist :description))
    ("public" . ,(oref gist :public))
    ("files" . ,(mapcar 'gh-gist-gist-file-to-obj (oref gist :files)))))

(defmethod gh-gist-gist-file-to-obj ((file gh-gist-gist-file))
  `(,(oref file :filename) . (("content" . ,(oref file :content)))))

(defmethod gh-gist-list ((api gh-gist-api) &optional username)
  (gh-api-authenticated-request 
   api 'gh-gist-read-list "GET"
   (format "/users/%s/gists" (or username (gh-api-get-username api)))))

(defmethod gh-gist-list-public ((api gh-gist-api))
  (gh-api-authenticated-request 
   api 'gh-gist-read-list "GET" "/gists/public"))

(defmethod gh-gist-list-starred ((api gh-gist-api))
  (gh-api-authenticated-request 
   api 'gh-gist-read-list "GET" "/gists/starred"))

(defmethod gh-gist-get ((api gh-gist-api) gist-or-id)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     api 'gh-gist-read "GET" (format "/gists/%s" id))))

(defmethod gh-gist-new ((api gh-gist-api) gist)
  (gh-api-authenticated-request
   api 'gh-gist-read "POST" (format "/users/%s/gists" 
                                    (gh-api-get-username api))
   (gh-gist-gist-to-obj gist)))

(defmethod gh-gist-edit ((api gh-gist-api) gist)
  (gh-api-authenticated-request
   api 'gh-gist-read "PATCH" (format "/users/%s/gists" 
                                     (gh-api-get-username api))
   (gh-gist-gist-to-obj gist)))

(defmethod gh-gist-set-star ((api gh-gist-api) gist-or-id star)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     api 'ignore (if star "POST" "DELETE") 
     (format "/gists/%s/star" id))))

(defmethod gh-gist-get-star ((api gh-gist-api) gist-or-id)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     api 'ignore "GET" (format "/gists/%s/star" id))))

(defmethod gh-gist-fork ((api gh-gist-api) gist-or-id)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     api 'gh-gist-read "POST" (format "/gists/%s/fork" id))))

(defmethod gh-gist-delete ((api gh-gist-api) gist-or-id)
  (let ((id (if (stringp gist-or-id) gist-or-id
              (oref gist-or-id :id))))
    (gh-api-authenticated-request
     api 'ignore "DELETE" (format "/gists/%s" id))))

(provide 'gh-gist)
;;; gh-gist.el ends here
