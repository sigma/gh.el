;;; gh-comments.el --- comments module for gh.el

;; Copyright (C) 2014 Toni Reina

;; Author: Toni Reina <areina0@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'gh-api)
(require 'gh-auth)
(require 'gh-common)

(defclass gh-comments-api (gh-api-v3)
  ((comment-cls :allocation :class :initform gh-comments-comment))
  "GitHub Comments api")

(defclass gh-comments-comment (gh-object)
  ((url :initarg :url)
   (html-url :initarg :html-url)
   (id :initarg :id)
   (body :initarg :body)
   (user :initarg :user :initform nil)
   (path :initarg :path)
   (position :initarg :position)
   (line :initarg :line)
   (commit-id :initarg :commit-id)
   (created-at :initarg :created_at)
   (updated-at :initarg :updated_at)
   (user-cls :allocation :class :initform gh-user))
  "Class for comments")

(defmethod gh-object-read-into ((comment gh-comments-comment) data)
  (call-next-method)
  (with-slots (url html-url id body user path position line commit-id created-at updated-at)
      comment
    (setq url (gh-read data 'url)
          html-url (gh-read data 'html_url)
          id (gh-read data 'id)
          body (gh-read data 'body)
          user (gh-object-read  (or (oref comment :user)
                                    (oref comment user-cls))
                                (gh-read data 'user))
          path (gh-read data 'path)
          position (gh-read data 'position)
          line (gh-read data 'line)
          commit-id (gh-read data 'commit_id)
          created-at (gh-read data 'created_at)
          updated-at (gh-read data 'updated_at))))

;; https://developer.github.com/v3/repos/comments/#list-commit-comments-for-a-repository
(defmethod gh-comments-list-repo ((api gh-comments-api) user repo)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api comment-cls)) "GET"
   (format "/repos/%s/%s/comments" user repo)))

;; https://developer.github.com/v3/repos/comments/#list-comments-for-a-single-commit
(defmethod gh-comments-list-commit ((api gh-comments-api) user repo ref)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api comment-cls)) "GET"
   (format "/repos/%s/%s/commits/%s/comments" user repo ref)))

(defmethod gh-comments-req-to-create ((req gh-comments-comment))
  `(("body" . ,(oref req body))
    ("path". ,(oref req path))
    ("line". ,(oref req line))
    ("position". ,(oref req position))))

;; https://developer.github.com/v3/repos/comments/#create-a-commit-comment
(defmethod gh-comments-new ((api gh-comments-api) user repo sha comment)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api comment-cls)) "POST"
   (format "/repos/%s/%s/commits/%s/comments" user repo sha)
   (gh-comments-req-to-create comment)))

;; https://developer.github.com/v3/repos/comments/#get-a-single-commit-comment
(defmethod gh-comments-get ((api gh-comments-api) user repo id)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api comment-cls)) "GET"
   (format "/repos/%s/%s/comments/%s" user repo id)))

(defmethod gh-comments-req-to-update ((req gh-comments-comment))
  `(("body" . ,(oref req body))))

;; https://developer.github.com/v3/repos/comments/#update-a-commit-comment
(defmethod gh-comments-update ((api gh-comments-api) user repo id comment)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api comment-cls)) "PATCH"
   (format "/repos/%s/%s/comments/%s" user repo id)
   (gh-comments-req-to-update comment)))

;; https://developer.github.com/v3/repos/comments/#delete-a-commit-comment
(defmethod gh-comments-delete ((api gh-comments-api) user repo id)
  (gh-api-authenticated-request
   api nil "DELETE"
   (format "/repos/%s/%s/comments/%s" user repo id)))

(provide 'gh-comments)
;;; gh-comments.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
