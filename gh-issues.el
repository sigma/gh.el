;;; gh-issues.el --- issues api for github

;; Copyright (C) 2012  Raimon Grau

;; Author: Raimon Grau <raimonster@gmail.com>
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

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'gh-api)
(require 'gh-auth)
(require 'gh-common)

(require 'gh-repos)

(defclass gh-issues-api (gh-api-v3)
  ((req-cls :allocation :class :initform gh-issues-issue))
  "Github Issues api")

(defclass gh-issues-issue (gh-object)
  ((url :initarg :url)
   (html-url :initarg :html-url)
   (number :initarg :number)
   (state :initarg :state)
   (title :initarg :title)
   (body :initarg :body)
   (user :initarg :user :initform gh-user)
   (labels :initarg :labels :initform nil)
   (assignee :initarg :assignee :initform gh-user)
   (milestone :initarg :milestone :initform gh-milestone)
   (open_issues :initarg :open_issues)
   (closed_issues :initarg :closed_issues)
   (created_at :initarg :created_at)
   (due_on :initarg :due_on))
  "issues request")

(defclass gh-issues-label (gh-object)
  ((url :initarg :url)
   (name :initarg :name)
   (color :initarg :color)))

(defclass gh-milestone (gh-object)
  ((url :initarg :url)
   (number :initarg :number)
   (state :initarg :state)
   (title :initarg :title)
   (description :initarg :description)
   (creator :initarg :creator :initform gh-user)
   (open_issues :initarg :open_issues)
   (closed_issues :initarg :closed_issues)
   (created_at :initarg :created_at)
   (due_on :initarg :due_on))
  "github milestone")

(defmethod gh-object-read-into ((issue gh-issues-issue) data)
  (call-next-method)
  (with-slots (url html-url number state title body
                   user labels assignee milestone open_issues
                   closed_issues created_at due_on)
      issue
    (setq url (gh-read data 'url)
          url (gh-read data 'url)
          html-url (gh-read data 'html-url)
          number (gh-read data 'number)
          state (gh-read data 'state)
          title (gh-read data 'title)
          body (gh-read data 'body)
          user (gh-object-read  (oref issue :user) (gh-read data 'user))
          labels (gh-read data 'labels)
          assignee (gh-object-read  (oref issue :assignee) (gh-read data 'assignee))
          milestone (gh-read data 'milestone)
          open_issues (gh-read data 'open_issues)
          closed_issues (gh-read data 'closed_issues)
          created_at (gh-read data 'created_at)
          due_on (gh-read data 'due_on))))

(defmethod gh-issues-issue-list ((api gh-issues-api) user repo)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api req-cls)) "GET"
   (format "/repos/%s/%s/issues" user repo)))

(defmethod gh-issues-issue-get ((api gh-issues-api) user repo id)
  (gh-authenticated-request
   api (gh-object-reader (oref api req-cls)) "GET"
   (format "/repos/%s/%s/issues/%s" user repo id)))

(defmethod gh-issues-issue-req-to-update ((req gh-issues-issue))
  `(("title" . ,(oref req title))
    ("body" . ,(oref req body))
    ("assignee" . ,(oref (oref req assignee) login) )
    ("labels" . ,(oref (oref req assignee) labels) )
    ("state" . ,(oref req state) )
    ("milestone" . ,(oref req milestone))))

(defmethod gh-issues-issue-update ((api gh-issues-api) user repo id req)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api req-cls)) "PATCH"
   (format "/repos/%s/%s/issues/%s" user repo id)
   (gh-issues-req-to-update req)))

(defmethod gh-issues-issue-new ((api gh-issues-api) user repo issue)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api req-cls)) "POST"
   (format "/repos/%s/%s/issues" user repo)
   (gh-issues-req-to-update issue)))

(provide 'gh-issues)
;;; gh-issues.el ends here
