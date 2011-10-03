;;; gh-pulls.el --- pull requests module for gh.el

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
(require 'gh-api)
(require 'gh-auth)
(require 'gh-common)

(require 'gh-repos)

;;;###autoload
(defclass gh-pulls-api (gh-api-v3)
  ()
  "Git pull requests API")

;;;###autoload
(defclass gh-pulls-ref ()
  ((label :initarg :label)
   (ref :initarg :ref :initform nil)
   (sha :initarg :sha :initform nil)
   (user :initarg :user :initform nil)
   (repo :initarg :repo :initform nil)))

(defclass gh-pulls-request-stub ()
  ((url :initarg :url)
   (html-url :initarg :html-url)
   (diff-url :initarg :diff-url)
   (patch-url :initarg :patch-url)
   (issue-url :initarg :issue-url)
   (number :initarg :number)
   (state :initarg :state)
   (title :initarg :title)
   (body :initarg :body)
   (created-at :initarg :created-at)
   (updated-at :initarg :updated-at)
   (closed-at :initarg :closed-at)
   (merged-at :initarg :merged-at)))

;;;###autoload
(defclass gh-pulls-request (gh-pulls-request-stub)
  ((merged :initarg :merged)
   (mergeable :initarg :mergeable)
   (merged-by :initarg :merged-by)
   (comments :initarg :comments)
   (commits :initarg :commits)
   (additions :initarg :additions)
   (deletions :initarg :deletions)
   (changed-files :initarg :changed-files)
   (head :initarg :head :initform nil)
   (base :initarg :base :initform nil))
  "Git pull requests API")

(defmethod gh-pulls-req-to-new ((req gh-pulls-request))
  (let ((head (oref req :head))
        (base (oref req :base)))
    `(("title" . ,(oref req :title))
      ("body" . ,(oref req :body))
      ("head" . ,(or (oref head :ref) (oref head :sha)))
      ("base" . ,(or (oref base :ref) (oref base :sha))))))

(defmethod gh-pulls-req-to-update ((req gh-pulls-request))
  `(("title" . ,(oref req :title))
    ("body" . ,(oref req :body))
    ("state" . ,(oref req :state))))

(defun gh-pulls-ref-read (reference &optional into)
  (let ((target (or into (gh-pulls-ref "ref"))))
    (with-slots (label ref sha user repo)
        target
      (setq label (gh-read reference 'label)
            ref (gh-read reference 'ref)
            sha (gh-read reference 'sha)
            user (gh-user-read (gh-read reference 'user) (oref target :user))
            repo (gh-repos-repo-read (gh-read reference 'repo)
                                     (oref target :repo))))
    target))

(defun gh-pulls-request-read (req &optional into)
  (let ((target (or into (gh-pulls-request "request"))))
    (with-slots (url html-url diff-url patch-url issue-url number
                     state title body created-at updated-at
                     closed-at merged-at merged mergeable
                     merged-by comments commits additions
                     deletions changed-files head base)
        target
      (setq url (gh-read req 'url)
            html-url (gh-read req 'html_url)
            diff-url (gh-read req 'diff_url)
            patch-url (gh-read req 'patch_url)
            issue-url (gh-read req 'issue_url)
            number (gh-read req 'number)
            state (gh-read req 'state)
            title (gh-read req 'title)
            body (gh-read req 'body)
            created-at (gh-read req 'created_at)
            updated-at (gh-read req 'updated_at)
            closed-at (gh-read req 'closed_at)
            merged-at (gh-read req 'merged_at)
            merged (gh-read req 'merged)
            mergeable (gh-read req 'mergeable)
            merged-by (gh-read req 'merged_by)
            comments (gh-read req 'comments)
            commits (gh-read req 'commits)
            additions (gh-read req 'additions)
            deletions (gh-read req 'deletions)
            changed-files (gh-read req 'changed_files)
            head (gh-pulls-ref-read (gh-read req 'head) (oref target :head))
            base (gh-pulls-ref-read (gh-read req 'base) (oref target :base))))
    target))

(defun gh-pulls-requests-read (reqs)
  (mapcar 'gh-pulls-request-read reqs))

(defmethod gh-pulls-list ((api gh-pulls-api) user repo)
  (gh-api-authenticated-request
   api 'gh-pulls-requests-read "GET"
   (format "/repos/%s/%s/pulls" user repo)))

(defmethod gh-pulls-get ((api gh-pulls-api) user repo id)
  (gh-api-authenticated-request
   api 'gh-pulls-request-read "GET"
   (format "/repos/%s/%s/pulls/%s" user repo id)))

(defmethod gh-pulls-new ((api gh-pulls-api) user repo req)
  (gh-api-authenticated-request
   api 'gh-pulls-request-read "POST" (format "/repos/%s/%s/pulls"
                                             user repo)
   (gh-pulls-req-to-new req)))

(defmethod gh-pulls-update ((api gh-pulls-api) user repo id req)
  (gh-api-authenticated-request
   api 'gh-pulls-request-read "PATCH" (format "/repos/%s/%s/pulls/%s"
                                              user repo id)
   (gh-pulls-req-to-update req)))

(provide 'gh-pulls)
;;; gh-pulls.el ends here
