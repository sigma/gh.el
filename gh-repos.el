;;; gh-repos.el --- repos module for gh.el

;; Copyright (C) 2011  Free Software Foundation, Inc.

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

;;;###autoload
(defclass gh-repos-api (gh-api-v3)
  ()
  "Repos API")

;;;###autoload
(defclass gh-repos-repo-stub ()
  ((name :initarg :name)
   (description :initarg :description :initform nil)
   (homepage :initarg :homepage :initform nil)
   (private :initarg :private :initform nil))
  "Class for user-created repository objects")

;;;###autoload
(defclass gh-repos-repo (gh-repos-repo-stub)
  ((url :initarg :url)
   (html-url :initarg :html-url)
   (clone-url :initarg :clone-url)
   (git-url :initarg :git-url)
   (ssh-url :initarg :ssh-url)
   (svn-url :initarg :svn-url)
   (owner :initarg :owner :initform nil)
   (language :initarg :language)
   (fork :initarg :fork)
   (forks :initarg :forks)
   (watchers :initarg :watchers)
   (size :initarg :size)
   (open-issues :initarg :open-issues)
   (pushed-at :initarg :pushed-at)
   (created-at :initarg :created-at))
  "Class for GitHub repositories")

(defun gh-repos-repo-read (repo &optional into)
  (let ((target (or into (gh-repos-repo "repo"))))
    (with-slots (url html-url clone-url git-url ssh-url svn-url
                     owner name description homepage language
                     private fork forks watchers size open-issues
                     pushed-at created-at)
        target
      (setq url (gh-read repo 'url)
            html-url (gh-read repo 'html_url)
            clone-url (gh-read repo 'clone_url)
            git-url (gh-read repo 'git_url)
            ssh-url (gh-read repo 'ssh_url)
            svn-url (gh-read repo 'svn_url)
            owner (gh-user-read (gh-read repo 'owner) (oref target :owner))
            name (gh-read repo 'name)
            description (gh-read repo 'description)
            homepage (gh-read repo 'homepage)
            language (gh-read repo 'language)
            private (gh-read repo 'private)
            fork (gh-read repo 'fork)
            forks (gh-read repo 'forks)
            watchers (gh-read repo 'watchers)
            size (gh-read repo 'size)
            open-issues (gh-read repo 'open_issues)
            pushed-at (gh-read repo 'pushed_at)
            created-at (gh-read repo 'created_at)))
    target))

(defun gh-repos-read-list (repos)
  (mapcar 'gh-repos-repo-read repos))

(defmethod gh-repos-user-list ((api gh-repos-api) &optional username)
  (gh-api-authenticated-request
   api 'gh-repos-read-list "GET"
   (format "/users/%s/repos" (or username (gh-api-get-username api)))))

(defmethod gh-repos-org-list ((api gh-repos-api) org)
  (gh-api-authenticated-request
   api 'gh-repos-read-list "GET"
   (format "/orgs/%s/repos" org)))

(defmethod gh-repos-repo-to-obj ((repo gh-repos-repo-stub)
                                 &rest caps)
  (let ((has_issues (plist-member caps :issues))
        (has_wiki (plist-member caps :wiki))
        (has_downloads (plist-member caps :downloads)))
    `(("name" . ,(oref repo :name))
      ("homepage" . ,(oref repo :homepage))
      ("description" . ,(oref repo :description))
      ("public" . ,(not (oref repo :private)))
      ,@(when has_issues
          (list (cons "has_issues" (plist-get caps :issues))))
      ,@(when has_wiki
          (list (cons "has_wiki" (plist-get caps :wiki))))
      ,@(when has_downloads
          (list (cons "has_downloads" (plist-get caps :downloads)))))))

(defmethod gh-repos-repo-new ((api gh-repos-api) repo-stub
                              &optional org &rest caps)
  (gh-api-authenticated-request
   api 'gh-repos-repo-read "POST" (if org (format "/orgs/%s/repos" org)
                                    "/user/repos")
   (apply 'gh-repos-repo-to-obj repo-stub caps)))

(defmethod gh-repos-repo-get ((api gh-repos-api) repo-id &optional user)
  (gh-api-authenticated-request
   api 'gh-repos-repo-read "GET" (format "/repos/%s/%s"
                                         (or user (gh-api-get-username api))
                                         repo-id)))

(defmethod gh-repos-repo-update ((api gh-repos-api) repo-stub
                                 &optional user &rest caps)
  (gh-api-authenticated-request
   api 'gh-repos-repo-read "PATCH" (format "/repos/%s/%s"
					   (or user (gh-api-get-username api))
					   (oref repo-stub :name))
   (apply 'gh-repos-repo-to-obj repo-stub caps)))

(defmethod gh-repos-repo-contributors ((api gh-repos-api) repo)
  (gh-api-authenticated-request
   api 'gh-user-read-list "GET" (format "/repos/%s/%s/contributors"
                                        (oref (oref repo :owner) :login)
                                        (oref repo :name))))



;;; TODO: generate some useful objects with the return values

(defmethod gh-repos-repo-languages ((api gh-repos-api) repo)
  (gh-api-authenticated-request
   api nil "GET" (format "/repos/%s/%s/languages"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

(defmethod gh-repos-repo-teams ((api gh-repos-api) repo)
  (gh-api-authenticated-request
   api nil "GET" (format "/repos/%s/%s/teams"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

(defmethod gh-repos-repo-tags ((api gh-repos-api) repo)
  (gh-api-authenticated-request
   api nil "GET" (format "/repos/%s/%s/tags"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

(defmethod gh-repos-repo-branches ((api gh-repos-api) repo)
  (gh-api-authenticated-request
   api nil "GET" (format "/repos/%s/%s/branches"
                         (oref (oref repo :owner) :login)
                         (oref repo :name))))

;;; Forks sub-API

(defmethod gh-repos-forks-list ((api gh-repos-api) repo)
  (gh-api-authenticated-request
   api 'gh-repos-read-list "GET" (format "/repos/%s/%s/forks"
                                         (oref (oref repo :owner) :login)
                                         (oref repo :name))))

(defmethod gh-repos-fork ((api gh-repos-api) repo &optional org)
  (gh-api-authenticated-request
   api 'gh-repos-repo-read "POST" (format "/repos/%s/%s/forks"
                                          (oref (oref repo :owner) :login)
                                          (oref repo :name))
   nil (when org `(("org" . ,org)))))

(provide 'gh-repos)
;;; gh-repos.el ends here
