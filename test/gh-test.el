;;; gh-test.el --- test for gh.el

;; Copyright (C) 2012  Yann Hodique

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

(require 'mocker)
(require 'ert)
(require 'url-http)

(when (require 'undercover nil t)
  (undercover "*.el" "gh.el/*.el" (:exclude "gh-pkg.el")))

(defun gh-test-get-traces-root ()
  (let* ((this-file (car
                     (rassoc-if
                      (lambda (items)
                        (member (cons 'provide 'gh-test) items))
                      load-history))))
    (concat (file-name-directory this-file)
            "traces/")))

(defun gh-test-get-fake-http-buffer (filename)
  (let ((buff (generate-new-buffer (concat " " (file-name-base filename)))))
    (with-current-buffer buff
      (insert-file-contents filename)
      (set (make-local-variable
            'url-http-end-of-headers)
           (search-forward-regexp "^$"))
      (make-local-variable 'url-http-response-version)
      (make-local-variable 'url-http-response-status)
      (url-http-parse-response))
    buff))

(defmacro gh-test-with-traces-buffers (bufs &rest body)
  (declare (indent 1) (debug t))
  (let* ((root (gh-test-get-traces-root))
         (syms nil)
         (specs (mapcar
                 (lambda (s)
                   (let* ((sym (car s))
                          (filename (cadr s))
                          (file (concat root filename)))
                     (push sym syms)
                     (list sym `(gh-test-get-fake-http-buffer ,file))))
                 bufs)))
    `(let ,specs
       (unwind-protect
           (progn
             ,@body)
         (dolist (buff (list ,@syms))
           (and (buffer-name buff)
                (kill-buffer buff)))))))

(defun gh-test-mock-api (cls)
  (make-instance cls :sync t
                 :auth (make-instance 'gh-authenticator :username "dummy")))

(defmacro gh-test-mock-url (recs &rest body)
  `(mocker-let ((url-retrieve-synchronously
                 (url)
                 ,recs))
     ,@body))

(defun gh-test-raw-object (filename)
  (let* ((root (gh-test-get-traces-root))
         (file (concat root filename))
         (json-false nil)
         (alist (oref (gh-url-response-init (make-instance 'gh-api-response)
                                            (gh-test-get-fake-http-buffer file))
                      :data)))
    alist))

(defun gh-test-object-view (raw cls)
  (marshal (gh-object-read-into (make-instance cls) raw) 'alist))

(defun gh-test-object-equal (obj1 obj2)
  (cond ((null obj1) (null obj2))
        ((and (json-alist-p obj1)
              (json-alist-p obj2))
         (let ((keys1 (mapcar 'car obj1))
               (keys2 (mapcar 'car obj2)))
           (should (equal (sort keys1 'string<)
                          (sort keys2 'string<)))
           (dolist (k keys1)
             (should (gh-test-object-equal (cdr (assq k obj1))
                                           (cdr (assq k obj2)))))
           t))
        ((and (listp obj1) (listp obj2))
         (and (should (gh-test-object-equal (car obj1) (car obj2)))
              (should (gh-test-object-equal (cdr obj1) (cdr obj2)))))
        (t
         (equal obj1 obj2))))

(defun gh-test:complete-object (fname cls &optional exclude-list)
  (let* ((raw (gh-test-raw-object fname))
         (obj (gh-test-object-view raw cls))
         (pred (lambda (item) (member (car item) exclude-list))))
    (gh-test-object-equal (remove-if pred raw) (remove-if pred obj))))

(defun gh-test:partial-object (fname cls key)
  (let ((raw (gh-test-raw-object fname)))
    (gh-test-object-equal
     (cdr (assq key raw))
     (cdr (assq key (gh-test-object-view raw cls))))))

(provide 'gh-test)
;;; gh-test.el ends here
