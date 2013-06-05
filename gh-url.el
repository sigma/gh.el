;;; gh-url.el --- url wrapper for gh.el

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

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'request)

(defclass gh-url-request ()
  ((method :initarg :method :type string)
   (url :initarg :url :type string)
   (query :initarg :query :initform nil)
   (headers :initarg :headers :initform nil)
   (data :initarg :data :initform "" :type string)
   (async :initarg :async :initform nil)
   (num-retries :initarg :num-retries :initform 0)

   (default-response-cls :allocation :class :initform gh-url-response)))

(defclass gh-url-response ()
  ((data-received :initarg :data-received :initform nil)
   (data :initarg :data :initform nil)
   (headers :initarg :headers :initform nil)
   (http-status :initarg :http-status :initform nil)
   (callbacks :initarg :callbacks :initform nil)
   (transform :initarg :transform :initform nil)
   (-req :initarg :-req :initform nil)))

(defmethod gh-url-response-run-callbacks ((resp gh-url-response))
  (let ((copy-list (lambda (list)
                     (if (consp list)
                         (let ((res nil))
                           (while (consp list) (push (pop list) res))
                           (prog1 (nreverse res) (setcdr res list)))
                       (car list)))))
    (let ((data (oref resp :data)))
      (dolist (cb (funcall copy-list (oref resp :callbacks)))
        (if (or (functionp cb) (symbolp cb))
            (funcall cb data)
          (apply (car cb) data (cdr cb)))
        (object-remove-from-list resp :callbacks cb))))
  resp)

(defmethod gh-url-add-response-callback ((resp gh-url-response) callback)
  (object-add-to-list resp :callbacks callback t)
  (if (oref resp :data-received)
      (gh-url-response-run-callbacks resp)
    resp))

;;; code borrowed from nicferrier's web.el
(defun gh-url-parse-headers (data)
  (let* ((headers nil)
         (header-lines (split-string data "\n"))
         (status-line (car header-lines)))
    (when (string-match
           "HTTP/\\([0-9.]+\\) \\([0-9]\\{3\\}\\)\\( \\(.*\\)\\)*"
           status-line)
      (push (cons 'status-version (match-string 1 status-line)) headers)
      (push (cons 'status-code (match-string 2 status-line)) headers)
      (push (cons 'status-string
                  (or (match-string 4 status-line) ""))
            headers))
    (loop for line in (cdr header-lines)
       if (string-match
           "^\\([A-Za-z0-9.-]+\\):[ ]*\\(.*\\)"
           line)
       do
       (let ((name (match-string 1 line))
             (value (match-string 2 line)))
         (push (cons name value) headers)))
    headers))

(defun gh-url-next-page (headers)
  (let ((links-header (cdr (assoc "Link" headers))))
    (when links-header
      (loop for item in (split-string links-header ", ")
            when (string-match "^<\\(.*\\)>; rel=\"next\"" item)
            return (match-string 1 item)))))

(defmethod gh-url-run-request ((req gh-url-request) &optional resp)
  (unless resp
    (setq resp (make-instance (oref req default-response-cls))))
  (oset resp :-req req)
  (request (oref req :url)
	   :sync    (not (oref req :async))
	   :type    (oref req :method)
	   :headers (oref req :headers)
	   :params  (oref req :query)
	   :data    (oref req :data)
	   :parser  (apply-partially 'gh-request-parse-response resp)
	   :success (apply-partially 'gh-request-handle-success resp)
	   :error   (apply-partially 'gh-request-handle-error resp))
  resp)

(defun gh-request-parse-response (resp)
  (let ((data (buffer-substring-no-properties (point-min) (point-max)))
	(transform (oref resp :transform)))
    (if transform
	(let ((json-array-type 'list))
	  (funcall transform data))
      data)))

(defun* gh-request-handle-success (resp &key response data
					&allow-other-keys)
  (oset resp :http-status (request-response-status-code response))
  (oset resp :headers (gh-url-parse-headers
		       (request-response--raw-header response)))
  (let ((prev-data (oref resp :data))
	(req (oref resp :-req)))
    (oset resp :data (append prev-data data))
    (when (gh-api-paged-request-p req)
      (let ((next-link (gh-url-next-page (oref resp :headers))))
	(when next-link
	  (oset req :url next-link)
	  (gh-url-run-request req resp))))
    (unless prev-data
      (oset resp :data-received t)
      (gh-url-response-run-callbacks resp)))
  resp)

(defun* gh-request-handle-error (resp &key error-thrown
				      &allow-other-keys)
  (let* ((req (oref resp :-req))
	 (num (oref req :num-retries)))
    (if (or (null num) (zerop num))
	(signal (car error-thrown)
		(cdr error-thrown))
      (oset req :num-retries (1- num))
      (gh-url-run-request req resp))))

(provide 'gh-url)
;;; gh-url.el ends here
