;;; gh-cache.el --- caching for gh.el

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

(eval-when-compile
  (require 'cl))

(require 'eieio)
(require 'pcache)

(defclass gh-cache (pcache-repository)
  ((entries :initarg :entries :initform (make-hash-table :test 'equal))
   (safe-methods :allocation :class :initform ("GET" "HEAD"))
   (invalidation-chain :allocation :class :initform nil)))

(defmethod pcache-invalidate :after ((cache gh-cache) key)
  (dolist (next (oref cache invalidation-chain))
    (let ((nextkey
           (replace-regexp-in-string (car next) (cdr next) key)))
      (when (not (equal key nextkey))
        (pcache-invalidate cache nextkey)))))

(provide 'gh-cache)
;;; gh-cache.el ends here
