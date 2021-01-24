;;; ewlc-log.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Edgar Denny
;;
;; Author: Edgar Denny <http://github/e-denny>
;; Maintainer: Edgar Denny <edgar1denny@gmail.com>
;; Created: August 31, 2020
;; Modified: August 31, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/e-denny/ewlc
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:


(defvar *log-buff* (generate-new-buffer "ewlc-log"))

(defun log-message (msg)
  "Print MSG to log buffer."
  (message "into: log-message")
  (print msg *log-buff*))

(defun write-log-file ()
  "Write log buffer to file."
  (let ((fn "/home/edgar/Projects/ewlc/src/log.txt"))
    (when (file-exists-p fn)
      (delete-file fn))
    (with-current-buffer *log-buff*
      (write-region (point-min) (point-max) fn))))

(provide 'ewlc-log)
;;; ewlc-log.el ends here
