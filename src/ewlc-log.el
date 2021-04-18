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


(defvar *log-file* "/home/edgar/Projects/ewlc/src/log-file.txt")

(defun log-message (msg)
  "Add a given MSG string to the end of a file."
  (append-to-file (concat msg "\n") nil *log-file*))

(defun create-log-file ()
  (interactive)
  "Write log buffer to file."
  (let ((fn *log-file*))
    (when (file-exists-p fn)
      (delete-file fn))
    (make-empty-file fn)))

(provide 'ewlc-log)
;;; ewlc-log.el ends here
