;;; ewlc-wm.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Edgar Denny
;;
;; Author: Edgar Denny <http://github/e-denny>
;; Maintainer: Edgar Denny <edgar1denny@gmail.com>
;; Created: August 31, 2020
;; Modified: August 31, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/e-denny-ewlc
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(module-load "/home/edgar/Projects/ewlc/lib/ewlc.so")

(require 'ewlc)

(defvar *ewlc-thread* nil "The thread running the wayland event loop.")

(defvar *ewlc-running* nil "Is the wayland compositor running?")

(defvar ewlc-keymap-prefix "C-," "The ewlc keymap prefix.")

(defun wc-focus-next-client ()
  "Focus next client."
  (ewlc-focus-next-client 1))

(defun wc-focus-prev-client ()
  "Focus previous client."
  (ewlc-focus-next-client -1))

(defun wc-next-master ()
  "Next-master."
  (ewlc-next-master 1))

(defun wc-prev-master ()
  "Prev master."
  (ewlc-next-master -1))

(defun wc-incr-master-ratio ()
  "Inrement master ratio."
  (ewlc-set-master-ratio 0.05))

(defun wc-decr-master-ratio ()
  "Decrement master ratio."
  (ewlc-set-master-ratio -0.05))

(defun wc-kill-client ()
  "Kill the active client."
  (ewlc-kill-client))

(defun exit-wc ()
  "Exit the wayland compositor."
  (ewlc-cleanup)
  (setq *ewlc-running* nil))

(defun wc-quit ()
  "Kill the window manager."
  (ewlc-quit)
  (exit-wm))

(defvar ewlc-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'wc-focus-next-client)
    (define-key map (kbd "p") #'wc-focus-prev-client)
    ))

(defvar ewlc-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map ewlc-keymap-prefix #'ewlc-command-map)
    (define-key map (kbd "M-n") #'wc-focus-next-client)
    (define-key map (kbd "M-p") #'wc-focus-prev-client)
    (define-key map (kbd "M-i") #'wc-incr-master-ratio)
    (define-key map (kbd "M-d") #'wc-decr-master-ratio)
    (define-key map (kbd "M-m") #'wc-next-master)
    (define-key map (kbd "M-b") #'wc-prev_master)
    (define-key map (kbd "M-k") #'wc-kill-client)
    (define-key map (kbd "M-q") #'wc-quit)
    map
    ))

;; (define-minor-mode ewlc-mode
;;   "Enable keymap for ewlc."
;;   :keymap ewlc-keymap-prefix
;;   :global defun)

(defun start-wc-old()
  "Start the wayland compositor."
  (ewlc-start)
  (setq *ewlc-running* t)
  (setq *ewlc-thread* (make-thread
                       (lambda ()
                         (while *ewlc-running*
                           ;; dispatch queued wayland events.
                           (ewlc-display-dispatch)
                           (ewlc-handle-keybindings)
                           (sleep-for 0.01)))
                       "loop-thread")))


(defun wm-loop ()
  (ewlc-display-dispatch)
  (ewlc-handle-keybindings))

(defun start-wc()
  "Start the wayland compositor."
  (ewlc-start)
  (setq *ewlc-running* t)
  (run-at-time 0 0.01 #'wm-loop))

(defun ewlc-apply-keybinding (mod key)
  "Apply the keybings for MOD and KEY."
  (let ((command nil)
        (handled 0))
    (message "keys: %s, %s" mod key)
    (setq command (lookup-key ewlc-prefix-map (kbd (concat mod key))))
    (message "command: %s" command)
    (when (and command (fboundp command))
      (funcall command)
      (setq handled 1))
    handled))

(provide 'ewlc-wm)
;;; ewlc-wm.el ends here
