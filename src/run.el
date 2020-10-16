;;; run.el --- description -*- lexical-binding: t; -*-
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



;; (defconst ewlc--server-name "server-ewlc"
;;   "Name of the subordinate Emacs server.")

;; (defvar ewlc--server-process nil
;;    "Process of the subordinate Emacs server.")

;; (defun ewlc--server-stop ()
;;   "Stop the subordinate Emacs server."
;;   (ewlc--log)
;;   (server-force-delete ewlc--server-name)
;;   (when ewlc--server-process
;;     (delete-process ewlc--server-process)
;;     (setq ewlc--server-process nil)))

;; (defun ewlc--server-eval-at (&rest args)
;;   "Wrapper of `server-eval-at' used to advice subrs."
;;   ;; Start the subordinate Emacs server if it's not alive
;;   (ewlc--log "%s" args)
;;   (unless (server-running-p ewlc--server-name)
;;     (when ewlc--server-process (delete-process ewlc--server-process))
;;     (setq ewlc--server-process
;;           (start-process ewlc--server-name
;;                          nil
;;                          (car command-line-args) ;The executable file
;;                          "-d" (frame-parameter nil 'display)
;;                          "-Q"
;;                          (concat "--daemon=" ewlc--server-name)
;;                          "--eval"
;;                          ;; Create an invisible frame
;;                          "(make-frame '((window-system . x) (visibility)))"))
;;     (while (not (server-running-p ewlc--server-name))
;;       (sit-for 0.001)))
;;   (server-eval-at
;;    ewlc--server-name 'something))


(module-load "/home/edgar/Projects/ewlc/lib/ewlc.so")

(require 'ewlc)

(defvar *ewlc-thread* "The thread running the wayland event loop.")

(defvar *ewlc-running* nil "Is the wayland compositor running?")

(defun start-wc ()
  "Start the wayland compositor."
  (ewlc-start)
  (setq *ewlc-running* t)
  (setq *ewlc-thread* (make-thread
                       (lambda ()
                         (while *ewlc-running*
                           ;; dispatch queued wayland events.
                           (message "before dispatch")
                           (ewlc-display-dispatch)
                           (message "before handle")
                           (setq result (ewlc-handle-keybindings))
                           (if result
                               (message "result: t")
                             (message "result: nil"))
                           (sleep-for 0.01)))
                       "loop-thread")))
(defun exit-wc ()
  "Exit the wayland compositor."
  (ewlc-cleanup)
  (setq *ewlc-running* nil))

(defun ewlc-apply-keybinding (mod key)
  "Apply the keybings for MOD and KEY."
  (message "into apply.")
  1)


(run-command term-cmd)

(ewlc-focus-next-client 1)
(ewlc-focus-next-client -1)

(set-master-ratio -0.05)
(set-master-ratio 0.05)

(next-master 1)
(next-master -1)


(start-wc)
(exit-wc)

(provide 'run)
;;; run.el ends here
