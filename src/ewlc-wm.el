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

;; (require ewlc)
(require 'cl-lib)

(defvar *ewlc-thread* nil "The thread running the wayland event loop.")

(defvar *ewlc-running* nil "Is the wayland compositor running?")

(defvar *ewlc-server* nil "The wayland compositor server.")

(defvar ewlc-keymap-prefix "C-," "The ewlc keymap prefix.")


;;--------------------------------------------------------------------

(defun client= (client-1 client-2)
  "Compare CLIENT-1 and CLIENT-2."
  (ewlc/c--client= client-1 client-2))

(defun move-client-to-front (elm lst)
  "Move ELM to front of the LST."
  (let ((new-lst (cl-remove elm lst :test 'client=)))
    (setq new-lst (cons elm new-lst))
    new-lst))

(defun new-focus-client (curr-client next-client restack)
  "Change focus from CURR-CLIENT to NEXT-CLIENT with possible RESTACK."
  (let ((client-stack-list (ewlc/c--get-stack-list *ewlc-server*))
        (client-focus-list (ewlc/c--get-focus-list *ewlc-server*)))
    (when next-client
      (when restack
        (setq client-stack-list (move-client-to-front next-client client-stack-list))
        (ewlc/c--set-stack-list *ewlc-server* client-stack-list))
      (when (not (client= curr-client next-client))
        (setq client-focus-list (move-client-to-front next-client client-focus-list))
        (ewlc/c--set-focus-list *ewlc-server* client-focus-list)))
    (when (not (client= curr-client next-client))
      (ewlc/c--focus-client curr-client next-client))))

(defun get-next-visible (client output client-list direction)
  "Get next or prev CLIENT within CLIENT-LIST which is visible on OUTPUT.
DIRECTION is 'next or 'prev."
  (let* (next
        (lst (if (equal direction 'prev)
                 (reverse client-list)
               (cl-copy-list client-list)))
        (i (cl-position client lst :test 'client=)))
    ;FIXME: the 'prev does not work correctly
    (cl-dolist (c (nthcdr (+ i 1) lst))
      (when (ewlc/c--is-visible-on c output)
        (setq next c)
        (cl-return next)))
    (when (not next)
      (cl-dolist (c (seq-subseq lst 0 (- i 1)))
        (when (ewlc/c--is-visible-on c output)
          (setq next c)
          (cl-return next))))
    next))

;; replacement function
(defun new-focus-next-client (direction)
  "Focus next client in DIRECTION next or prev."
  (let ((curr-client (ewlc/c--get-active-client *ewlc-server*))
        next-client
        (client-list (ewlc/c--get-client-list *ewlc-server*))
        (curr-output (ewlc/c--get-active-output *ewlc-server*)))
    (when curr-client
      (setq next-client (get-next-visible curr-client curr-output client-list direction))
      (new-focus-client curr-client next-client t))))

;;--------------------------------------------------------------------

(defun wc-focus-next-client ()
  "Focus next client."
  (new-focus-next-client 'next))

(defun wc-focus-prev-client ()
  "Focus previous client."
  (new-focus-next-client 'prev))

(defun wc-add-master ()
  "Add a master."
  (ewlc-add-master *ewlc-server* 1))

(defun wc-remove-master ()
  "Remove a master."
  (ewlc-add-master *ewlc-server* -1))

(defun wc-incr-master-ratio ()
  "Inrement master ratio."
  (ewlc-set-master-ratio *ewlc-server* 0.05))

(defun wc-decr-master-ratio ()
  "Decrement master ratio."
  (ewlc-set-master-ratio *ewlc-server* -0.05))

(defun wc-kill-client ()
  "Kill the active client."
  (ewlc-kill-client *ewlc-server*))

(defun wc-zoom ()
  "Zoom the active client."
  (ewlc-zoom *ewlc-server*))

(defun wc-toggle-floating ()
  "Zoom the active client."
  (ewlc-toggle-floating *ewlc-server*))

(defun wc-view ()
  "Zoom the active client."
  (ewlc-view *ewlc-server*))

(defun wc-spawn (cmd args)
  "Spawn an application CMD with the arguments ARGS."
  (ewlc-spawn cmd args))

(defun exit-wc ()
  "Exit the wayland compositor."
  (ewlc-cleanup *ewlc-server*)
  (setq *ewlc-running* nil))

(defun wc-quit ()
  "Kill the window manager."
  (ewlc-quit *ewlc-server*)
  (exit-wc))

(defun terminal ()
  "Start a terminal."
  (ewlc-spawn "alacritty" ""))

(defvar ewlc-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'wc-focus-next-client)
    (define-key map (kbd "p") #'wc-focus-prev-client)))

(defvar ewlc-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map ewlc-keymap-prefix #'ewlc-command-map)
    (define-key map (kbd "M-n") #'wc-focus-next-client)
    (define-key map (kbd "M-p") #'wc-focus-prev-client)
;;    (define-key map (kbd "M-s") #'new-focus-next-client)
    (define-key map (kbd "M-i") #'wc-incr-master-ratio)
    (define-key map (kbd "M-d") #'wc-decr-master-ratio)
    (define-key map (kbd "M-m") #'wc-add-master)
    (define-key map (kbd "M-b") #'wc-remove-master)
    (define-key map (kbd "M-k") #'wc-kill-client)
    (define-key map (kbd "M-z") #'wc-zoom)
    (define-key map (kbd "M-v") #'wc-view)
    (define-key map (kbd "M-t") #'terminal)
    (define-key map (kbd "M-q") #'wc-quit)
    (define-key map (kbd "M-f") #'wc-toggle-floating)
    map
    ))

;; (define-minor-mode ewlc-mode
;;   "Enable keymap for ewlc."
;;   :keymap ewlc-keymap-prefix
;;   :global defun)

(defun start-wc ()
  "Start the wayland compositor."
  (setq *ewlc-server* (ewlc-start))
  (setq *ewlc-running* t)
  (setq *ewlc-thread* (make-thread
                       (lambda ()
                         (let ((count 0))
                           (while (and (< count 1000)  *ewlc-running*)
                             (ewlc-handle-keybindings *ewlc-server*)
                             (ewlc-handle-events *ewlc-server*)
                             ;; dispatch queued wayland events.
                             ;; FIXME: I think that there is a potential race condition here.
                             ;; At least the events can occur too late or out of sequence.
                             ;; I need non-blocking async event handling
                             (ewlc-display-dispatch *ewlc-server*)
                             (setq count (+ 1 count))
                             (sleep-for 0.01))
                           )
                         )
                       "loop-thread")))

(defun wm-loop ()
  "Dispatch wayland events."
  (ewlc-display-dispatch *ewlc-server*)
  (ewlc-handle-keybindings *ewlc-server*)
  (ewlc-handle-events *ewlc-server*))

(defun start-wc-new()
  "Start the wayland compositor."
  (setq *ewlc-server* (ewlc-start))
  (setq *ewlc-running* t)
  (ewlc-display-dispatch *ewlc-server*)
  (run-at-time 0 0.01 #'wm-loop)
  )

(defun ewlc-apply-keybinding (mod key)
  "Apply the keybings for MOD and KEY."
  (let ((command (lookup-key ewlc-prefix-map (kbd (concat mod key)))))
    (if (and command (fboundp command))
        (progn
          (funcall command)
          1)
      0)))

;;--------------------------------------------------------------------

(provide 'ewlc-wm)
;;; ewlc-wm.el ends here
