;;; ewlc-server.el --- description -*- lexical-binding: t; -*-
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

(require 'cl-lib)
(load "/home/edgar/Projects/ewlc/src/ewlc-log.el")
(require 'ewlc-log)

(defvar *ewlc* nil "The wayland compositor server.")

(cl-defstruct ewlc-client
  floating-p
  resize-p
  geom

)

(cl-defstruct ewlc-output
  output-ptr
  wlr-output
  box
  num-master
  master-ratio

)

(cl-defstruct ewlc
  server
  renderer
  seat

  output-list
  active-output
  output-layout
  output-geom

  ;; input devices
  keyboard-list
  repeat-rate
  repeat-delay

  ;; cursor
  cursor
  cursor-mgr
  cursor-mode
  sloppy-focus

  ;; clients
  client-list
  client-stack-list
  active-client
  grabbed-client
  grabbed-client-x
  grabbed-client-y

  root-color
)

;; ------------------------------------------------------------
;; output
;; ------------------------------------------------------------

(defun ewlc-set-output (old-output new-output))

(defun ewlc-get-output-at-point (cursor)
  "Get the output at the CURSOR."
  (ewlc--c/get-output-at-point (ewlc-output-layout *ewlc*) cursor))

(defun ewlc-visible-on-p (output client))

(defun ewlc-output= (output-1 output-2)
  "Compare OUTPUT-1 and OUTPUT-2."
  (ewlc-compare-outputs output-1 output-2))


(defun ewlc-set-next-output (direction)
  "Set the active output on the basis of the DIRECTION."
  (let* ((output-list (ewlc-output-list *ewlc*))
         (curr-output (ewlc-active-output *ewlc*))
         (len (length output-list))
         (pos (cl-position curr-output output-list :test 'ewlc-output=)))
    (setf (ewlc-active-output *ewlc*) (cond ((equal direction 'next)
                                             (if (< (+ pos 1) len)
                                                 (nth (+ pos 1) output-list)
                                               (nth 0 output-list)))
                                            ((equal direction 'prev)
                                             (if (= pos 0)
                                                 (nth (- len 1) output-list)
                                               (nth (- pos 1) output-list)))))))

(defun ewlc-arrange (output)
  "Tile the visible clients on the OUTPUT."
  (setf (ewlc-output-box output) (ewlc-output-layout-get-box (ewlc-output-layout *ewlc*)
                                                             (ewlc-output-wlr-output output)))
  (let* ((n 0)
         (i 0)
         (my 0)
         (ty 0)
         mw
         h
         (output-box (ewlc-output-box output))
         (output-x (wlr-box-x output-box))
         (output-y (wlr-box-y output-box))
         (output-width (wlr-box-width output-box))
         (output-height (wlr-box-height output-box)))
    (dolist (client (ewlc-client-list *ewlc*))
      (when (and (ewlc-visible-on-p client output)
                 (not (ewlc-client-floating-p client)))
        (cl-incf n))
      (when (> n 0)
        (if (> n (ewlc-output-num-master output))
            (setq mw (if (ewlc-output-num-master output)
                         (* output-width (ewlc-output-master-ratio output))
                       0))
          (setq mw (ewlc-box-width output)))
        (dolist (client (ewlc-client-list *ewlc*))
          (let* ((client-geom (ewlc-client-geom client))
                 (client-height (wlr-box-height client-geom)))
            (when (and (ewlc-visible-on-p client output)
                       (not (ewlc-client-floating-p client)))
              (if (< i (ewlc-output-num-master output))
                  (progn
                    (setq h (/ (- output-height my)
                               (- (min n (ewlc-output-num-master output)) i)))
                    (ewlc-resize client output-x (+ output-y my) mw h 0)
                    (setq my (+ my client-height)))
                (setq h (/ (- output-height ty) (- n i)))
                (ewlc-resize client (+ output-x mw) (+ output-y ty) (- output-width mw) h 0)
                (setq my (+ my client-height))))
            (incf i)))))))

;; handlers ---------------------------------------------------

(defun ewlc-backout-new-output-handler (wlr-output)
  "This event is raised when new WLR-OUTPUT (a display) becomes available."
  ;; Set the output's preferred mode.
  (wlr-output-set-mode wlr-output (wlr-output-preferred-mode wlr-output))
  (let* ((output-ptr (ewlc-make-output-ptr wlr-output))
         (output (make-ewlc-output :ptr output-ptr
                                   :wlr-output wlr-output
                                   :master-ratio 0.5
                                   :master-num 1
                                   :scale 2)))
    (wlr-output-set-scale wlr-output (ewlc-output-scale output))
    (wlr-xcursor-manager-load (ewlc-cursor-mgr *ewlc*) (ewlc-output-scale output))
    (wlr-output-set-transform wlr-output)
    (ewlc-output-set-event-listeners output-ptr wlr-output)
    (push output (ewlc-output-list *ewlc*))
    (wlr-output-enable wlr-output 1)
    (when (wlr-output-commit wlr-output)
      ;; The add-auto function arranges outputs from left-to-right in handled order.
      (wlr-output-layout-add-auto (ewlc-output-layout *ewlc*) wlr-output)
      (setf (ewlc-output-geom *ewlc*) (wlr-output-layout-get-box
                                       (ewlc-output-layout *ewlc*))))))

(defun ewlc-output-frame-handler (output)
  "OUTPUT frame handler.
Called every time the output is ready to display a frame, generally at
the output's refresh rate."
  (let ((now (current-time))
        (render t)
        (renderer (ewlc-renderer *ewlc*))
        (wlr-output (ewlc-output-wlr-output output)))
    (dolist (client (ewlc-client-stack-list *ewlc*))
      (when (ewlc-client-resize-p client)
        (wlr-surface-send-frame-done (ewlc-get-surface client) now)
        ;; do not render if any xdg clients have an outstanding resize
        (setq render nil)))
    (when (wlr-output-attach-render wlr-output)
      (when render
        (wlr-renderer-begin renderer wlr-output))
        (wlr-renderer-clear renderer (ewlc-root-color *ewlc*))
        (ewlc-render-clients wlr-output now)
        (ewlc-render-independents wlr-output now)
        ;; This function is a no-op when hardware cursors are in use.
        (wlr-output-render-software-cursors wlr-output)
        (wlr-renderer-end renderer))
      (wlr-output-commit wlr-output)))

(defun ewlc-output-destroy-handler (output)
  "Handler for the destroy OUTPUT signal."
  (setf (ewlc-output-list *ewlc*) (cl-remove output (ewlc-output-list *ewlc*)
                                             :test 'ewlc-output=))
  (ewlc-free (ewlr-output-ptr output)))

;; ------------------------------------------------------------
;; client
;; ------------------------------------------------------------

(defun ewlc-set-floating (client flag))


(defun ewlc-render-clients (wlr-output time))
(defun ewlc-render-independents (wlr-output time))

(defun ewlc-resize (client x y width height flag))

(defun wlr-get-client-at-point (cursor))

(defun ewlc-get-surface (client))

(defun ewlc-get-independent-at-point (cursor))

(defun ewlc-get-active-client ())

(defun ewlc-client-type (client)
  (ewlc--c/client-type client))

(defun ewlc-focus-client (curr-client new-client flag))

(defun ewlc-client-xwayland-surface (client))

(defun ewlc-client-xdg-surface (client))

(defun ewlc-xdg-surface-at (surface x y))

(defun ewlc-xwayland-surface-at (surface x y))

;; ------------------------------------------------------------
;; pointer
;; ------------------------------------------------------------

;; bound to a key/button command
(defun action-move-resize (cursor-mode)
  (setf (ewlc-grabbed-client *ewlc*) (ewlc-get-client-at-point (ewlc-cursor *ewlc*)))
  (let ((client (ewlc-grabbed-client *ewlc*)))
    (when client
      (setf (ewlc-cursor-mode *ewlc*) cursor-mode)
      ;; float the window and grab it for move/resize
      (ewlc-set-floating client t)
      (let ((client-x (ewlc-client-x client))
            (client-y (ewlc-client-y client))
            (client-h (ewlc-client-height client))
            (client-w (ewlc-client-width client))
            (cursor-x (ewlc-cursor-x (ewlc-cursor *ewlc*)))
            (cursor-y (ewlc-cursor-y (ewlc-cursor *ewlc*))))
        (cond ((equal cursor-mode 'cursor-move)
               (setf (ewlc-grabbed-client-x *ewlc*) (- cursor-x client-x))
               (setf (ewlc-grabbed-client-y *ewlc*) (- cursor-y client-y))
               (ewlc-set-cursor-image (ewlc-cursor-mgr *ewlc*) "fleur"
                                      (ewlc-cursor *ewlc*)))
              ((equal cursor-mode 'cursor-resize)
               ;; FIXME: does not work for X windows
               (ewlc--c/cursor-warp-closest (ewlc-cursor *ewlc*)
                                            (+ client-x client-w)
                                            (+ client-y client-h))
               (ewlc-set-cursor-image (ewlc-cursor-mgr *ewlc*) "bottom_right_corner"
                                      (ewlc-cursor *ewlc*))))))))

(defun ewlc-cursor-x (cursor)
  (ewlc--c/cursor-x cursor))

(defun ewlc-cursor-y (cursor)
  (ewlc--c/cursor-y cursor))

(defun ewlc-client-x (cursor)
  (ewlc--c/client-x cursor))

(defun ewlc-client-y (cursor)
  (ewlc--c/client-y cursor))

(defun ewlc-xwayland-x (cursor)
  (ewlc--c/xwayland-x cursor))

(defun ewlc-wayland-y (cursor)
  (ewlc--c/xwayland-y cursor))

(defun ewlc-client-width (cursor)
  (ewlc--c/client-width cursor))

(defun ewlc-client-height (cursor)
  (ewlc--c/client-height cursor))

(defun ewlc-set-cursor-image (cursor-mgr image-text cursor)
  (ewlc--c/set-cursor-image cursor-mgr image-text cursor))

(defun ewlc-apply-button-action (seat event)
  (ewlc--c/apply-button-action seat event))

(defun ewlc-seat-pointer-notify-button (seat event)
  "Notify client of button press EVENT for the SEAT."
  (ewlc--c/seat-pointer-notify-button seat event))

(defun ewlc-free-event (event)
  "Free the memory (in C) allocated for EVENT."
  (ewlc--c/free-event event))

(defun ewlc-create-pointer (device)
  "Create pointer DEVICE."
  ;; FIXME: this looks wrong - should assign a ewlc-pointer?
  (ewlc--c/create-pointer device (ewlc-pointer *ewlc*)))

(defun ewlc-get-button-press-state (event)
  "Get the button state (e.g. 'pressed, 'released) of a pointer button EVENT."
  (ewlc-c/get-button-press-state event))

(defun ewlc-cursor-set-surface (cursor seat event)
  "Tell the SEAT CURSOR to use the provided EVENT surface."
  (ewlc-c/cursor-set-surface cursor seat event))

(defun ewlc-pointer-focus (client surface x y time))

(defun ewlc-get-event-relative-motion-time (event))

(defun ewlc-get-event-absolute-motion-time (event))

(defun ewlc-resize (client x y width height interact-p))

(defun ewlc-motion-resize (time)
  "Contol the motion/resize of a client as part of a cursor motion at TIME."
  (when (ewlc-sloppy-focus *ewlc*)
    ;; Update active output as part of a client drag.
    ;; FIXME: need to fix ewlc-get-output-at-point
    (setf (ewlc-active-output *ewlc*) (ewlc-get-output-at-point (ewlc-cursor *ewlc*))))
  (cond ((equal (ewlc-cursor-mode *ewlc*) 'cursor-move)
         (ewlc-resize (ewlc-grabbed-client *ewlc*)
                      (- (ewlc-cursor-x (ewlc-cursor *ewlc*))
                         (ewlc-client-x (ewlc-grabbed-client *ewlc*)))
                      (- (ewlc-cursor-y (ewlc-cursor *ewlc*))
                         (ewlc-client-y (ewlc-grabbed-client *ewlc*)))
                      (ewlc-client-width (ewlc-grabbed-client *ewlc*))
                      (ewlc-client-height (ewlc-grabbed-client *ewlc*))
                      t))
        ((equal (ewlc-cursor-mode *ewlc*) 'cursor-resize)
         (ewlc-resize (ewlc-grabbed-client *ewlc*)
                      (ewlc-client-x (ewlc-grabbed-client *ewlc*))
                      (ewlc-client-y (ewlc-grabbed-client *ewlc*))
                      (- (ewlc-cursor-x (ewlc-cursor *ewlc*))
                         (ewlc-client-x (ewlc-grabbed-client *ewlc*)))
                      (- (ewlc-cursor-y (ewlc-cursor *ewlc*))
                         (ewlc-client-y (ewlc-grabbed-client *ewlc*)))
                      t))
        (t
         (let* ((client (ewlc-get-independent-at-point (ewlc-cursor *ewlc*)))
                surface-x-y)
           (if client
               (setq surface-x-y (ewlc-surface-xwayland-at
                                  (ewlc-client-xwayland-surface client)
                                  (- (ewlc-cursor-x (ewlc-cursor *ewlc*))
                                     (ewlc-xwayland-x client)
                                     (ewlc-border-width *ewlc*))
                                  (- (ewlc-cursor-y (ewlc-cursor *ewlc*))
                                     (ewlc-xwayland-y client)
                                     (ewlc-border-width *ewlc*))))
             (setq client (ewlc-get-client-at-point (ewlc-cursor *ewlc*)))
             (if (and client (equal (ewl-client-type client) 'xdg-shell))
                 (setq surface-x-y (ewlc-xdg-surface-at
                                    (ewlc-client-xdg-surface client)
                                    (- (ewlc-cursor-x (ewlc-cursor *ewlc*))
                                       (ewlc-client-x client)
                                       (ewlc-border-width *ewlc*))
                                    (- (ewlc-cursor-y (ewlc-cursor *ewlc*))
                                       (ewlc-client-y client)
                                       (ewlc-border-width *ewlc*))))
               (setq surface-x-y (ewlc-surface-xwayland-at
                                  (ewlc-client-xwayland-surface client)
                                  (- (ewlc-cursor-x (ewlc-cursor *ewlc*))
                                     (ewlc-client-x client)
                                     (ewlc-border-width *ewlc*))
                                  (- (ewlc-cursor-y (ewlc-cursor *ewlc*))
                                     (ewlc-client-y client)
                                     (ewlc-border-width *ewlc*)))))
             (unless surface-x-y
               ;; If there is no client surface under the cursor, set image to default.
               (ewlc-set-cursor-image (ewlc-cursor *ewlc*)
                                      "left_ptr"
                                      (ewlc-cursor-mgr *ewlc*)))
             (cl-destructuring-bind
                 (surface x y) surface-x-y
               (ewlc-pointer-focus client surface x y time)))))))

;; handlers

(defun ewlc-cursor-motion-absolute-handler (event)
  "Handle EVENT forwarded by the cursor when a pointer emits a _absolute_ motion."
  (wlr-cursor-warp-absolute (ewlc-cursor *ewlc*) event)
  (ewlc-motion-resize (ewlc-event-absolute-motion-time event))
  (ewlc-free-event event))

(defun ewlc-cursor-motion-handler (event)
  "Handle EVENT forwarded by the cursor when a pointer emits a _relative_ motion."
  (wlr-cursor-move (ewlc-cursor *ewlc*) event)
  (ewlc-motion-resize (ewlc-event-relative-motion-time event))
  (ewlc-free-event event))

(defun elwc-cursor-frame-handler (event)
  "Handle EVENT forwarded by the cursor when pointer emits a frame event.
Frame events are sent after regular pointer events to group multiple events together."
  (wlr-seat-pointer-notify-frame (ewlc-seat *ewlc*))
  ;; TODO: does this event need to be freed?
  (ewlc-free event))

(defun ewlc-frame-handler (event)
  "Handle the EVENT raised when the cursor emits a 'frame event'.
Frame evens are raised."
  ;; FIXME: this is wrong
  (ewlc--c/seat-set-primary-selection (ewlc-seat *ewlc*) event)
  (ewlc-free event))

(defun ewlc-seat-request-set-primary-selection-handler (event)
  "Handle the EVENT raised when client requests to set the selection.
Usually occurs when the user copies something."
  (wlr-seat-set-primary-selection (ewlc-seat *ewlc*) event)
  (ewlc-free event))

(defun ewlc-seat-request-set-selection-handler (event)
  "Handle the EVENT raised when client requests to set the selection.
Usually occurs when the user copies something."
  (wlr-seat-set-selection (ewlc-seat *ewlc*) event)
  (ewlc-free event))

(defun ewlc-seat-pointer-request-set-cursor-handler (event)
  "Handle EVENT raised by the seat when a client provides a cursor image."
  (unless (equal (ewlc-cursor-mode *ewlc*) 'cursor-normal)
    (wlr-cursor-set-surface (ewlc-cursor *ewlc*) (ewlc-seat *ewlc*) event))
  (ewlc-free event))

(defun ewlc-pointer-axis-handler (event)
  "Handle an axis EVENT forwarded by the pointer (e.g. scroll wheel movement)."
  (wlr-seat-pointer-notify-axis (ewlc-seat *ewlc*) event)
  (ewlc-free event))

(defun ewlc-pointer-button-handler (event)
  "Handle an button EVENT forwarded by the pointer."
  (let ((button-state (wlr-get-button-press-state event))
        (handled nil))
    (cond ((equal button-state 'pressed)
           ;; change focus if the button is pressed over a client,
           ;; then start the button action
           (let ((new-client (wlr-get-client-at-point (ewlc-cursor *ewlc*)))
                 (curr-client (ewlc-get-active-client)))
             (ewlc-focus-client curr-client new-client t)
             (setq handled (ewlc-apply-button-action (ewlc-seat *ewlc*) event))))
          ((equal button-state 'released)
           (unless (equal (ewlc-cursor-mode *ewlc*) 'cursor-normal)
             ;; exit move/resize and reset the cursor
             (wlr-xcursor-manager-set-cursor-image (ewlc-cursor-mgr *ewlc*) "left_ptr"
                                                   (ewlc-cursor *ewlc*))
             (setf (ewlc-cursor-mode *ewlc*) 'cursor-normal)
             (setf (ewlc-active-output *ewlc*) (ewlc-get-output-at-point
                                                (ewlc-cursor *ewlc*)))
             (ewlc-set-output (ewlc-grabbed-client *ewlc*) (ewlc-active-output *ewlc*))
             (setq handled t)))
          (t
           (log-message (format "other event: %s" event))))
    (unless handled
      ;; event was not handled by the compositor - notify client with pointer focus
      (wlr-seat-pointer-notify-button (ewlc-seat *ewlc*) event))
    (ewlc-free event)))

;; ------------------------------------------------------------
;; keyboard
;; ------------------------------------------------------------

(defun ewlc-create-keyboard (device)
  "Create keyboard DEVICE."
  (let ((keyboard (ewlc-create-keyboard device
                                        (ewlc-seat *ewlc*)
                                        (ewlc-repeat-rate *ewlc*)
                                        (ewlc-repeat-delay *ewlc*))))
    (setf (ewlc-keyboard-list *ewlc*) (push keyboard (ewlc-keyboard-list *ewlc*)))))


(defun ewlc-keyboard-destroy-handler (keyboard)
  "Destroy KEYBOARD."
  (setf (ewlc-keyboard-list *ewlc*) (cl-remove keyboard
                                               (ewlc-keyboard-list *ewlc*)
                                               :test 'keyboard=))
  (ewlc-free keyboard))

(defun ewlc-keyboard-modifiers-handler (keyboard)
  "Handle modifier key press on KEYBOARD. Communicate press to the client."
  (let ((seat (ewlc-seat *ewlc*)))
    (wlr-seat-set-keyboard seat keyboard)
    (wlr-seat-keyboard-notify-modifiers seat keyboard)))

;; ------------------------------------------------------------
;; input device
;; ------------------------------------------------------------

(defun ewlc-new-input-handler (device)
  "Handle a new input event for DEVICE."
  (log-message (format "device: %s" device))
  (let ((device-type (wlr-get-device-type device)))
    (cond ((equal device-type 'keyboard)
           (ewlc-create-keyboard device))
          ((equal device-type 'pointer)
           (ewlc-create-pointer device))
          (t
           (log-message (format "other device: %s" device)))))
  (wlr-set-seat-capabilites (ewlc-seat *ewlc*) (not (eq (ewlc-keyboard-list *ewlc*) nil))))

;; ------------------------------------------------------------

(provide 'ewlc-server)
;;; ewlc-server.el ends here
