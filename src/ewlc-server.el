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

(module-load "/home/edgar/Projects/ewlc/lib/ewlc.so")

(require 'cl-lib)
(load "/home/edgar/Projects/ewlc/src/ewlc-log.el")
(require 'ewlc-log)

(defvar *ewlc* nil "The wayland compositor server.")

(cl-defstruct ewlc-keyboard
  ptr
  device
  )

(cl-defstruct ewlc-client
  ptr
  floating-p
  resize-p
  geom
  xdg-surface
  xwayland-surface
  type
  output
  title
  app-id
  )

(cl-defstruct ewlc-output
  ptr
  wlr-output
  box
  num-master
  master-ratio
  scale
  )

(cl-defstruct ewlc
  ptr
  renderer
  seat
  display
  backend
  compositor
  xdg-deco-mgr
  xdg-shell
  xwayland
  netatom

  ;; outputs
  output-list
  active-output
  output-layout
  output-geom

  ;; input devices
  modifier-key
  keyboard-list
  repeat-rate
  repeat-delay

  ;; cursor
  cursor
  cursor-mgr
  cursor-mode
  sloppy-focus

  ;; buttons
  ;; FIXME: get buttons working

  ;; clients
  client-list
  client-stack-list
  client-focus-list
  independent-list
  active-client
  grabbed-client
  grabbed-client-x
  grabbed-client-y

  ;; colors
  root-color
  border-color
  focus-color
  border-width

  startup-pid
  running-p
  )

;; ------------------------------------------------------------
;; server
;; ------------------------------------------------------------

(defun ewlc-update-window-type (client)
  "Update the window type for xwayland CLIENTs."
  (let ((xwayland-surface (ewlc-client-xwayland-surface client)))
    (when (ewlc-floating-type-p xwayland-surface)
      (setf (ewlc-client-floating-p client) t))))

(defun ewlc-display-dispatch ()
  "Dispatch the event loop for the display."
  (let ((event-loop (wl-display-get-event-loop (ewlc-display *ewlc*))))
    (wl-display-flush-clients (ewlc-display *ewlc*))
    (wl-event-loop event-loop -1)))

(defun ewlc-cleanup ()
  "Cleanup prior to exiting."
  (when (not (eql (ewlc-startup-pid *ewlc*) -1))
    (ewlc-kill (ewlc-startup-pid *ewlc*)))
  (wlr-xwayland-destroy (ewlc-xwayland *ewlc*))
  (wl-display-destroy-clients (ewlc-display *ewlc*))
  (wl-display-destroy (ewlc-display *ewlc*))
  (wlr-xcursor-manager-destroy (ewlc-xcursor-mgr *ewlc*))
  (wlr-cursor-destroy (ewlc-cursor *ewlc*))
  (wlr-output-layout-destroy (ewlc-output-layout *ewlc*))
  (ewlc-free (ewlc-ptr *ewlc*)))

(defun ewlc-start (shell-command)
  "Start the compositor with the startup application SHELL-COMMAND."
  (when (not (getenv "XDR_RUNTIME_DIR"))
    (error "XDR_RUNTIME_DIR must be set"))
  (let* ((server-ptr (ewlc-make-server-ptr))
         (display (wl-display-create))
         (backend (let ((b (wlr-backend-autocreate display)))
                    (if b
                        b
                      (error "Could not create backend"))))
         (renderer (wlr-backend-get-renderer backend))
         (compositor (wlr-compositor-create display renderer)))
    ;; clean up child processes immediately
    (ewlc-set-sigchld 0)
    (wlr-renderer-init-wl-display renderer display)
    (setq *ewlc* (make-ewlc :ptr server-ptr
                            :display display
                            :backend backend
                            :renderer renderer
                            :compositor compositor
                            :seat (wlr-seat-create display "seat0")
                            :cursor (wlr-cursor-create)
                            :cursor-mgr (wlr-xcursor-manager-create 24)
                            :output-layout (wlr-output-layout-create)
                            :xdg-shell (wlr-xdg-shell-create display)
                            :xwayland  (wlr-xwayland-create display compositor)
                            :xdg-deco-mgr (wlr-xdg-decoration-manager-v1-create display)
                            :output-list nil
                            :keyboard-list nil
                            :modifier-key "M"
                            :client-list nil
                            :client-focus-list nil
                            :client-stack-list nil
                            :independent-list nil
                            :root-color '(0.3 0.3 0.3 1.0)
                            :border-color '(0.5 0.5 0.5 1.0)
                            :focus-color '(1.0 0.0 0.0 1.0)))
    (wlr-export-dmabuf-manager-v1-create display)
    (wlr-screencopy-manager-v1-create display)
    (wlr-data-device-manager-create display)
    (wlr-gamma-control-manager-v1-create display)
    (wlr-primary-selection-v1-device-manager-create display)
    (wlr-viewporter-create display)
    (wlr-xdg-output-manager-v1-create display (ewlc-output-layout *ewlc*))
    (wlr-cursor-attach-output-layout (ewlc-cursor *ewlc*)(ewlc-output-layout *ewlc*))

    ;; create the listeners (defined in C code)
    ;; TODO: rename to 'register-listeners'
    (ewlc-backend-set-listeners (ewlc-ptr *ewlc*) (ewlc-backend *ewlc*))
    (ewlc-xdg-shell-set-listeners (ewlc-ptr *ewlc*) (ewlc-xdg-shell *ewlc*))
    (ewlc-xdg-deco-set-listeners (ewlc-ptr *ewlc*) (ewlc-xdg-deco-mgr *ewlc*))
    (ewlc-cursor-set-listeners (ewlc-ptr *ewlc*) (ewlc-cursor *ewlc*))
    (ewlc-seat-set-listeners (ewlc-ptr *ewlc*) (ewlc-seat *ewlc*))
    (ewlc-xwayland-set-listeners (ewlc-ptr *ewlc*) (ewlc-seat *ewlc*))
    (setenv "DISPLAY" (wlr-xwayland-display-name (ewlc-xwayland *ewlc*)))
    (let ((socket (wl-display-add-socket-auto display)))
      (when (not socket)
        (error "Could not create socket"))
      (when (not (wlr-backend-start backend))
        (error "Could not start backend"))
      ;; Handle pending events for all new inputs/ouputs.
      (ewlc-handle-events)
      ;; Now that outputs are initialized, choose initial active-output based on
      ;; cursor position, and set default cursor image.
      (let* ((cursor (ewlc-cursor *ewlc*))
             (cursor-mgr (ewlc-cursor-mgr *ewlc*))
             (x (wlr-cursor-x cursor))
             (y (wlr-cursor-y cursor)))
        (setf (ewlc-active-output *ewlc*) (ewlc-get-output-at-point cursor))
        (wlr-cursor-warp-closest cursor nil x y)
        (wlr-xcursor-manager-set-cursor-image cursor-mgr "left_ptr" cursor))
      (setenv "WAYLAND_DISPLAY" socket 1)
      (start-process-shell-command "" nil shell-command))))

(defun ewlc-keyboard-from-listener-container (keyboard-ptr)
  "Get the EWLC-KEYBOARD that has KEYBOARD-PTR in it's keyboard-ptr slot."
  (cl-dolist (ewlc-keyboard (ewlc-keyboard-list *ewlc*))
    (when (equal keyboard-ptr (ewlc-keyboard-ptr ewlc-keyboard))
      (cl-return ewlc-keyboard))))

(defun ewlc-output-from-listener-container (output-ptr)
  "Get the EWLC-OUTPUT that has OUTPUT-PTR in it's output-ptr slot."
  (cl-dolist (ewlc-output (ewlc-output-list *ewlc*))
    (when (equal output-ptr (ewlc-output-ptr ewlc-output))
      (cl-return ewlc-output))))

(defun ewlc-client-from-listener-container (client-ptr)
  "Get the EWLC-CLIENT that has CLIENT-PTR in it's client-ptr slot."
  (cl-dolist (ewlc-client (ewlc-client-list *ewlc*))
    (when (equal client-ptr (ewlc-client-ptr ewlc-client))
      (cl-return ewlc-client))))

(defun ewlc-handle-events ()
  "Handle all the events."
  ;; FIXME: need to get from pointer to defstruct that contains the pointer
  ;; i.e. find the pointer in the list of containers and hence the defstruct.
  (while (ewlc-pending-events-p (ewlc-ptr *ewlc*))
    (cl-destructuring-bind (event-container event-data event-type) (ewlc-get-event (ewlc-ptr *ewlc*))
      (pcase event-type
        ('ewlc-pointer-axis
         (let ((wlr-event-pointer-axis event-data))
           (ewlc-pointer-axis-handler wlr-event-pointer-axis)))
        ('ewlc-pointer-button
         (let ((wlr-event-pointer-button event-data))
           (ewlc-pointer-button-handler wlr-event-pointer-button)))
        ('ewlc-cursor-motion
         (let ((wlr-event-pointer-motion event-data))
           (ewlc-cursor-motion-handler wlr-event-pointer-motion)))
        ('ewlc-cursor-frame
         (ewlc-cursor-frame-handler))
        ('ewlc-cursor-motion-absolute
         (let ((wlr-event-pointer-motion-absolute event-data))
           (ewlc-cursor-motion-absolute-handler wlr-event-pointer-motion-absolute)))

        ('ewlc-seat-request-set-cursor
         (let ((wlr-seat-pointer-request-set-cursor-event event-data))
           (ewlc-seat-pointer-request-set-cursor-handler wlr-seat-pointer-request-set-cursor-event)))
        ('ewlc-seat-request-set-primary-selection
         (let ((wlr-seat-request-set-primary-selection-event event-data))
           (ewlc-seat-request-set-primary-selection-handler
            wlr-seat-request-set-primary-selection-event)))
        ('ewlc-seat-request-set-selection
         (let ((wlr-seat-request-set-selection-event event-data))
           (ewlc-seat-request-set-selection-handler wlr-seat-request-set-selection-event)))

        ('ewlc-keyboard-destroy
         (let* ((keyboard-ptr event-container)
                (ewlc-keyboard (ewlc-keyboard-from-listener-container keyboard-ptr)))
           (ewlc-keyboard-destroy-handler ewlc-keyboard)))
        ('ewlc-keyboard-key
         (let* ((keyboard-ptr event-container)
                (ewlc-keyboard (ewlc-keyboard-from-listener-container keyboard-ptr))
                (wlr-event-keyboard-key event-data))
           (ewlc-keyboard-key-handler ewlc-keyboard wlr-event-keyboard-key)))
        ('ewlc-keyboard-modifiers
         (let* ((keyboard-ptr event-container)
                (ewlc-keyboard (ewlc-keyboard-from-listener-container keyboard-ptr)))
           (ewlc-keyboard-modifiers-handler ewlc-keyboard)))

        ('ewlc-backend-new-input
         (let ((wlr-input-device event-data))
           (ewlc-backend-new-input-handler wlr-input-device)))
        ('ewlc-backend-new-output
         (let ((wlr-output event-data))
           (ewlc-backend-new-output-handler wlr-output)))

        ;; The event-data for decorations is a wlr_xdg_toplevel_decoration_v1 pointer.
        ('ewlc-xdg-deco-mgr-new-toplevel-decoration
         (let ((wlr-xdg-toplevel-decoration-v1 event-data))
           (ewlc-xdg-deco-mgr-new-toplevel-decoration-handler wlr-xdg-toplevel-decoration-v1)))
        ('ewlc-deco-destroy
         (let ((wlr-xdg-toplevel-decoration-v1 event-data))
           (ewlc-deco-destroy-handler wlr-xdg-toplevel-decoration-v1)))
        ('ewlc-deco-request-mode
         (let ((wlr-xdg-toplevel-decoration-v1 event-data))
           (ewlc-deco-request-mode-handler wlr-xdg-toplevel-decoration-v1)))

        ('ewlc-output-destroy
         (let* ((output-ptr event-container)
                (ewlc-output (ewlc-output-from-listener-container output-ptr)))
           (ewlc-output-destroy-handler ewlc-output)))
        ('ewlc-output-frame
         (let* ((output-ptr event-container)
                (ewlc-output (ewlc-output-from-listener-container output-ptr)))
           (ewlc-output-frame-handler ewlc-output)))

        ('ewlc-new-xdg-shell-surface
         (let ((wlr-xdg-surface event-data))
           (ewlc-new-xdg-shell-surface-handler wlr-xdg-surface)))
        ('ewlc-new-xwayland-surface
         (let ((wlr-xwayland-surface event-data))
           (ewlc-new-xwayland-surface-handler wlr-xwayland-surface)))

        ('ewlc-xdg-surface-commit
         (let* ((client-ptr event-container)
                (ewlc-client (ewlc-client-from-listener-container client-ptr)))
           (ewlc-xdg-surface-commit-handler ewlc-client)))
        ('ewlc-surface-map
         (let* ((client-ptr event-container)
                (ewlc-client (ewlc-client-from-listener-container client-ptr)))
           (ewlc-surface-map-handler ewlc-client)))
        ('ewlc-surface-unmap
         (let* ((client-ptr event-container)
                (ewlc-client (ewlc-client-from-listener-container client-ptr)))
           (ewlc-surface-unmap-handler ewlc-client)))
        ('ewlc-surface-destroy
         (let* ((client-ptr event-container)
                (ewlc-client (ewlc-client-from-listener-container client-ptr)))
           (ewlc-surface-destroy-handler ewlc-client)))

        ('ewlc-xwayland-ready
         (ewlc-xwayland-ready-handler))
        ('ewlc-xwayland-surface-request-activate
         (let* ((client-ptr event-container)
                (ewlc-client (ewlc-client-from-listener-container client-ptr)))
           (ewlc-xwayland-surface-request-activate-handler ewlc-client)))
        (_
         (error "Event not handled"))))
    ;; TODO: free the event point inside this function
    (ewlc-remove-event (ewlc-ptr *ewlc*))))


;; handlers ---------------------------------------------------

(defun ewlc-xwayland-ready-handler ()
  "Xwayland event handler."
  (let* ((xc (ewlc-xcb-connect (ewlc-xwayland *ewlc*)))
         (err (ewlc-xcb-connection-has-error xc)))
    (when err
      (error "The call from xcb_connect to X server failed"))
    (setf (ewlc-netatom *ewlc*) (ewlc-set-atoms xc))
    (wlr-xwayland-set-seat (ewlc-xwayland *ewlc*) (ewlc-seat *ewlc*))
    (ewlc-xcb-disconnect xc)))

(defun ewlc-xdg-deco-mgr-new-toplevel-decoration-handler (wlr-xdg-toplevel-decoration-v1)
  "Handler for toplevel decoration WLR-XDG-TOPLEVEL-DECORATION-V1."
  (let* ((deco-ptr (ewlc-make-deco-ptr wlr-xdg-toplevel-decoration-v1 (ewlc-ptr *ewlc*))))
    (ewlc-set-xdg-deco-mgr-toplevel-listeners deco-ptr wlr-xdg-toplevel-decoration-v1)
    (ewlc-deco-request-mode-handler wlr-xdg-toplevel-decoration-v1)))

(defun ewlc-deco-request-mode-handler (wlr-xdg-toplevel-decoration-v1)
  "Handle WLR-XDG-TOPLEVEL-DECORATION-V1 set mode event."
  (wlr-xdg-toplevel-decoration-v1-set-mode wlr-xdg-toplevel-decoration-v1))

(defun ewlc-deco-destroy-handler (wlr-xdg-toplevel-decoration-v1)
  "Handle WLR-XDG-TOPLEVEL-DECORATION-V1 destory event."
  ;; FIXME: need to access the wlr_deco->data to get pointer to free
  (ewlc-free wlr-xdg-toplevel-decoration-v1))

;; ------------------------------------------------------------
;; output
;; ------------------------------------------------------------


(defun ewlc-get-output-at-point (cursor)
  "Get the wlr-output at the CURSOR."
  (let* ((output-layout (ewlc-output-layout *ewlc*)))
    ;; can return nil
    (wlr-output-layout-output-at output-layout cursor)))

(defun ewlc-visible-on-p (ewlc-output ewlc-client)
  "Return t when EWLC-CLIENT is visible on EWLC-OUTPUT."
  (equal (ewlc-client-output ewlc-client) ewlc-output))

(defun ewlc-output= (ewlc-output-1 ewlc-output-2)
  "Compare EWLC-OUTPUT-1 and EWLC-OUTPUT-2."
  ;; TODO: does this work?
  (equal (ewlc-output-ptr ewlc-output-1) (ewlc-output-ptr ewlc-output-2)))

(defun ewlc-set-next-output (direction)
  "Set the active output on the basis of the DIRECTION."
  (let* ((output-list (ewlc-output-list *ewlc*))
         (curr-ewlc-output (ewlc-active-output *ewlc*))
         (len (length output-list))
         (pos (cl-position curr-ewlc-output output-list :test 'ewlc-output=)))
    (setf (ewlc-active-output *ewlc*) (cond ((equal direction 'next)
                                             (if (< (+ pos 1) len)
                                                 (nth (+ pos 1) output-list)
                                               (nth 0 output-list)))
                                            ((equal direction 'prev)
                                             (if (= pos 0)
                                                 (nth (- len 1) output-list)
                                               (nth (- pos 1) output-list)))))))

(defun ewlc-arrange (ewlc-output)
  "Tile the visible clients on the EWLC-OUTPUT."
  (setf (ewlc-output-box output) (ewlc-output-layout-get-box (ewlc-output-layout *ewlc*)
                                                             (ewlc-output-wlr-output ewlc-output)))
  (let* ((n 0)
         (i 0)
         (my 0)
         (ty 0)
         mw
         h
         (output-box (ewlc-output-box ewlc-output))
         (ox (wlr-box-x output-box))
         (oy (wlr-box-y output-box))
         (ow (wlr-box-width output-box))
         (oh (wlr-box-height output-box)))
    (dolist (client (ewlc-client-list *ewlc*))
      (when (and (ewlc-visible-on-p client ewlc-output)
                 (not (ewlc-client-floating-p client)))
        (cl-incf n))
      (when (> n 0)
        (if (> n (ewlc-output-num-master ewlc-output))
            (setq mw (if (ewlc-output-num-master ewlc-output)
                         (* ow (ewlc-output-master-ratio ewlc-output))
                       0))
          (setq mw (ewlc-box-width ewlc-output)))
        (dolist (client (ewlc-client-list *ewlc*))
          (let* ((client-geom (ewlc-client-geom client))
                 (ch (wlr-box-height client-geom)))
            (when (and (ewlc-visible-on-p client ewlc-output)
                       (not (ewlc-client-floating-p client)))
              (if (< i (ewlc-output-num-master ewlc-output))
                  (progn
                    (setq h (/ (- oh my)
                               (- (min n (ewlc-output-num-master ewlc-output)) i)))
                    (ewlc-resize client ox (+ oy my) mw h 0)
                    (setq my (+ my ch)))
                (setq h (/ (- oh ty) (- n i)))
                (ewlc-resize client (+ ox mw) (+ oy ty) (- ow mw) h 0)
                (setq my (+ my ch))))
            (incf i)))))))

;; handlers ---------------------------------------------------

(defun ewlc-backend-new-output-handler (wlr-output)
  "This event is raised when new WLR-OUTPUT (a display) becomes available."
  ;; Set the output's preferred mode.
  (wlr-output-set-mode wlr-output (wlr-output-preferred-mode wlr-output))
  (let* ((output-ptr (ewlc-make-output-ptr wlr-output))
         (output (make-ewlc-output :ptr output-ptr
                                   :wlr-output wlr-output
                                   :master-ratio 0.5
                                   :num-master 1
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
        (wlr-surface-send-frame-done (ewlc-client-wlr-surface client) now)
        ;; do not render if any xdg clients have an outstanding resize
        (setq render nil)))
    (when (wlr-output-attach-render wlr-output)
      (when render
        (wlr-renderer-begin renderer wlr-output))
      (wlr-renderer-clear renderer (ewlc-root-color *ewlc*))
      (ewlc-render-clients output now)
      (ewlc-render-independents wlr-output now)
      ;; This function is a no-op when hardware cursors are in use.
      (wlr-output-render-software-cursors wlr-output)
      (wlr-renderer-end renderer))
    (wlr-output-commit wlr-output)))

(defun ewlc-output-destroy-handler (ewlc-output)
  "Handler for the destroy EWLC-OUTPUT signal."
  (setf (ewlc-output-list *ewlc*) (cl-remove ewlc-output (ewlc-output-list *ewlc*)
                                             :test 'ewlc-output=))
  (ewlc-free (ewlr-output-ptr ewlc-output)))

;; ------------------------------------------------------------
;; client
;; ------------------------------------------------------------

(defun ewlc-client= (client-a client-b)
  "Return t if CLIENT-A and CLIENT-B are equal, else return nil."
  ;; TODO: check that this works
  (equal (ewlc-client-ptr client-a) (ewlc-client-ptr client-b)))

(defun ewlc-apply-title (client output)
  "Apply title to CLIENT on OUTPUT."
  (if (not (equal (ewlc-client-type client) 'xdg-shell))
      (progn
        (ewlc-update-window-type client)
        (setf (ewlc-client-app-id client)
              (wlr-xwayland-get-surface-class (ewlc-client-xwayland-surface client)))
        (setf (ewlc-client-title client)
              (wlr-xwayland-get-surface-title (ewlc-client-xwayland-surface client))))
    (setf (ewlc-client-app-id client) (wlr-xdg-surface-get-app-id (ewlc-client-xdg-surface client)))
    (setf (ewlc-client-title client) (wlr-xdg-surface-get-title (ewlc-client-xdg-surface client))))
  (when (not (ewlc-client-app-id client))
    (setf (ewlc-client-app-id client) "broken"))
  (when (not (ewlc-client-title client))
    (setf (ewlc-client-client client) "broken"))
  (ewlc-set-output client output))

(defun ewlc-client-wlr-surface (client)
  "Get the wlr_surface for the CLIENT's wlr_xdg_surface or wlr_xwayland surface."
  (if (not (equal (ewlc-client-type client) 'xdg-shell))
      (wlr-xwayland-surface-wlr-surface (ewlc-client-xwayland-surface client))
    (wlr-xdg-surface-get-wlr-surface (ewlc-client-xwayland-surface client))))

(defun ewlc-render-clients (output time)
  "Render the clients on OUTPUT at TIME."
  (let ((active-client (ewlc-get-active-client)))
    ;; Each subsequent window we render is rendered on top of the last. Because
    ;; our stacking list is ordered front-to-back, we iterate over it backwards.
    (dolist (client (reverse (ewlc-client-stack-list *ewlc*)))
      (when (or (ewlc-visible-on-p client output)
                (wlr-output-layout-intersects (elwc-output-layout *ewlc*)
                                              (ewlc-output-wlr-output output)
                                              (ewlc-client-geom client)))
        (let* ((surface (ewlc-client-wlr-surface client))
               (orig-x (wlr-box-x (ewlc-client-geom client)))
               (orig-y (wlr-box-y (ewlc-client-geom client)))
               (bw (ewlc-border-width *ewlc*))
               (w (wlr-surface-current-width surface))
               (h (wlr-surface-current-height surface))
               (output-coords (wlr-output-layout-output-coords (ewlc-output-layout *ewlc*)
                                                               (ewlc-output-wlr-output output)
                                                               orig-x
                                                               orig-y))
               (x (nth 0 output-coords))
               (y (nth 1 output-coords))
               (border-list `((,x          ,y          ,(+ w (* 2 bw)) ,bw)    ;; top
                              (,x          ,(+ y bw)   ,bw             ,h)     ;; left
                              (,(+ x bw w) ,(+ y bw)   ,bw             ,h)     ;; right
                              (,x          ,(+ y bw h) ,(+ w (* 2 bw)) ,bw)))  ;; bottom
               (border-color (if (ewlc-client= client active-client)
                                 (ewlc-focus-color *ewlc*)
                               (ewlc-border-color *ewlc*))))
          ;; render the client borders
          (dolist (border border-list)
            (cl-destructuring-bind (x y w h) border
              (let ((border-box (wlr-box-create x y w h)))
                (wlr-scale-box border-box (ewlc-output-scale output))
                (wlr-render-rect (ewlc-renderer *ewlc*)
                                 border-box
                                 border-color
                                 (ewlc-output-wlr-output output)))))
          ;; render the client surfaces
          (let* ((x (+ (wlr-box-x (ewlc-client-geom client)) bw))
                 (y (+ (wlr-box-y (ewlc-client-geom client)) bw))
                 (render-data (ewlc-create-render-data (ewlc-output-wlr-output *ewlc*)
                                                       (ewlc-output-layout *ewlc*)
                                                       (ewlc-renderer *ewlc*)
                                                       x y time)))
            (if (not (equal (ewlc-client-type client) 'xdg-shell))
                (wlr-surface-for-each-surface-render (wlr-xwayland-surface-wlr-surface
                                                      (ewlc-client-xwayland-surface client))
                                                     render-data)
              (wlr-xdg-surface-for-each-surface-render (ewlc-client-xdg-surface client)
                                                       render-data))))))))

(defun ewlc-render-independents (wlr-output time)
  "Render independents for WLR-OUTPUT at TIME."
  (dolist (client (reverse (ewlc-independent-list *ewlc*)))
    (let* ((surface (ewlc-client-xwayland-surface client))
           (x (wlr-xwayland-surface-x surface))
           (y (wlr-xwayland-surface-y surface))
           (w (wlr-xwayland-surface-width surface))
           (h (wlr-xwayland-surface-height surface))
           (wlr-box (wlr-box-create x y w h)))
      (when (wlr-output-layout-intersects (ewlc-output-layout *ewlc*) wlr-output wlr-box)
        (let ((render-data (ewlc-create-render-data wlr-output
                                                    (ewlc-output-layout *ewlc*)
                                                    (ewlc-renderer *ewlc*)
                                                    x y time)))
          (wlr-surface-for-each-surface-render (wlr-xwayland-surface-wlr-surface surface)
                                               render-data))))))

(defun ewlc-get-top-client-focus (ewlc-output)
  "Get the top focused client on the EWLC-OUTPUT."
  (cl-dolist (client (ewlc-client-focus-list *ewlc*))
    (when (ewlc-visible-on-p client ewlc-output)
      (cl-return client))))

(defun ewlc-set-floating (client floating-p)
  "Set the CLIENT to floating depending on FLOATING-P."
  (when (not (equal (ewlc-client-floating-p client) floating-p))
    (setf (ewlc-client-floating-p client) floating-p)
    (ewlc-arrange (ewlc-client-output client))))


(defun ewlc-set-output (client target-output)
  "Set CLIENT to TARGET-OUTPUT."
  (let* ((curr-output (ewlc-client-output client))
         (old-wlr-output (ewlc-output-wlr-output curr-output))
         (new-wlr-output (ewlc-output-wlr-output target-output))
         (client-surface (ewlc-get-wlr-surface client))
         new-client)
    (when (and target-output (not (equal target-output curr-output)))
      (setf (ewlc-client-outout client) target-output)
      (when curr-output
        (wlr-surface-send-leave client-surface old-wlr-output)
        (ewlc-arrange curr-output))
      (when target-output
        (ewlc-apply-bounding-box client (ewlc-output-box target-output))
        ;; FIXME: Fwlr_xdg_shell.c is not completed.
        (wlr-surface-send-enter client-surface new-wlr-output)
        (ewlc-arrange target-output))
      (setq new-client (ewlc-get-top-client (ewlc-active-output *ewlc*)))
      (ewlc-focus-client client new-client t))))

(defun ewlc-pointer-focus (client surface x y time-msec)
  "Focus the CLIENT or SURFACE at the pointer X Y coordinates at TIME-MSEC."
  (when (and client (not surface))
    (setq surface (ewlc-get-wlr-surface client))
    (let* ((seat (ewlc-seat *ewlc*))
           (pointer-focused-surface (wlr-seat-pointer-focused-surface seat)))
      (cond ((not surface)
             ;; there is no surface, so clear pointer focus
             (wlr-seat-pointer-notify-clear-focus (ewlc-seat *ewlc*)))
            ((equal surface pointer-focused-surface)
             ;; surface is already focused. Notify of motion.
             (wlr-seat-pointer-notify-motion seat time-msec x y))
            (t
             (wlr-seat-pointer-notify-enter seat surface x y)
             (when (and (ewlc-sloppy-focus *ewlc*) (equal (ewlc-client-type client) 'x11-unmanaged))
               (ewlc-focus-client (ewlc-get-active-client) client nil)))))))

(cl-defun ewlc-get-client-at-point (cursor)
  "Get the client at the CURSOR point."
  (cl-dolist (client (ewlc-client-stack-list *ewlc*))
    (when (and (ewlc-client-visible-on-output-p client (ewlc-client-output client))
               (wlr-box-contains-point (ewlc-client-geom client) cursor))
      (cl-return client))))

(defun ewlc-get-wlr-surface (client)
  "Get the wlr_surface for the CLIENT."
  (if (equal (ewlc-client-type client) 'xdg-shell)
      (wlr-xdg-surface-get-wlr-surface (ewlc-client-xdg-surface client))
    (wlr-xwayland-surface-get-wlr-surface (ewlc-client-xwayland-surface client))))

(defun ewlc-apply-bounding-box (client bounding-box)
  "Apply BOUNDING-BOX to CLIENT geometry."
  (let ((c-geom (ewlc-client-geom client))
        (border-width (ewlc-border-width *ewlc*))
        (bb-x (wlr-box-x bounding-box))
        (bb-y (wlr-box-y bounding-box))
        (bb-width (wlr-box-width bounding-box))
        (bb-height (wlr-box-height bounding-box)))
    (wlr-set-box-width c-geom (max 1 (wlr-box-width c-geom)))
    (wlr-set-box-height c-geom (max 1 (wlr-box-height c-geom)))
    (when (>= (wlr-box-x c-geom) (+ bb-x bb-width))
      (wlr-set-box-x c-geom (- (+ bb-x bb-width) (wlr-box-width c-geom))))
    (when (>= (wlr-box-y c-geom) (+ bb-y bb-height))
      (wlr-set-box-y c-geom (- (+ bb-y bb-height) (wlr-box-height c-geom))))
    (when (<= (+ (wlr-box-x c-geom) (wlr-box-width c-geom) (* 2 border-width)) bb-x)
      (wlr-set-box-x c-geom bb-x))
    (when (<= (+ (wlr-box-y c-geom) (wlr-box-height c-geom) (* 2 border-width)) bb-y)
      (wlr-set-box-y c-geom bb-y))))


(defun client-visible-on-output-p (client output)
  "Return t if CLIENT is visible on OUTPUT."
  (equal (ewlc-client-output client) output))

(defun ewlc-get-active-client ()
  "Get the active client from client-focus-list that visible on the output."
  (let ((output (ewlc-active-output *ewlc*))
        (client (car (ewlc-client-focus-list *ewlc*))))
    (if (and client (ewlc-visible-on-p output client))
        client
      nil)))

(cl-defun ewlc-focus-client (old-client new-client raise-p)
  "Focus the NEW-CLIENT and raise if RAISE-P is t, unfocus the OLD-CLIENT."
  (when (and new-client raise-p)
    (setf (ewlc-stack-list *ewlc*)
          (cl-remove new-client (ewlc-stack-list *ewlc*) :test 'ewlc-client=))
    (setf (ewlc-stack-list *ewlc*) (push new-client (ewlc-stack-list *ewlc*))))
  (when (equal old-client new-client)
    ;; nothing to do
    (cl-return-from ewlc-focus-client))
  (when (and (not (equal old-client new-client))
             old-client)
    ;; de-activate old client
    (if (not (equal (ewlc-client-type old-client) 'xdg-shell))
        (wlr-xwayland-surface-activate (ewlc-client-xwayland-surface old-client) 0)
      (wlr-xdg-toplevel-set-activated (ewlc-client-xdg-surface old-client) 0)))
  (when (not new-client)
    ;; no new client - so just clear focus.
    (wlr-seat-keyboard-notify-clear-focus (ewlc-seat *ewlc*))
    (cl-return-from ewlc-focus-client))
  (let* ((seat (ewlc-seat *ewlc*))
         (keyboard (wlr-seat-get-keyboard seat)))
    ;; there is a new-client - so focus it
    (wlr-set-keyboard-notify-enter seat (ewlc-client-wlr-surface new-client) keyboard)
    ;; put client at top of focus list
    (setf (ewlc-focus-list *ewlc*)
          (cl-remove new-client (ewlc-focus-list *ewlc*) :test 'ewlc-client=))
    (setf (ewlc-focus-list *ewlc*) (push new-client (ewlc-focus-list *ewlc*))))
  ;; activate new client
  (if (not (equal (ewlc-client-type old-client) 'xdg-shell))
      (wlr-xwayland-surface-activate (ewlc-client-xwayland-surface old-client) 0)
    (wlr-xdg-toplevel-set-activated (ewlc-client-xdg-surface old-client) 0)))


(defun ewlc-resize (client x y width height interact-p)
  "Resize to CLIENT to X, Y, WIDTH, HEIGHT depending on INTERACT-P.
In a more fleshed-out compositor, wait for the client to prepare a buffer
at the new size, then commit any movement that was prepared."
  (let* ((client-geom (ewlc-client-geom client))
         (bbox (if interact-p
                   (ewlc-output-geom *ewlc*)
                 client-geom))
         (border-width (ewlc-border-width *ewlc*)))
    (wlr-set-box-x client-geom x)
    (wlr-set-box-y client-geom y)
    (wlr-set-box-width client-geom width)
    (wlr-set-box-height client-geom height)
    (ewlc-apply-bounding-box client bbox)
    (let* ((bclient-geom (ewlc-client-geom client))
           (bx (wlr-box-x bclient-geom))
           (by (wlr-box-y bclient-geom))
           (bwidth (wlr-box-width bclient-geom))
           (bheight (wlr-box-height bclient-geom)))
      (if (not (equal (ewlc-client-type client) 'xdg-shell))
          (wlr-xwayland-surface-configure (ewlc-xwayland-surface client)
                                          bx
                                          by
                                          (- bwidth (* 2 border-width))
                                          (- bheight (* 2 border-width)))
        (wlr-xdg-toplevel-set-size (ewlc-xdg-surface client)
                                   (- bwidth (* 2 border-width))
                                   (- bheight (* 2 border-width)))))))

(defun ewlc-get-independent-at-point (cursor)
  "Get the xwayland client a CURSOR point."
  (dolist (client (ewlc-independent-list *ewlc*))
    (let ((geom (wlr-surface-xwayland-get-wlr-box (ewlc-client-xwayland-surface client))))
      (when (wlr-box-contains-point geom cursor)
        (cl-return client)))))

;; handlers ---------------------------------------------------

(defun ewlc-surface-map-handler (client)
  "Called when the CLIENT surface is mapped, or ready to display on-screen."
  (if (equal (ewlc-client-type client) 'x11-unmanaged)
      (setf (ewlc-independent-list *ewlc*) (push client (ewlc-independent-list client)))
    (setf (ewlc-client-list *ewlc*) (push client (ewlc-client-list client)))
    (setf (ewlc-stack-list *ewlc*) (push client (ewlc-stack-list client)))
    (setf (ewlc-focus-list *ewlc*) (push client (ewlc-focus-list client)))
    (if (not (equal (ewlc-client-type client) 'xdg-shell))
        (let ((geom (ewlc-client-geom client))
              (surface (ewlc-client-surface-xwayland client))
              (border-width (ewlc-border-width *ewlc*)))
          (wlr-set-box-x geom (wlr-box-x surface))
          (wlr-set-box-y geom (wlr-box-y surface))
          (wlr-set-box-width geom (+ (* 2 border-width) (wlr-box-width surface)))
          (wlr-set-box-height geom (+ (* 2 border-width) (wlr-box-height surface))))
      (setf (ewlc-client-geom client) (wlr-xdg-surface-get-geometry
                                       (ewlc-client-surface-xdg client)
                                       (ewlc-client-geom client)))
      (let ((geom (ewlc-client-geom client))
            (border-width (ewlc-border-width *ewlc*)))
        (wlr-set-box-width geom (+ (wlr-box-width geom) (* 2 border-width)))
        (wlr-set-box-height geom (+ (wlr-box-height geom) (* 2 border-width)))))
    (elwc-apply-title client (ewlc-active-output *ewlc*))))


(defun ewlc-new-xdg-shell-surface-handler (xdg-surface)
  "This event is raised when wlr_xdg_shell receives a new XDG-SURFACE.
The event is from a client, either a toplevel (application window) or popup."
  (when (wlr-xdg-surface-role-toplevel xdg-surface)
    (let* ((client-ptr (ewlc-make-xdg-surface-client-ptr xdg-surface))
           (client (make-ewlc-client :ptr client-ptr
                                     :xdg-surface xdg-surface
                                     :type 'xdg-surface
                                     :floating-p nil)))
      (wlr-xdg-toplevel-set-tiled (ewlc-client-xdg-surface client))
      (ewlc-set-xdg-surface-client-listeners (ewlc-client-ptr client)
                                             (ewlc-client-xdg-surface client)))))

(defun ewlc-new-xwayland-surface-handler (xwayland-surface)
  "This event is raised when there is a new XWAYLAND-SURFACE."
  (let* ((client-ptr (ewlc-make-xwayland-surface-client-ptr xwayland-surface))
         (client (make-ewlc-client :ptr client-ptr
                                   :xwayland-surface xwayland-surface
                                   :floating-p nil)))
    (setf (ewlc-client-type client) (if (wlr-xwayland-surface-override-redirect)
                                        'x11-unmanaged
                                      'x11-managed))
    (ewlc-set-xwayland-surface-client-listeners (ewlc-client-ptr client)
                                                (ewlc-client-xwayland-surface client))))


(defun ewlc-surface-destroy-handler (client)
  "Called when the CLIENT surface is destroyed."
  (ewlc-remove-client-listeners client (ewlc-client-type client))
  (ewlc-free (ewlc-client-ptr client)))

(defun ewlc-xdg-surface-commit-handler (client)
  "Handle the commit event for the CLIENT."
  (when (and (ewlc-client-resize-p client)
             (>= (wlr-xdg-surface-get-configure-serial (ewlc-client-xdg-surface client)) 0))
    ;; mark pending resize as completed
    (setf (ewlc-client-resize-p client) nil)))

(defun ewlc-xwayland-surface-request-activate-handler (client)
  "Handle the request activate event for the CLIENT."
  (when (equal (ewlc-client-type client) 'x11-managed)
    (wlr-xwayland-surface-activate (ewlc-client-xwayland-surface client) 1)))


(defun ewlc-surface-unmap-handler (client)
  "Handle the unmap event for the CLIENT surface."
  (setf (ewlc-client-list *ewlc*)
        (cl-remove client (ewlc-client-list *ewlc*) :test 'ewlc-client=))
  (unless (equal (ewlc-client-type client) 'x11-unmanaged)
    (let* ((output (ewlc-client-output client))
           (new-focused-client (ewlc-get-top-client-focus output)))
      (setf (ewlc-client-focus-list *ewlc*)
            (cl-remove client (ewlc-client-focus-list *ewlc*) :test 'ewlc-client=))
      (setf (ewlc-client-stack-list *ewlc*)
            (cl-remove client (ewlc-client-stack-list *ewlc*) :test 'ewlc-client=))
      (ewlc-arrange output)
      (ewlc-focus-client nil new-focused-client t))))

;; ------------------------------------------------------------
;; pointer
;; ------------------------------------------------------------

(defun ewlc-action-move-resize (cursor-mode)
  "Apply action depending on if CURSOR-MODE is ewlc-cursor-move, or ewlc-cursor-resize."
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
        (cond ((equal cursor-mode 'ewlc-cursor-move)
               (setf (ewlc-grabbed-client-x *ewlc*) (- cursor-x client-x))
               (setf (ewlc-grabbed-client-y *ewlc*) (- cursor-y client-y))
               (ewlc-set-cursor-image (ewlc-cursor-mgr *ewlc*) "fleur"
                                      (ewlc-cursor *ewlc*)))
              ((equal cursor-mode 'ewlc-cursor-resize)
               ;; FIXME: does not work for X windows
               (ewlc--c/cursor-warp-closest (ewlc-cursor *ewlc*)
                                            (+ client-x client-w)
                                            (+ client-y client-h))
               (ewlc-set-cursor-image (ewlc-cursor-mgr *ewlc*) "bottom_right_corner"
                                      (ewlc-cursor *ewlc*))))))))

(defun ewlc-apply-button-action (event)
  "Apply the button action for the EVENT."
  (let* ((keyboard (wlr-seat-get-keyboard (ewlc-seat *ewlc*)))
         (mods (wlr-keyboard-get-modifiers keyboard))
         (button (wlr-event-pointer-button-get-button event)))
    (when (string= (elwc-modifier-key *ewlc*) mods)
      (pcase button
        ('wlr-button-left
         (ewlc-action-move-resize 'ewlc-cursor-move))
        ('wlr-button-middle
         nil)
        ('wlr-button-right
         (ewlc-action-move-resize 'ewlc-cursor-resize))))))

(defun ewlc-create-pointer (wlr-input-device)
  "Attach pointer WLR-INPUT-DEVICE to the cursor."
  ;; TODO: can we attach multiple devices to the same cursor?
  ;; no need to keep an ewlc list of input devices and add to the list?
  (wlr-cursor-attach-input-device (ewlc-cursor *ewlc*) wlr-input-device))


(defun ewlc-motion-resize (time-msec)
"The motion/resize of a client due to cursor motion at TIME-MSEC."
(when (ewlc-sloppy-focus *ewlc*)
  ;; Update active output as part of a client drag.
  ;; FIXME: need to fix ewlc-get-output-at-point
  (setf (ewlc-active-output *ewlc*) (ewlc-get-output-at-point (ewlc-cursor *ewlc*))))
(let* ((grabbed-client (ewlc-grabbed-client *ewlc*))
       (grabbed-client-geom (ewlc-client-geom grabbed-client))
       (grabbed-client-x (wlr-box-x grabbed-client-geom))
       (grabbed-client-y (wlr-box-y grabbed-client-geom))
       (grabbed-client-width (wlr-box-width grabbed-client-geom))
       (grabbed-client-height (wlr-box-height grabbed-client-geom))
       (cursor (ewlc-cursor *ewlc*))
       (cursor-x (wlr-cursor-x cursor))
       (cursor-y (wlr-cursor-y cursor))
       (border-width (ewlc-border-width *ewlc*)))
  (cond ((equal (ewlc-cursor-mode *ewlc*) 'ewlc-cursor-move)
         (ewlc-resize grabbed-client
                      (- cursor-x grabbed-client-x)
                      (- cursor-y grabbed-client-y)
                      grabbed-client-width
                      grabbed-client-height
                      t))
        ((equal (ewlc-cursor-mode *ewlc*) 'ewlc-cursor-resize)
         (ewlc-resize grabbed-client
                      grabbed-client-x
                      grabbed-client-y
                      (- cursor-x grabbed-client-x)
                      (- cursor-y grabbed-client-y)
                      t))
        (t
         (let* ((client (ewlc-get-independent-at-point (ewlc-cursor *ewlc*)))
                surface)
           ;; FIXME: coordinates of the clients are incorrect functions.
           (if client
               (setq surface (wlr-surface-surface-at
                              (ewlc-client-xwayland-surface client)
                              (- cursor-x (ewlc-xwayland-x client) border-width)
                              (- cursor-y (ewlc-xwayland-y client) border-width)))
             (setq client (ewlc-get-client-at-point (ewlc-cursor *ewlc*)))
             (if (and client (equal (ewl-client-type client) 'xdg-shell))
                 (setq surface (wlr-xdg-surface-surface-at
                                (ewlc-client-xdg-surface client)
                                ;; FIXME: ewlc-client-x function does not exist
                                (- cursor-x (ewlc-client-x client) border-width)
                                (- cursor-y (ewlc-client-y client) border-width)))
               (setq surface (wlr-surface-surface-at
                              (ewlc-client-xwayland-surface client)
                              (- cursor-x (ewlc-client-x client) border-width)
                              (- cursor-y (ewlc-client-y client) border-width))))
             (unless surface
               ;; If there is no client surface under the cursor, set image to default.
               (ewlc-set-cursor-image cursor "left_ptr" (ewlc-cursor-mgr *ewlc*)))
             (cl-destructuring-bind
                 (wlr-surface x y) surface
               (ewlc-pointer-focus client wlr-surface x y time-msec))))))))

;; handlers --------------------------------------------------------------------

(defun ewlc-cursor-motion-absolute-handler (event)
  "Handle EVENT forwarded by the cursor when a pointer emits a _absolute_ motion."
  (wlr-cursor-warp-absolute (ewlc-cursor *ewlc*) event)
  (ewlc-motion-resize (wlr-event-pointer-motion-absolute-time-msec event)))

(defun ewlc-cursor-motion-handler (event)
  "Handle EVENT forwarded by the cursor when a pointer emits a _relative_ motion."
  (wlr-cursor-move (ewlc-cursor *ewlc*) event)
  (ewlc-motion-resize (wlr-event-pointer-motion-time-msec event)))

(defun ewlc-cursor-frame-handler ()
  "Handle EVENT forwarded by the cursor when pointer emits a frame event.
Frame events are sent after regular pointer events to group multiple events together."
  (wlr-seat-pointer-notify-frame (ewlc-seat *ewlc*)))

(defun ewlc-seat-request-set-primary-selection-handler (event)
  "Handle the EVENT raised when client requests to set the selection.
Usually occurs when the user copies something."
  (wlr-seat-set-primary-selection (ewlc-seat *ewlc*) event))

(defun ewlc-seat-request-set-selection-handler (event)
  "Handle the EVENT raised when client requests to set the selection.
Usually occurs when the user copies something."
  (wlr-seat-set-selection (ewlc-seat *ewlc*) event))

(defun ewlc-seat-pointer-request-set-cursor-handler (event)
  "Handle EVENT raised by the seat when a client provides a cursor image."
  (unless (equal (ewlc-cursor-mode *ewlc*) 'cursor-normal)
    (wlr-cursor-set-surface (ewlc-cursor *ewlc*) (ewlc-seat *ewlc*) event)))

(defun ewlc-pointer-axis-handler (event)
  "Handle an axis EVENT forwarded by the pointer (e.g. scroll wheel movement)."
  (wlr-seat-pointer-notify-axis (ewlc-seat *ewlc*) event))

(defun ewlc-pointer-button-handler (event)
  "Handle an button EVENT forwarded by the pointer."
  (let ((button-state (wlr-get-button-press-state event))
        (handled nil))
    (cond ((equal button-state 'wlr-button-pressed)
           ;; change focus if the button is pressed over a client,
           ;; then start the button action
           (let ((new-client (ewlc-get-client-at-point (ewlc-cursor *ewlc*)))
                 (curr-client (ewlc-get-active-client)))
             (ewlc-focus-client curr-client new-client t)
             (setq handled (ewlc-apply-button-action event))))
          ((equal button-state 'wlr-button-released)
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
      (wlr-seat-pointer-notify-button (ewlc-seat *ewlc*) event))))

;; ------------------------------------------------------------
;; keyboard
;; ------------------------------------------------------------

(defun ewlc-create-keyboard (wlr-input-device)
  "Create keyboard for WLR-INPUT-DEVICE."
  (wlr-keyboard-set-keymap wlr-input-device)
  (wlr-keyboard-set-repeat-info wlr-input-device
                                (ewlc-repeat-rate *ewlc*)
                                (ewlc-repeat-delay *ewlc*))
  (let* ((keyboard-ptr (ewlc-make-keyboard-ptr wlr-input-device))
         (keyboard (make-ewlc-keyboard :ptr keyboard-ptr
                                       :device wlr-input-device)))
    (ewlc-keyboard-set-event-listeners (ewlc-keyboard-ptr keyboard)
                                       (ewlc-keyboard-device keyboard))
    (wlr-seat-set-keyboard (ewlc-seat *ewlc*) (ewlc-keyboard-device keyboard))
    (push keyboard (ewlc-keyboard-list *ewlc*))))

;; keyboard event handlers ---------------------------------------------

(defun ewlc-keyboard-destroy-handler (ewlc-keyboard)
  "Destroy EWLC-KEYBOARD."
  (setf (ewlc-keyboard-list *ewlc*) (cl-remove ewlc-keyboard
                                               (ewlc-keyboard-list *ewlc*)
                                               :test 'keyboard=))
  (ewlc-free (ewlc-keyboard-ptr ewlc-keyboard)))

(defun ewlc-keyboard-key-handler (ewlc-keyboard wlr-event-keyboard-key)
  "Handle WLR-EVENT-KEYBOARD-KEY for EWLC-KEYBOARD."
  (let* ((event wlr-event-keyboard-key)
         (wlr-keyboard (wlr-device-get-keyboard (ewlc-keyboard-device ewlc-keyboard)))
         ;; convert from libinput keycode to xkbcommon
         (event-keycode (+ 8 (wlr-event-keyboard-key-get-keycode event)))
         (event-state (wlr-event-keyboard-key-get-state event))
         (event-time-msec (wlr-event-keyboard-key-get-time-msec event))
         (key-list (xkb-state-key-get-syms wlr-keyboard event-keycode))
         (mods (wlr-state-key-get-modifiers wlr-keyboard)))
    (if (and (equal event-state 'wlr-key-pressed) (equal mods (ewlc-modifier-key *ewlc*)))
        (dolist (key key-list)
          (unless (ewlc-apply-keybinding (concat mods "-" key))
            ;; pass non-handled keys to client that use the modifier
            (wlr-seat-set-keyboard (ewlc-seat *ewlc*) (ewlc-keyboard-device ewlc-keyboard))
            (wlr-seat-keyboard-notify-key (ewlc-seat *ewlc*)
                                          event-time-msec
                                          event-keycode
                                          event-state)))
      ;; pass non-modifier keys to the client
      (wlr-seat-set-keyboard (ewlc-seat *ewlc*) (ewlc-keyboard-device ewlc-keyboard))
      (wlr-seat-keyboard-notify-key (ewlc-seat *ewlc*) event-time-msec event-keycode event-state))))

(defun ewlc-keyboard-modifiers-handler (ewlc-keyboard)
  "Handle modifier key press on EWLC-KEYBOARD. Communicate press to the client."
  (let ((seat (ewlc-seat *ewlc*)))
    (wlr-seat-set-keyboard seat (ewlc-keyboard-device  ewlc-keyboard))
    (wlr-seat-keyboard-notify-modifiers seat (ewlc-keyboard-device ewlc-keyboard))))

;; ------------------------------------------------------------
;; input device
;; ------------------------------------------------------------

(defun ewlc-backend-new-input-handler (wlr-input-device)
  "Handle a new input WLR-INPUT-DEVICE."
  (log-message (format "device: %s" wlr-input-device))
  (let ((device-type (wlr-input-device-get-type wlr-input-device)))
    (pcase device-type
      ('wlr-input-device-keyboard
       (ewlc-create-keyboard wlr-input-device)
       (wlr-set-seat-capabilites (ewlc-seat *ewlc*) (not (eq (ewlc-keyboard-list *ewlc*) nil))))
      ('wlr-input-device-pointer
       (ewlc-create-pointer wlr-input-device))
      (_
       (log-message (format "other device: %s" wlr-input-device))))))


;; ------------------------------------------------------------
;; keyboard commands
;; ------------------------------------------------------------

(defun ewlc-prev-visible-client (client output client-list)
  "Find previous client after CLIENT within CLIENT-LIST which is visible on OUTPUT."
  (let* ((pos (cl-position client client-list :test 'ewlc-client=))
         (after-pos-list (cl-subseq client-list (1+ pos) ))
         (before-pos-list (cl-subseq client-list 0 (1- pos )))
         (prev-client nil))
    (cl-dolist (c (nreverse before-pos-list))
      (when (ewlc-visible-on-p c output)
        (setq prev-client c)
        (cl-return)))
    (when (not prev-client)
      (cl-dolist (c (nreverse after-pos-list))
        (when (ewlc-visible-on-p c output)
          (setq prev-client c)
          (cl-return))))
    prev-client))

(defun ewlc-next-visible-client (client output client-list)
  "Find next client after CLIENT within CLIENT-LIST which is visible on OUTPUT."
  (let* ((pos (cl-position client client-list :test 'ewlc-client=))
         (after-pos-list (cl-subseq client-list (1+ pos) ))
         (before-pos-list (cl-subseq client-list 0 (1- pos )))
         (next-client nil))
    (cl-dolist (c after-pos-list)
      (when (ewlc-visible-on-p c output)
        (setq next-client c)
        (cl-return)))
    (when (not next-client)
      (cl-dolist (c before-pos-list)
        (when (ewlc-visible-on-p c output)
          (setq next-client c)
          (cl-return))))
    next-client))

(defun ewlc-command-focus-next-client ()
  "Focus next client."
  (let* ((output (ewlc-active-output *ewlc*))
         (curr-client (ewlc-active-client *ewlc*))
         (next-client nil))
    (when (and output curr-client)
      (setq next-client (ewlc-next-visible-client curr-client output (ewlc-client-list *ewlc*)))
      (when next-client
        (ewlc-focus-client curr-client next-client t)))))

(defun ewlc-command-focus-prev-client ()
  "Focus previous client."
  (let* ((output (ewlc-active-output *ewlc*))
         (curr-client (ewlc-active-client *ewlc*))
         (prev-client nil))
    (when (and output curr-client)
      (setq prev-client (ewlc-prev-visible-client curr-client output (ewlc-client-list *ewlc*)))
      (when prev-client
        (ewlc-focus-client curr-client prev-client t)))))

(defun ewlc-command-add-master ()
  "Add a master."
  (let ((output (ewlc-active-output *ewlc*)))
    (cl-incf (ewlc-output-num-master output))
    (ewlc-arrange output)))

(defun elwc-command-remove-master ()
  "Remove a master."
  (let ((output (ewlc-active-output *ewlc*)))
    (when (> (ewlc-output-num-master output) 1)
      (cl-decf (ewlc-output-num-master output))
      (ewlc-arrange output))))

(defun ewlc-command-incr-master-ratio ()
  "Increment master ratio."
  (let* ((inc 0.05)
         (output (ewlc-active-output *ewlc*))
         (new-master-ratio (+ (ewlc-output-master-ratio output) inc)))
    (when (<= new-master-ratio 0.9)
      (setf (ewlc-output-master-ratio output) new-master-ratio)
      (ewlc-arrange output))))

(defun ewlc-command-decr-master-ratio ()
  "Decrement master ratio."
  (let* ((inc -0.05)
         (output (ewlc-active-output *ewlc*))
         (new-master-ratio (+ (ewlc-output-master-ratio output) inc)))
    (when (>= new-master-ratio 0.1)
      (setf (ewlc-output-master-ratio output) new-master-ratio)
      (ewlc-arrange output))))

(defun ewlc-command-kill-client ()
  "Kill the active client."
  (let ((client (ewlc-get-active-client)))
    (when client
      (if (not (equal (ewlc-client-type client) 'xdg-shell))
          (wlr-xwayland-surface-close (ewlc-client-xwayland-surface client))
        (wlr-xdg-toplevel-send-close (ewlc-client-xdg-surface client))))))

(defun ewlc-command-toggle-floating ()
  "Toggle floating the active client."
  (let ((client (ewlc-get-active-client)))
    (when client
      (ewlc-set-floating client (not (ewlc-client-floating-p client))))))

(defun ewlc-command-kill ()
  "Exit the wayland compositor."
  (setf (ewlc-running-p *ewlc*) nil)
  (wl-display-terminate (ewlc-display *ewlc*))
  (ewlc-cleanup))

(defun ewlc-command-terminal ()
  "Start a terminal."
  ;; TODO: re-write this function
  (ewlc-spawn "alacritty" ""))

(defvar ewlc-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'ewlc-focus-next-client)
    (define-key map (kbd "M-p") #'ewlc-focus-prev-client)
    (define-key map (kbd "M-y") #'ewlc-new-focus-next-client)
    (define-key map (kbd "M-i") #'ewlc-command-incr-master-ratio)
    (define-key map (kbd "M-d") #'ewlc-command-decr-master-ratio)
    (define-key map (kbd "M-m") #'ewlc-command-add-master)
    (define-key map (kbd "M-b") #'ewlc-command-remove-master)
    (define-key map (kbd "M-k") #'ewlc-command-kill-client)
    (define-key map (kbd "M-t") #'ewlc-command-terminal)
    (define-key map (kbd "M-q") #'ewlc-command-kill)
    (define-key map (kbd "M-f") #'ewlc-command-toggle-floating)
    map
    ))

;; ------------------------------------------------------------
;; event loop
;; ------------------------------------------------------------

(defvar *ewlc-thread* nil "The thread running the wayland event loop.")

(defun ewlc-apply-keybinding (key-binding)
  "Apply the keybings for KEY-BINDING."
  (let ((command (lookup-key ewlc-prefix-map (kbd (concat key-binding)))))
    (if (and command (fboundp command))
        (progn
          (funcall command)
          t)
      nil)))

(defun ewlc-start-compositor ()
  "Start the wayland compositor."
  (ewlc-start "alacritty")
  (setq (ewlc-running-p *ewlc*) t)
  (setq *ewlc-thread* (make-thread (lambda ()
                                     (while (ewlc-running-p *ewlc*)
                                       (ewlc-handle-events)
                                       (ewlc-handle-keybindings (ewlc-server *ewlc*))
                                       (ewlc-display-dispatch)
                                       ;; or use: run-at-time ?
                                       (sleep-for 0.01)))
                                   "loop-thread")))

(provide 'ewlc-server)
;;; ewlc-server.el ends here
