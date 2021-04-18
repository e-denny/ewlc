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

(setq debug-on-error t)

;; (module-load "/home/edgar/Projects/ewlc/lib/ewlc.so")
(load "/home/edgar/Projects/ewlc/lib/ewlc.so")

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
  unmapped-client-list
  stack-client-list
  focus-client-list
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
    ;; FIXME: this needs two parameters
    (when (ewlc-floating-type-p xwayland-surface)
      (setf (ewlc-client-floating-p client) t))))

(defun ewlc-display-dispatch ()
  "Dispatch the event loop for the display."
  (let ((event-loop (wl-display-get-event-loop (ewlc-display *ewlc*))))
    (log-message (format "ewlc-display-dispatch: event-loop: '%s'" event-loop))
    (log-message (format "ewlc-display-dispatch: display: '%s'" (ewlc-display *ewlc*)))
    (wl-display-flush-clients (ewlc-display *ewlc*))
    (log-message (format "ewlc-display-dispatch: before wl-event-loop-dispatch"))
    (wl-event-loop-dispatch event-loop -1)
    (log-message (format "ewlc-display-dispatch: after wl-event-loop-dispatch"))
    ))

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
  (log-message (format "ewlc-start: start"))
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
                            :unmapped-client-list nil
                            :focus-client-list nil
                            :stack-client-list nil
                            :independent-list nil
                            ;; TODO: are these next 2 settings sensible?
                            :repeat-rate 25
                            :repeat-delay 600
                            :border-width 4
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
    ;; FIXME: this causes a crash.
    ;; (ewlc-xwayland-set-listeners (ewlc-ptr *ewlc*) (ewlc-seat *ewlc*))
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
        (setf (ewlc-active-output *ewlc*) (ewlc-get-ewlc-output-at-point cursor))
        (log-message (format "    ewlc-start: active-output: '%s'" (ewlc-active-output *ewlc*)))
        (log-message (format "    +++++++++++++++++++++++++++++++"))
        (wlr-cursor-warp-closest cursor x y)
        (wlr-xcursor-manager-set-cursor-image cursor-mgr "left_ptr" cursor))
      (setenv "WAYLAND_DISPLAY" socket 1)
      (start-process-shell-command "" nil shell-command)
      (log-message (format "ewlc-start: end")))))

(defun ewlc-keyboard-from-listener-container (keyboard-ptr)
  "Get the EWLC-KEYBOARD that has KEYBOARD-PTR in it's keyboard-ptr slot."
  (cl-dolist (ewlc-keyboard (ewlc-keyboard-list *ewlc*))
    (when (ewlc-keyboard-ptr= keyboard-ptr (ewlc-keyboard-ptr ewlc-keyboard))
      (cl-return ewlc-keyboard))))

(defun ewlc-output-from-listener-container (output-ptr)
  "Get the EWLC-OUTPUT that has OUTPUT-PTR in it's output-ptr slot."
  (log-message (format "    ewlc-output-from-listener-container: output-ptr: '%s'" output-ptr))
  (log-message (format "    ewlc-output-from-listener-container: output-list: '%s'"
                       (ewlc-output-list *ewlc*)))
  (cl-dolist (ewlc-output (ewlc-output-list *ewlc*))
    (log-message (format "    =: '%s'" (ewlc-output-ptr=  output-ptr (ewlc-output-ptr ewlc-output))))
    (when (ewlc-output-ptr= output-ptr (ewlc-output-ptr ewlc-output))
      (log-message (format "    match"))
      (cl-return ewlc-output))))

(defun ewlc-client-from-listener-container (client-ptr)
  "Get the EWLC-CLIENT that has CLIENT-PTR in it's client-ptr slot."
  (log-message (format "    ewlc-client-from-listener-container: client-ptr: '%s'" client-ptr))
  (log-message (format "    ewlc-client-from-listener-container: client-list: '%s'"
                       (ewlc-client-list *ewlc*)))
  (log-message (format "    ewlc-client-from-listener-container: unmapped-client-list: '%s'"
                       (ewlc-unmapped-client-list *ewlc*)))
  (cl-dolist (ewlc-client (ewlc-client-list *ewlc*))
    (when (ewlc-client-ptr= client-ptr (ewlc-client-ptr ewlc-client))
      (log-message (format "    client match"))
      (cl-return ewlc-client)))
  ;; Could not find client in client list, so try un-mapped clients
  (cl-dolist (ewlc-client (ewlc-unmapped-client-list *ewlc*))
    (when (ewlc-client-ptr= client-ptr (ewlc-client-ptr ewlc-client))
      (log-message (format "    unmapped client match"))
      (cl-return ewlc-client))))

(defun ewlc-handle-events ()
  "Handle all the events."
  ;; FIXME: need to get from pointer to defstruct that contains the pointer
  ;; i.e. find the pointer in the list of containers and hence the defstruct.
  (log-message (format "ewlc-handle-events: start"))
  (log-message (format "  pending events: '%s'" (ewlc-pending-events-p (ewlc-ptr *ewlc*))))
  (while (ewlc-pending-events-p (ewlc-ptr *ewlc*))
    (cl-destructuring-bind (event-container event-data event-type) (ewlc-get-event (ewlc-ptr *ewlc*))
      (log-message (format "  event-container: '%s'\n  event-data: '%s'\n  event-type: '%s'"
                           event-container event-data event-type))
      (log-message (format "  ------------------------------"))
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
        ('ewlc-deco-mgr-new-toplevel-decoration
         (log-message (format "  >> case: '%s'" event-type))
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
           (log-message (format "  output-ptr: '%s'" output-ptr))
           (log-message (format "  ewlc-output: '%s'" ewlc-output))
           (ewlc-output-frame-handler ewlc-output)))

        ('ewlc-xdg-shell-new-surface
         (log-message (format "  >> case: '%s'" event-type))
         (let ((wlr-xdg-surface event-data))
           (log-message (format "  event-data: '%s'" event-data))
           (log-message (format "  wlr-xdg-surface: '%s'" wlr-xdg-surface))
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
    (ewlc-remove-event (ewlc-ptr *ewlc*)))
  (log-message (format "ewlc-handle-events: end"))
  )


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
  (log-message (format "    ewlc-xdg-deco-mgr-new-toplevel-decoration-handler: wlr-xdg-toplevel-decoration-v1: '%s'" wlr-xdg-toplevel-decoration-v1))
  (let* ((deco-ptr (ewlc-make-deco-ptr (ewlc-ptr *ewlc*))))
    (log-message (format "    ewlc-xdg-deco-mgr-new-toplevel-decoration-handler: check1"))
    (ewlc-set-xdg-deco-mgr-toplevel-listeners deco-ptr wlr-xdg-toplevel-decoration-v1)
    (log-message (format "    ewlc-xdg-deco-mgr-new-toplevel-decoration-handler: check2"))
    (ewlc-deco-request-mode-handler wlr-xdg-toplevel-decoration-v1))
  (log-message (format "    ewlc-xdg-deco-mgr-new-toplevel-decoration-handler: end"))
  )

(defun ewlc-deco-request-mode-handler (wlr-xdg-toplevel-decoration-v1)
  "Handle WLR-XDG-TOPLEVEL-DECORATION-V1 set mode event."
  (log-message (format "    ewlc-deco-request-mode-handler: check1"))
  (wlr-xdg-toplevel-decoration-v1-set-mode wlr-xdg-toplevel-decoration-v1)
  (log-message (format "    ewlc-deco-request-mode-handler: check2"))
  )

(defun ewlc-deco-destroy-handler (wlr-xdg-toplevel-decoration-v1)
  "Handle WLR-XDG-TOPLEVEL-DECORATION-V1 destory event."
  ;; FIXME: need to access the wlr_deco->data to get pointer to free
  (ewlc-free wlr-xdg-toplevel-decoration-v1))

;; ------------------------------------------------------------
;; output
;; ------------------------------------------------------------

(defun ewlc-output-from-wlr-output (wlr-output)
  "Get the EWLC-OUTPUT that has WLR-OUTPUT in it's slot."
  (log-message (format "    ewlc-output-from-wlr-output: wlr-output: '%s'" wlr-output))
  (log-message (format "    ewlc-output-from-wlr-output: output-list: '%s'" (ewlc-output-list *ewlc*)))
  (cl-dolist (ewlc-output (ewlc-output-list *ewlc*))
    (log-message (format "    ewlc-output-from-wlr-output: wlr-output-1: '%s'" wlr-output))
    (log-message (format "    ewlc-output-from-wlr-output: wlr-output-2: '%s'" (ewlc-output-wlr-output ewlc-output)))
    (log-message (format "    =: '%s'" (wlr-output=  wlr-output (ewlc-output-wlr-output ewlc-output))))
    (when (wlr-output= wlr-output (ewlc-output-wlr-output ewlc-output))
      (log-message (format "    match"))
      (cl-return ewlc-output))))

(defun ewlc-get-ewlc-output-at-point (cursor)
  "Get the ewlc-output at the CURSOR."
  (let* ((output-layout (ewlc-output-layout *ewlc*))
         (wlr-output (wlr-output-layout-output-at output-layout cursor)))
    (log-message (format "    ewlc-get-ewlc-output-at-point: output-layout: '%s'" output-layout))
    (log-message (format "    ewlc-get-ewlc-output-at-point: wlr-output: '%s'" wlr-output))
    (log-message (format "    ewlc-get-ewlc-output-at-point: output-list: '%s'" (ewlc-output-list *ewlc*)))
    (if wlr-output
        (ewlc-output-from-wlr-output wlr-output)
      nil)))

;; FIXME: I re-named this function - change all the calls to it.
(defun ewlc-get-wlr-output-at-point (cursor)
  "Get the wlr-output at the CURSOR."
  (let ((output-layout (ewlc-output-layout *ewlc*)))
    ;; can return nil
    (wlr-output-layout-output-at output-layout cursor)))

(defun ewlc-output= (ewlc-output-1 ewlc-output-2)
  "Compare EWLC-OUTPUT-1 and EWLC-OUTPUT-2."
  (ewlc-output-ptr= (ewlc-output-ptr ewlc-output-1) (ewlc-output-ptr ewlc-output-2)))

(defun ewlc-set-next-output (direction)
  "Set the active output on the basis of the DIRECTION."
  (let* ((output-list (ewlc-output-list *ewlc*))
         (curr-ewlc-output (ewlc-active-output *ewlc*))
         (len (length output-list))
         (pos (cl-position curr-ewlc-output output-list :test 'ewlc-output=)))
    (setf (ewlc-active-output *ewlc*) (cond
                                       ((equal direction 'next)
                                        (if (< (+ pos 1) len)
                                            (nth (+ pos 1) output-list)
                                          (nth 0 output-list)))
                                       ((equal direction 'prev)
                                        (if (= pos 0)
                                            (nth (- len 1) output-list)
                                          (nth (- pos 1) output-list)))))))

(defun ewlc-arrange (ewlc-output)
  "Tile the visible clients on the EWLC-OUTPUT."
  (log-message (format "          ewlc-arrange: ewlc-output: '%s'" ewlc-output))
  (setf (ewlc-output-box ewlc-output) (wlr-output-layout-get-box (ewlc-output-layout *ewlc*)
                                                                 (ewlc-output-wlr-output ewlc-output)))
  (log-message (format "          ewlc-arrange: box: '%s'" (ewlc-output-box ewlc-output)))
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
         (oh (wlr-box-height output-box))
         (num-master (ewlc-output-num-master ewlc-output))
         (master-ratio (ewlc-output-master-ratio ewlc-output)))
    (log-message (format "          ewlc-arrange: check1"))
    (log-message (format "          ewlc-arrange: ox: '%s'" ox))
    (log-message (format "          ewlc-arrange: oy: '%s'" oy))
    (log-message (format "          ewlc-arrange: ow: '%s'" ow))
    (log-message (format "          ewlc-arrange: oh: '%s'" oh))
    ;; TODO: can I create the needed list with a filter ?
    (dolist (client (ewlc-client-list *ewlc*))
      (when (and (ewlc-client-visible-on-output-p client ewlc-output)
                 (not (ewlc-client-floating-p client)))
        (cl-incf n)))
    (log-message (format "          ewlc-arrange: check2"))
    (when (> n 0)
      (log-message (format "          ewlc-arrange: check3"))
      (if (> n num-master)
          (setq mw (if (> num-master 0)
                       (* ow master-ratio)
                     0))
        ;; FIXME: this is the error
        (setq mw ow)
        (log-message (format "          ewlc-arrange: mw: '%s'" mw))
        )
      (log-message (format "          ewlc-arrange: check4"))
      (dolist (client (ewlc-client-list *ewlc*))
        (log-message (format "          ewlc-arrange: check5"))
        (let* ((client-geom (ewlc-client-geom client))
               (ch (wlr-box-height client-geom)))
          (log-message (format "          ewlc-arrange: check6"))
          (when (and (ewlc-client-visible-on-output-p client ewlc-output)
                     (not (ewlc-client-floating-p client)))
            (log-message (format "          ewlc-arrange: check7"))
            (if (< i num-master)
                (progn
                  (setq h (/ (- oh my)
                             (- (min n num-master) i)))
                  (log-message (format "          ewlc-arrange: check8"))
                  (ewlc-resize client ox (+ oy my) mw h 0)
                  (setq my (+ my ch)))
              (setq h (/ (- oh ty) (- n i)))
              (ewlc-resize client (+ ox mw) (+ oy ty) (- ow mw) h 0)
              (setq my (+ my ch))))
          (cl-incf i)))))
  (log-message (format "          ewlc-arrange: end"))
  )

;; handlers ---------------------------------------------------

(defun ewlc-backend-new-output-handler (wlr-output)
  "This event is raised when new WLR-OUTPUT (a display) becomes available."
  ;; Set the output's preferred mode.
  (wlr-output-set-mode wlr-output (wlr-output-preferred-mode wlr-output))
  (let* ((output-ptr (ewlc-make-output-ptr (ewlc-ptr *ewlc*)))
         (output (make-ewlc-output :ptr output-ptr
                                   :wlr-output wlr-output
                                   :master-ratio 0.5
                                   :num-master 1
                                   :scale 2.0)))
    (wlr-output-set-scale wlr-output (ewlc-output-scale output))
    (wlr-xcursor-manager-load (ewlc-cursor-mgr *ewlc*) (ewlc-output-scale output))
    (wlr-output-set-transform wlr-output)
    (ewlc-output-set-listeners output-ptr wlr-output)
    (push output (ewlc-output-list *ewlc*))
    (wlr-output-enable wlr-output)
    (when (wlr-output-commit wlr-output)
      ;; The add-auto function arranges outputs from left-to-right in handled order.
      (wlr-output-layout-add-auto (ewlc-output-layout *ewlc*) wlr-output)
      (setf (ewlc-output-geom *ewlc*) (wlr-output-layout-get-box (ewlc-output-layout *ewlc*)
                                                                 wlr-output))
      (setf (ewlc-output-box output) (ewlc-output-geom *ewlc*))
      (log-message (format "    ewlc-backend-new-output-handler: box: '%s'" (ewlc-output-box output)))
      )))

(defun ewlc-output-frame-handler (ewlc-output)
  "EWLC-OUTPUT frame handler.
Called every time the output is ready to display a frame, generally at
the output's refresh rate."
  (log-message (format "    ewlc-output-frame-handler: start"))
  (let ((now (current-time))
        (render t)
        (renderer (ewlc-renderer *ewlc*))
        (wlr-output (ewlc-output-wlr-output ewlc-output))
        (color (ewlc-root-color *ewlc*)))
    (log-message (format "    ewlc-output-frame-handler: check1"))
    (dolist (client (ewlc-stack-client-list *ewlc*))
      (when (ewlc-client-resize-p client)
        (wlr-surface-send-frame-done (ewlc-client-wlr-surface client) now)
        ;; do not render if any xdg clients have an outstanding resize
        (setq render nil)))
    (log-message (format "    ewlc-output-frame-handler: check2"))
    (when (wlr-output-attach-render wlr-output)
      (when render
        (log-message (format "    ewlc-output-frame-handler: check2.1"))
        (wlr-renderer-begin renderer wlr-output)
        (log-message (format "    ewlc-output-frame-handler: check2.2"))
        ;; set background color
        (wlr-renderer-clear renderer color)
        (log-message (format "    ewlc-output-frame-handler: check2.3"))
        (ewlc-render-clients ewlc-output now)
        (log-message (format "    ewlc-output-frame-handler: check2.4"))
        (ewlc-render-independents wlr-output now)
        (log-message (format "    ewlc-output-frame-handler: check2.5"))
        ;; This function is a no-op when hardware cursors are in use.
        (wlr-output-render-software-cursors wlr-output)
        (log-message (format "    ewlc-output-frame-handler: check2.6"))
        (wlr-renderer-end renderer)))
    (log-message (format "    ewlc-output-frame-handler: check3"))
    (wlr-output-commit wlr-output)
    (log-message (format "    ewlc-output-frame-handler: end"))
    ))

(defun ewlc-output-destroy-handler (ewlc-output)
  "Handler for the destroy EWLC-OUTPUT signal."
  (setf (ewlc-output-list *ewlc*) (cl-remove ewlc-output
                                             (ewlc-output-list *ewlc*)
                                             :test 'ewlc-output=))
  (ewlc-free (ewlr-output-ptr ewlc-output)))

;; ------------------------------------------------------------
;; client
;; ------------------------------------------------------------

(defun ewlc-client= (client-a client-b)
  "Return t if CLIENT-A and CLIENT-B are equal, else return nil."
  (log-message (format "      ewlc-client=: ptr-a: '%s'" (ewlc-client-ptr client-a)))
  (log-message (format "      ewlc-client=: ptr-b: '%s'" (ewlc-client-ptr client-a)))
  (log-message (format "      ewlc-client=: =: '%s'" (ewlc-client-ptr= (ewlc-client-ptr client-a) (ewlc-client-ptr client-b))))
  (ewlc-client-ptr= (ewlc-client-ptr client-a) (ewlc-client-ptr client-b))
  )

(defun ewlc-apply-title (client ewlc-output)
  "Apply title to CLIENT on EWLC-OUTPUT."
  (log-message (format "      ewlc-apply-title: start"))
  (pcase (ewlc-client-type client)
    ('xdg-shell
     (log-message (format "      ewlc-apply-title: xdg-shell"))
     (let ((surface (ewlc-client-xdg-surface client)))
       (log-message (format "      ewlc-apply-title: surface: '%s'" surface))
       (setf (ewlc-client-app-id client) (wlr-xdg-surface-get-app-id surface))
       (setf (ewlc-client-title client) (wlr-xdg-surface-get-title surface))))
    (_
     (let ((surface (ewlc-client-xwayland-surface client)))
       (setf (ewlc-client-app-id client) (wlr-xwayland-surface-get-class surface))
       (setf (ewlc-client-title client) (wlr-xwayland-surface-get-title surface)))))
  (log-message (format "      ewlc-apply-title: check1"))
  (when (not (ewlc-client-app-id client)) (setf (ewlc-client-app-id client) "broken"))
  (log-message (format "      ewlc-apply-title: check2"))
  (when (not (ewlc-client-title client)) (setf (ewlc-client-client client) "broken"))
  (log-message (format "      ewlc-apply-title: check2"))
  (ewlc-set-output client ewlc-output)
  (log-message (format "      ewlc-apply-title: end"))
  )

(defun ewlc-client-wlr-surface (client)
  "Get the wlr_surface for the CLIENT's wlr_xdg_surface or wlr_xwayland surface."
  (if (equal (ewlc-client-type client) 'xdg-shell)
      (wlr-xdg-surface-get-wlr-surface (ewlc-client-xdg-surface client))
    (wlr-xwayland-surface-get-wlr-surface (ewlc-client-xwayland-surface client))))

(defun ewlc-render-clients (output time)
  "Render the clients on OUTPUT at TIME."
  (log-message (format "        ewlc-render-clients: output: '%s'" output))
  (log-message (format "        ewlc-render-clients: time: '%s'" time))
  (let ((active-client (ewlc-get-active-client)))
    ;; Each subsequent window we render is rendered on top of the last. Because
    ;; our stacking list is ordered front-to-back, we iterate over it backwards.
    (dolist (client (reverse (ewlc-stack-client-list *ewlc*)))
      (log-message (format "        ewlc-render-clients: client: '%s'" client))
      (when (or (ewlc-client-visible-on-output-p client output)
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
          (log-message (format "        ewlc-render-clients: check1"))
          (log-message (format "        ewlc-render-clients: surface: '%s'" (ewlc-client-wlr-surface client)))
          (log-message (format "        ewlc-render-clients: orig-x: '%s'" orig-x))
          (log-message (format "        ewlc-render-clients: orig-y: '%s'" orig-y))
          (log-message (format "        ewlc-render-clients: bw: '%s'" bw))
          (log-message (format "        ewlc-render-clients: w: '%s'" w))
          (log-message (format "        ewlc-render-clients: h: '%s'" h))
          (log-message (format "        ewlc-render-clients: layout: '%s'" (ewlc-output-layout *ewlc*)))
          (log-message (format "        ewlc-render-clients: wlr-output: '%s'" (ewlc-output-wlr-output output)))
          (log-message (format "        ewlc-render-clients: output-coords: '%s'" output-coords))
          (log-message (format "        ewlc-render-clients: check2"))
          ;; render the client borders
          (dolist (border border-list)
            (cl-destructuring-bind (x y w h) border
              (log-message (format "        ewlc-render-clients: ('%s', '%s', '%s', '%s')" x y w h))
              (let ((border-box (wlr-box-create x y w h)))
                (wlr-scale-box border-box (ewlc-output-scale output))
                (wlr-render-rect (ewlc-renderer *ewlc*)
                                 border-box
                                 border-color
                                 (ewlc-output-wlr-output output)))))
          (log-message (format "        ewlc-render-clients: check3"))
          ;; render the client surfaces

          (let* ((x (+ (wlr-box-x (ewlc-client-geom client)) bw))
                 (y (+ (wlr-box-y (ewlc-client-geom client)) bw))
                 )
            (log-message (format "        ewlc-render-clients: x: '%s'" x))
            (log-message (format "        ewlc-render-clients: y: '%s'" y))
            (log-message (format "        ewlc-render-clients: layout: '%s'" (ewlc-output-layout *ewlc*)))
            (log-message (format "        ewlc-render-clients: wlr-output: '%s'" (ewlc-output-wlr-output output)))
            (log-message (format "        ewlc-render-clients: renderer: '%s'" (ewlc-renderer *ewlc*)))
            (log-message (format "        ewlc-render-clients: time: '%s'" time))
            (log-message (format "        ewlc-render-clients: check3.1"))
            )
          (let* ((x (+ (wlr-box-x (ewlc-client-geom client)) bw))
                 (y (+ (wlr-box-y (ewlc-client-geom client)) bw))
                 (render-data (ewlc-create-render-data (ewlc-output-wlr-output output)
                                                       (ewlc-output-layout *ewlc*)
                                                       (ewlc-renderer *ewlc*)
                                                       x y time))
                 )
            (log-message (format "        ewlc-render-clients: check3.2"))
            (if (equal (ewlc-client-type client) 'xdg-shell)
                (progn
                  (log-message (format "        ewlc-render-clients: check3.3"))
                  (wlr-xdg-surface-for-each-surface-render (ewlc-client-xdg-surface client)
                                                           render-data)
                  (log-message (format "        ewlc-render-clients: check3.4"))
                  )
              (wlr-surface-for-each-surface-render (wlr-xwayland-surface-get-wlr-surface
                                                    (ewlc-client-xwayland-surface client))
                                                   render-data)))))))
  (log-message (format "        ewlc-render-clients: end"))
  )

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
          (wlr-surface-for-each-surface-render (wlr-xwayland-surface-get-wlr-surface surface)
                                               render-data))))))

(defun ewlc-get-top-client-focus (ewlc-output)
  "Get the top focused client on the EWLC-OUTPUT."
  (cl-dolist (client (ewlc-focus-client-list *ewlc*))
    (when (ewlc-client-visible-on-output-p client ewlc-output)
      (cl-return client))))

(defun ewlc-set-floating (client floating-p)
  "Set the CLIENT to floating depending on FLOATING-P."
  (when (not (equal (ewlc-client-floating-p client) floating-p))
    (setf (ewlc-client-floating-p client) floating-p)
    (ewlc-arrange (ewlc-client-output client))))


(defun ewlc-set-output (client target-output)
  "Set CLIENT to TARGET-OUTPUT."
  (log-message (format "        ewlc-set-output: client: '%s'" client ))
  (log-message (format "        ewlc-set-output: target-output: '%s'" target-output))
  (let* ((curr-output (ewlc-client-output client))
         (client-wlr-surface (ewlc-get-wlr-surface client))
         new-client)
    (log-message (format "        ewlc-set-output: curr-output: '%s'" (ewlc-client-output client)))
    (log-message (format "        ewlc-set-output: client-surface: '%s'" client-wlr-surface))
    (when (or (and target-output (not curr-output))
              (and target-output (not (ewlc-output= target-output curr-output))))
      (log-message (format "        ewlc-set-output: check1"))
      (setf (ewlc-client-output client) target-output)
      (when curr-output
        (log-message (format "        ewlc-set-output: curr-output: '%s'" curr-output))
        (let ((curr-wlr-output (ewlc-output-wlr-output curr-output)))
          (wlr-surface-send-leave client-wlr-surface curr-wlr-output)
          (ewlc-arrange curr-output)))
      (when target-output
        (log-message (format "        ewlc-set-output: target-output: '%s'" target-output))
        (let ((target-wlr-output (ewlc-output-wlr-output target-output)))
          (log-message (format "        ewlc-set-output: target-wlr-output: '%s'" target-wlr-output))
          (log-message (format "        ewlc-set-output: output-box: '%s'" (ewlc-output-box target-output)))
          (ewlc-apply-bounding-box client (ewlc-output-box target-output))
          (log-message (format "        ewlc-set-output: check2"))
          (wlr-surface-send-enter client-wlr-surface target-wlr-output)
          (log-message (format "        ewlc-set-output: check3"))
          (ewlc-arrange target-output)))
      (log-message (format "        ewlc-set-output: check4"))
      (setq new-client (ewlc-get-top-client-focus (ewlc-active-output *ewlc*)))
      (log-message (format "        ewlc-set-output: new-client: '%s'" new-client))
      (ewlc-focus-client client new-client t)))
  (log-message (format "        ewlc-set-output: end"))
  )

(defun ewlc-pointer-focus (client surface x y time-msec)
  "Focus the CLIENT or SURFACE at the pointer X Y coordinates at TIME-MSEC."
  (when (and client (not surface))
    (setq surface (ewlc-get-wlr-surface client))
    (let* ((seat (ewlc-seat *ewlc*))
           (pointer-focused-surface (wlr-seat-pointer-focused-surface seat)))
      (cond ((not surface)
             ;; there is no surface, so clear pointer focus
             (wlr-seat-pointer-notify-clear-focus (ewlc-seat *ewlc*)))
            ((wlr-surface= surface pointer-focused-surface)
             ;; surface is already focused. Notify of motion.
             (wlr-seat-pointer-notify-motion seat time-msec x y))
            (t
             (wlr-seat-pointer-notify-enter seat surface x y)
             (when (and (ewlc-sloppy-focus *ewlc*) (equal (ewlc-client-type client) 'x11-unmanaged))
               (ewlc-focus-client (ewlc-get-active-client) client nil)))))))

(defun ewlc-get-client-at-point (cursor)
  "Get the client at the CURSOR point."
  (cl-dolist (client (ewlc-stack-client-list *ewlc*))
    (when (and (ewlc-client-visible-on-output-p client (ewlc-client-output client))
               (wlr-box-contains-point (ewlc-client-geom client) cursor))
      (cl-return client))))

(defun ewlc-get-wlr-surface (client)
  "Get the wlr_surface for the CLIENT."
  (pcase (ewlc-client-type client)
    ('xdg-shell
     (wlr-xdg-surface-get-wlr-surface (ewlc-client-xdg-surface client)))
    (_
     (wlr-xwayland-surface-get-wlr-surface (ewlc-client-xwayland-surface client)))))

(defun ewlc-apply-bounding-box (client bounding-box)
  "Apply BOUNDING-BOX to CLIENT geometry."
  (log-message (format "          ewlc-apply-bounding-box: client: '%s'" client))
  (log-message (format "          ewlc-apply-bounding-box: bounding-box: '%s'" bounding-box))
  (let ((client-geom (ewlc-client-geom client))
        (bw (ewlc-border-width *ewlc*))
        (x (wlr-box-x bounding-box))
        (y (wlr-box-y bounding-box))
        (width (wlr-box-width bounding-box))
        (height (wlr-box-height bounding-box)))
    (log-message (format "          ewlc-apply-bounding-box: x: '%s'" x))
    (log-message (format "          ewlc-apply-bounding-box: y: '%s'" y))
    (wlr-set-box-width client-geom (max 1 (wlr-box-width client-geom)))
    (wlr-set-box-height client-geom (max 1 (wlr-box-height client-geom)))
    (when (>= (wlr-box-x client-geom) (+ x width))
      (wlr-set-box-x client-geom (- (+ x width) (wlr-box-width client-geom))))
    (when (>= (wlr-box-y client-geom) (+ y height))
      (wlr-set-box-y client-geom (- (+ y height) (wlr-box-height client-geom))))
    (when (<= (+ (wlr-box-x client-geom) (wlr-box-width client-geom) (* 2 bw)) x)
      (wlr-set-box-x client-geom x))
    (when (<= (+ (wlr-box-y client-geom) (wlr-box-height client-geom) (* 2 bw)) y)
      (wlr-set-box-y client-geom y)))
  (log-message (format "          ewlc-apply-bounding-box: end"))
  )


(defun ewlc-client-visible-on-output-p (client output)
  "Return t if CLIENT is visible on OUTPUT."
  (ewlc-output= (ewlc-client-output client) output))

(defun ewlc-get-active-client ()
  "Get the active client from focus-client-list that visible on the output."
  (let ((output (ewlc-active-output *ewlc*))
        (client (car (ewlc-focus-client-list *ewlc*))))
    (if (and client (ewlc-client-visible-on-output-p client output))
        client
      nil)))

(cl-defun ewlc-focus-client (old-client new-client raise-p)
  "Focus the NEW-CLIENT and raise if RAISE-P is t, unfocus the OLD-CLIENT."
  (log-message (format "          ewlc-focus-client: old-client: '%s'" old-client))
  (log-message (format "          ewlc-focus-client: new-client: '%s'" new-client))
  (log-message (format "          ewlc-focus-client: raise-p: '%s'" raise-p))
  (when (and new-client raise-p)
    (log-message (format "          ewlc-focus-client: stack-client-list '%s'" (ewlc-stack-client-list *ewlc*)))
    (setf (ewlc-stack-client-list *ewlc*)
          (cl-remove new-client (ewlc-stack-client-list *ewlc*) :test 'ewlc-client=))
    (log-message (format "          ewlc-focus-client: check1"))
    (push new-client (ewlc-stack-client-list *ewlc*)))
  (log-message (format "          ewlc-focus-client: stack-client-list '%s'" (ewlc-stack-client-list *ewlc*)))
  (log-message (format "          ewlc-focus-client: compare: '%s'" (ewlc-client= old-client new-client)))
  (when (ewlc-client= old-client new-client)
    ;; nothing to do
    (log-message (format "          ewlc-focus-client: return-from"))
    (cl-return-from ewlc-focus-client))
  (log-message (format "          ewlc-focus-client: check2"))
  (when (and old-client
             (not (ewlc-client= old-client new-client)))
    ;; de-activate old client
    (if (equal (ewlc-client-type old-client) 'xdg-shell)
        (wlr-xdg-toplevel-set-activated (ewlc-client-xdg-surface old-client) 0)
      (wlr-xwayland-surface-activate (ewlc-client-xwayland-surface old-client) 0)))
  (when (not new-client)
    ;; no new client - so just clear focus.
    (wlr-seat-keyboard-notify-clear-focus (ewlc-seat *ewlc*))
    (cl-return-from ewlc-focus-client))
  (let* ((seat (ewlc-seat *ewlc*))
         (keyboard (wlr-seat-get-keyboard seat)))
    ;; there is a new-client - so focus it
    (wlr-set-keyboard-notify-enter seat (ewlc-client-wlr-surface new-client) keyboard)
    ;; put client at top of focus list
    (setf (ewlc-focus-client-list *ewlc*)
          (cl-remove new-client (ewlc-focus-client-list *ewlc*) :test 'ewlc-client=))
    (setf (ewlc-focus-client-list *ewlc*) (push new-client (ewlc-focus-client-list *ewlc*))))
  ;; activate new client
  (if (equal (ewlc-client-type old-client) 'xdg-shell)
      (wlr-xdg-toplevel-set-activated (ewlc-client-xdg-surface old-client) 0)
    (wlr-xwayland-surface-activate (ewlc-client-xwayland-surface old-client) 0))
  (log-message (format "          ewlc-focus-client: end"))
  )


(defun ewlc-resize (client x y width height interact-p)
  "Resize to CLIENT to X, Y, WIDTH, HEIGHT depending on INTERACT-P.
In a more fleshed-out compositor, wait for the client to prepare a buffer
at the new size, then commit any movement that was prepared."
  (log-message (format "            ewlc-resize: client: '%s'" client))
  (log-message (format "            ewlc-resize: x: '%s'" x))
  (log-message (format "            ewlc-resize: y: '%s'" y))
  (log-message (format "            ewlc-resize: width: '%s'" width))
  (log-message (format "            ewlc-resize: height: '%s'" height))
  (log-message (format "            ewlc-resize: interact-p: '%s'" interact-p))
  (let* ((client-geom (ewlc-client-geom client))
         (bbox (if interact-p
                   (ewlc-output-geom *ewlc*)
                 client-geom)))
    (wlr-set-box-x client-geom x)
    (wlr-set-box-y client-geom y)
    (wlr-set-box-width client-geom width)
    (wlr-set-box-height client-geom height)
    (ewlc-apply-bounding-box client bbox))
  (log-message (format "            ewlc-resize: check1"))
  (let* ((border-width (ewlc-border-width *ewlc*))
         (bclient-geom (ewlc-client-geom client))
         (bx (wlr-box-x bclient-geom))
         (by (wlr-box-y bclient-geom))
         (bwidth (wlr-box-width bclient-geom))
         (bheight (wlr-box-height bclient-geom)))
    (log-message (format "            ewlc-resize: check2"))
    (pcase (ewlc-client-type client)
      ('xdg-shell
       (log-message (format "            ewlc-resize: check3"))
       (log-message (format "            ewlc-resize: xdg-surface: '%s'" (ewlc-client-xdg-surface client)))
       (log-message (format "            ewlc-resize: bwidth: '%s'" bwidth))
       (log-message (format "            ewlc-resize: bheight: '%s'" bheight))
       (log-message (format "            ewlc-resize: border-width: '%s'" border-width))
       (wlr-xdg-toplevel-set-size (ewlc-client-xdg-surface client)
                                  (- bwidth (* 2 border-width))
                                  (- bheight (* 2 border-width)))
       (log-message (format "            ewlc-resize: check3.1"))
       )
      (_
       (log-message (format "            ewlc-resize: check4"))
       (wlr-xwayland-surface-configure (ewlc-xwayland-surface client)
                                       bx
                                       by
                                       (- bwidth (* 2 border-width))
                                       (- bheight (* 2 border-width)))
       )))
  (log-message (format "            ewlc-resize: interact-p: '%s'" interact-p))
  )

(defun ewlc-get-independent-at-point (cursor)
  "Get the xwayland client a CURSOR point."
  (dolist (client (ewlc-independent-list *ewlc*))
    (let ((geom (wlr-xwayland-surface-get-wlr-box (ewlc-client-xwayland-surface client))))
      (when (wlr-box-contains-point geom cursor)
        (cl-return client)))))

;; handlers ---------------------------------------------------

(defun ewlc-surface-map-handler (client)
  "Called when the CLIENT surface is mapped, or ready to display on-screen."
  (log-message (format "    ewlc-surface-map-handler: client: '%s'" client))
  (log-message (format "    ewlc-surface-map-handler: client-type: '%s'" (ewlc-client-type client)))
  (if (equal (ewlc-client-type client) 'x11-unmanaged)
      (push client (ewlc-independent-list *ewlc*))
    (push client (ewlc-client-list *ewlc*))
    (push client (ewlc-stack-client-list *ewlc*))
    (push client (ewlc-focus-client-list *ewlc*)))
  (log-message (format "    ewlc-surface-map-handler: check1"))
  (pcase (ewlc-client-type client)
    ;; TODO: what about geometry of unmanaged clients - do nothing?
    ('x11-managed
     (log-message (format "    ewlc-surface-map-handler: check2"))
     (let ((geom (ewlc-client-geom client))
           (surface (ewlc-client-surface-xwayland client))
           (border-width (ewlc-border-width *ewlc*)))
       (wlr-set-box-x geom (wlr-box-x surface))
       (wlr-set-box-y geom (wlr-box-y surface))
       (wlr-set-box-width geom (+ (* 2 border-width) (wlr-box-width surface)))
       (wlr-set-box-height geom (+ (* 2 border-width) (wlr-box-height surface)))))
    ('xdg-shell
     (log-message (format "    ewlc-surface-map-handler: check3"))
     (log-message (format "    ewlc-surface-map-handler: xdg-surface: '%s'" (ewlc-client-xdg-surface client)))
     (log-message (format "    ewlc-surface-map-handler: geom: '%s'" (wlr-xdg-surface-get-geometry (ewlc-client-xdg-surface client))))
     (let ((geom (wlr-xdg-surface-get-geometry (ewlc-client-xdg-surface client)))
           (bw (ewlc-border-width *ewlc*)))
       (log-message (format "    ewlc-surface-map-handler: check3.1"))
       (log-message (format "    ewlc-surface-map-handler: width: '%s'" (wlr-box-width geom)))
       (log-message (format "    ewlc-surface-map-handler: bw: '%s'" bw))
       (wlr-set-box-width geom (+ (wlr-box-width geom) (* 2 bw)))
       (log-message (format "    ewlc-surface-map-handler: check3.2"))
       (wlr-set-box-height geom (+ (wlr-box-height geom) (* 2 bw)))
       (log-message (format "    ewlc-surface-map-handler: check3.3"))
       (setf (ewlc-client-geom client) geom))))
  (log-message (format "    ewlc-surface-map-handler: check4"))
  (log-message (format "    Is this correct that there is no active output?"))
  (log-message (format "    +++++++++++++++++++++++++++++++++++++++++++++++"))
  (log-message (format "    ewlc-surface-map-handler: output: '%s'" (ewlc-active-output *ewlc*)))
  (ewlc-apply-title client (ewlc-active-output *ewlc*))
  (log-message (format "    ewlc-new-xdg-shell-surface-handler: end"))
  )


(defun ewlc-new-xdg-shell-surface-handler (xdg-surface)
  "This event is raised when there is a new XDG-SURFACE.
The event is from a client, either a toplevel (application window) or popup."
  (log-message (format "    ewlc-new-xdg-shell-surface-handler: xdg-surface: '%s'" xdg-surface))
  ;; TODO: return if not toplevel?
  (when (wlr-xdg-surface-role-toplevel xdg-surface)
    (let* ((client-ptr (ewlc-make-xdg-surface-client-ptr (ewlc-ptr *ewlc*)))
           (client (make-ewlc-client :ptr client-ptr
                                     :xdg-surface xdg-surface
                                     :type 'xdg-shell
                                     :floating-p nil)))
      (wlr-xdg-toplevel-set-tiled (ewlc-client-xdg-surface client))
      (ewlc-set-xdg-surface-client-listeners (ewlc-client-ptr client)
                                             (ewlc-client-xdg-surface client))
      ;; create client - but store it in unmapped list until it gets mapped.
      (push client (ewlc-unmapped-client-list *ewlc*))))
  (log-message (format "    ewlc-new-xdg-shell-surface-handler: client-list: '%s'"
                       (ewlc-unmapped-client-list *ewlc*)))
  (log-message (format "    ewlc-new-xdg-shell-surface-handler: end"))
  )

(defun ewlc-new-xwayland-surface-handler (xwayland-surface)
  "This event is raised when there is a new XWAYLAND-SURFACE."
  (let* ((client-ptr (ewlc-make-xwayland-surface-client-ptr (ewlc-ptr *ewlc*)))
         (client (make-ewlc-client :ptr client-ptr
                                   :xwayland-surface xwayland-surface
                                   :type (if (wlr-xwayland-surface-override-redirect)
                                             'x11-unmanaged
                                           'x11-managed)
                                   :floating-p nil)))
    (ewlc-set-xwayland-surface-client-listeners (ewlc-client-ptr client)
                                                (ewlc-client-xwayland-surface client))
    ;; create client - but store it in unmapped list until it gets mapped.
    (push client (ewlc-unmapped-client-list client))))


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
  ;; TODO: also remove from the unmanaged client list.
  (setf (ewlc-client-list *ewlc*) (cl-remove client (ewlc-client-list *ewlc*) :test 'ewlc-client=))
  (unless (equal (ewlc-client-type client) 'x11-unmanaged)
    (let* ((output (ewlc-client-output client))
           (new-focused-client (ewlc-get-top-client-focus output)))
      (setf (ewlc-client-focus-list *ewlc*)
            (cl-remove client (ewlc-client-focus-list *ewlc*) :test 'ewlc-client=))
      (setf (ewlc-stack-client-list *ewlc*)
            (cl-remove client (ewlc-stack-client-list *ewlc*) :test 'ewlc-client=))
      (ewlc-arrange output)
      (ewlc-focus-client nil new-focused-client t))))

;; ------------------------------------------------------------
;; pointer
;; ------------------------------------------------------------

(defun ewlc-action-move-resize (cursor-mode)
  "Apply action depending on if CURSOR-MODE is ewlc-cursor-move, or ewlc-cursor-resize."
  (setf (ewlc-grabbed-client *ewlc*) (ewlc-get-client-at-point (ewlc-cursor *ewlc*)))
  (when-let ((client (ewlc-grabbed-client *ewlc*)))
    (setf (ewlc-cursor-mode *ewlc*) cursor-mode)
    ;; float the window and grab it for move/resize
    (ewlc-set-floating client t)
    (let ((client-x (ewlc-client-x client))
          (client-y (ewlc-client-y client))
          (client-h (ewlc-client-height client))
          (client-w (ewlc-client-width client))
          (cursor-x (ewlc-cursor-x (ewlc-cursor *ewlc*)))
          (cursor-y (ewlc-cursor-y (ewlc-cursor *ewlc*))))
      (pcase cursor-mode
        ('ewlc-cursor-move
         (setf (ewlc-grabbed-client-x *ewlc*) (- cursor-x client-x))
         (setf (ewlc-grabbed-client-y *ewlc*) (- cursor-y client-y))
         (wlr-xcursor-manager-set-cursor-image (ewlc-cursor-mgr *ewlc*) "fleur" (ewlc-cursor *ewlc*)))
        ('elwc-cursor-resize
         ;; FIXME: does not work for X windows
         (wlr-cursor-warp-closest (ewlc-cursor *ewlc*) (+ client-x client-w) (+ client-y client-h))
         (wlr-xcursor-manager-set-cursor-image (ewlc-cursor-mgr *ewlc*)
                                               "bottom_right_corner"
                                               (ewlc-cursor *ewlc*)))))))

(defun ewlc-apply-button-action (wlr-event-pointer-button)
  "Apply the button action for the WLR-EVENT-POINTER-BUTTON."
  (let* ((keyboard (wlr-seat-get-keyboard (ewlc-seat *ewlc*)))
         (mods (wlr-keyboard-get-modifiers keyboard))
         (button (wlr-event-pointer-button-get-button wlr-event-pointer-button)))
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
  "The motion/resize of a client due to cursor miotion at TIME-MSEC."
  (when (ewlc-sloppy-focus *ewlc*)
    ;; Update active output as part of a client drag.
    (setf (ewlc-active-output *ewlc*) (ewlc-get-ewlc-output-at-point (ewlc-cursor *ewlc*))))
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
    (pcase (ewlc-cursor-mode *ewlc*)
      ('ewlc-cursor-move
       (ewlc-resize grabbed-client
                    (- cursor-x grabbed-client-x)
                    (- cursor-y grabbed-client-y)
                    grabbed-client-width
                    grabbed-client-height
                    t))
      ('ewlc-cursor-resize
       (ewlc-resize grabbed-client
                    grabbed-client-x
                    grabbed-client-y
                    (- cursor-x grabbed-client-x)
                    (- cursor-y grabbed-client-y)
                    t))
      (_
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
             (wlr-xcursor-manager-set-cursor-image cursor "left_ptr" (ewlc-cursor-mgr *ewlc*)))
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
  (unless (equal (ewlc-cursor-mode *ewlc*) 'ewlc-cursor-normal)
    (wlr-cursor-set-surface (ewlc-cursor *ewlc*) (ewlc-seat *ewlc*) event)))

(defun ewlc-pointer-axis-handler (wlr-event-pointer-axis)
  "Handle an WLR-EVENT-POINTER-AXIS forwarded by the pointer (e.g. scroll wheel movement)."
  (wlr-seat-pointer-notify-axis (ewlc-seat *ewlc*) wlr-event-pointer-axis))

(defun ewlc-pointer-button-handler (event)
  "Handle an button EVENT forwarded by the pointer."
  (let ((handled nil))
    (pcase (wlr-get-button-press-state event)
      ('wlr-button-pressed
       ;; change focus if the button is pressed over a client,
       ;; then start the button action
       (let ((new-client (ewlc-get-client-at-point (ewlc-cursor *ewlc*)))
             (curr-client (ewlc-get-active-client)))
         (ewlc-focus-client curr-client new-client t)
         (setq handled (ewlc-apply-button-action event))))
      ('wlr-button-released
       (unless (equal (ewlc-cursor-mode *ewlc*) 'ewlc-cursor-normal)
         ;; exit move/resize and reset the cursor
         (wlr-xcursor-manager-set-cursor-image (ewlc-cursor-mgr *ewlc*)
                                               "left_ptr"
                                               (ewlc-cursor *ewlc*))
         (setf (ewlc-cursor-mode *ewlc*) 'ewlc-cursor-normal)
         (setf (ewlc-active-output *ewlc*) (ewlc-get-ewlc-output-at-point (ewlc-cursor *ewlc*)))
         (ewlc-set-output (ewlc-grabbed-client *ewlc*) (ewlc-active-output *ewlc*))
         (setq handled t)))
      (_
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
  (let* ((keyboard-ptr (ewlc-make-keyboard-ptr (ewlc-ptr *ewlc*)))
         (keyboard (make-ewlc-keyboard :ptr keyboard-ptr
                                       :device wlr-input-device)))
    (ewlc-keyboard-set-listeners (ewlc-keyboard-ptr keyboard)
                                 (ewlc-keyboard-device keyboard))
    (wlr-seat-set-keyboard (ewlc-seat *ewlc*) (ewlc-keyboard-device keyboard))
    (push keyboard (ewlc-keyboard-list *ewlc*))))

(defun ewlc-keyboard= (keyboard-a keyboard-b)
  "Compare KEYBOARD-A and KEYBOARD-B and return t if equal."
  (ewlc-keyboard-ptr= (ewlc-keyboard-ptr keyboard-a) (ewlc-keyboard-ptr keyboard-b)))

;; keyboard event handlers ---------------------------------------------

(defun ewlc-keyboard-destroy-handler (ewlc-keyboard)
  "Destroy EWLC-KEYBOARD."
  (setf (ewlc-keyboard-list *ewlc*) (cl-remove ewlc-keyboard
                                               (ewlc-keyboard-list *ewlc*)
                                               :test 'ewlc-keyboard=))
  (ewlc-free (ewlc-keyboard-ptr ewlc-keyboard)))

(defun ewlc-keyboard-key-handler (ewlc-keyboard wlr-event-keyboard-key)
  "Handle WLR-EVENT-KEYBOARD-KEY for EWLC-KEYBOARD."
  (let* ((event wlr-event-keyboard-key)
         (device (ewlc-keyboard-device ewlc-keyboard))
         (seat (ewlc-seat *ewlc*))
         (wlr-keyboard (wlr-input-device-get-keyboard device))
         ;; convert from libinput keycode to xkbcommon
         (event-keycode (+ 8 (wlr-event-keyboard-key-get-keycode event)))
         (event-state (wlr-event-keyboard-key-get-state event))
         (event-time-msec (wlr-event-keyboard-key-get-time-msec event))
         (key-list (xkb-state-key-get-syms wlr-keyboard event-keycode))
         (mods (wlr-keyboard-get-modifiers wlr-keyboard)))
    (if (and (equal event-state 'wlr-key-pressed)
             (equal mods (ewlc-modifier-key *ewlc*)))
        (dolist (key key-list)
          (unless (ewlc-apply-keybinding (concat mods "-" key))
            ;; pass non-handled keys to client that use the modifier
            (wlr-seat-set-keyboard seat device)
            (wlr-seat-keyboard-notify-key seat event-time-msec event-keycode event-state)))
      ;; pass non-modifier keys to the client
      (wlr-seat-set-keyboard seat device)
      (wlr-seat-keyboard-notify-key seat event-time-msec event-keycode event-state))))

(defun ewlc-keyboard-modifiers-handler (ewlc-keyboard)
  "Handle modifier key press on EWLC-KEYBOARD. Communicate press to the client."
  (let ((seat (ewlc-seat *ewlc*))
        (device (ewlc-keyboard-device ewlc-keyboard)))
    (wlr-seat-set-keyboard seat device)
    (wlr-seat-keyboard-notify-modifiers seat device)))

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
       (wlr-seat-set-capabilites (ewlc-seat *ewlc*) (not (eq (ewlc-keyboard-list *ewlc*) nil))))
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
      (when (ewlc-client-visible-on-output-p c output)
        (setq prev-client c)
        (cl-return)))
    (when (not prev-client)
      (cl-dolist (c (nreverse after-pos-list))
        (when (ewlc-client-visible-on-output-p c output)
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
      (when (ewlc-client-visible-on-output-p c output)
        (setq next-client c)
        (cl-return)))
    (when (not next-client)
      (cl-dolist (c before-pos-list)
        (when (ewlc-client-visible-on-output-p c output)
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

(defun ewlc-command-remove-master ()
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
      (if (equal (ewlc-client-type client) 'xdg-shell)
          (wlr-xdg-toplevel-send-close (ewlc-client-xdg-surface client))
        (wlr-xwayland-surface-close (ewlc-client-xwayland-surface client))))))

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
    (define-key map (kbd "M-n") #'ewlc-command-focus-next-client)
    (define-key map (kbd "M-p") #'ewlc-command-focus-prev-client)
    (define-key map (kbd "M-i") #'ewlc-command-incr-master-ratio)
    (define-key map (kbd "M-d") #'ewlc-command-decr-master-ratio)
    (define-key map (kbd "M-m") #'ewlc-command-add-master)
    (define-key map (kbd "M-b") #'ewlc-command-remove-master)
    (define-key map (kbd "M-k") #'ewlc-command-kill-client)
    ;;    (define-key map (kbd "M-t") #'ewlc-command-terminal)
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

(defun ewlc-loop ()
  "The ewlc loop function."
  (let ((count 0))
    (while (and (ewlc-running-p *ewlc*) (< count 20))
      (log-message (format "before: ewlc-handle-events"))
      (ewlc-handle-events)
      (log-message (format "before: ewlc-display-dispatch"))
      (ewlc-display-dispatch)
      (log-message (format "before: sleep-for"))
      (sleep-for 0.1)
      (log-message (format "end of while: count: '%d'" count))
      (log-message (format "================================"))
      ;; (sleep-for 0.01)
      (cl-incf count))))

(defun ewlc-start-compositor ()
  "Start the wayland compositor."
  (create-log-file)
  (ewlc-start "alacritty")
  (setf (ewlc-running-p *ewlc*) t)
  (setq *ewlc-thread* (make-thread #'ewlc-loop "loop-thread"))
  )

(provide 'ewlc-server)
;;; ewlc-server.el ends here
