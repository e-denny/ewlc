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

(cl-defstruct ewlc
  server

  ;; input devices
  keyboard-list

  ;; outputs
  output-list
  active-output

  ;; clients
  client-list
  running-p)

(provide 'ewlc-server)
;;; ewlc-server.el ends here
