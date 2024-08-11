;;; zenicb-history.el --- keep a history of commands in ZenICB

;; Copyright (C) 1997, 1999 Faried Nawaz

;; Author: Faried Nawaz <fn@Hungry.COM>
;; Based almost entirely on zenirc-history.el by
;;      Per Persson <pp@gnu.ai.mit.edu>
;; Maintainer: <fn-icb@LISP-READER.Hungry.COM>
;; Keywords: zenicb, revisionistic history
;; Created: 97-03-17 (will this become 100-..-.. in three years?)

;; $Id: zenicb-history.el,v 1.6 1999/01/04 09:55:58 fn Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:


(require 'zenicb)

;; default is "/server" because that's probably the first thing you want to do
(defvar zenicb-history-list '("/m server ")
  "*A list of commands run by the user.")
(make-variable-buffer-local 'zenicb-history-list)

;; hairy variables to keep track of command
(defvar zenicb-history-list-backward nil)
(make-variable-buffer-local 'zenicb-history-list-backward)
(defvar zenicb-history-list-forward nil)
(make-variable-buffer-local 'zenicb-history-list-forward)
(defvar zenicb-history-list-current nil)
(make-variable-buffer-local 'zenicb-history-list-current)

;; reset hairy variables when a new command is sent to the server
(defun zenicb-history-command (command)
  (setq zenicb-history-list-backward nil
        zenicb-history-list-forward nil
        zenicb-history-list-current nil
        zenicb-history-list (cons command zenicb-history-list)))

;; step up one entry in the history list
(defun zenicb-history-backward ()
  (interactive)
  (if (not zenicb-history-list-backward)
      ; initialize variables if their reset
      (setq zenicb-history-list-backward zenicb-history-list
            zenicb-history-list-current
            (buffer-substring zenicb-process-mark (point-max))
            zenicb-history-list-forward zenicb-history-list-forward))
  ; remove contents of line
  (beginning-of-line)
  (if (not (= (point) (point-max)))
      (delete-backward-char (- (point) (point-max))))
  ; insert previous command
  (insert (car zenicb-history-list-backward))
  ; update hairy variables
  (setq zenicb-history-list-forward (cons
                                     zenicb-history-list-current
                                     zenicb-history-list-forward)
        zenicb-history-list-current (car zenicb-history-list-backward)
        zenicb-history-list-backward (cdr zenicb-history-list-backward)))

;; step down one entry in the history list
(defun zenicb-history-forward ()
  (interactive)
  (if (not zenicb-history-list-forward)
      ; reset variables
      (setq zenicb-history-list-backward nil
            zenicb-history-list-forward nil)
    ; remove contents of line
    (beginning-of-line)
    (if (not (= (point) (point-max)))
        (delete-backward-char (- (point) (point-max))))
    ; insert next command
    (insert (car zenicb-history-list-forward))
    ; update hairy variables
    (setq zenicb-history-list-backward (cons
                                        zenicb-history-list-current
                                        zenicb-history-list-backward)
          zenicb-history-list-current (car zenicb-history-list-forward)
          zenicb-history-list-forward (cdr zenicb-history-list-forward))))

(provide 'zenicb-history)

(zenicb-add-hook 'zenicb-send-line-hook 'zenicb-history-command)

(define-key zenicb-mode-map "\M-p" 'zenicb-history-backward)
(define-key zenicb-mode-map "\M-n" 'zenicb-history-forward)

;;; End of zenicb-history.el
