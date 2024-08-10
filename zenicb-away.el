;;; zenicb-away.el --- provide /away command for ZenICB

;; Copyright (C) 1997, 1999 Faried Nawaz

;; Author: Faried Nawaz <fn@Hungry.COM>
;; Maintainer: <fn-icb@LISP-READER.Hungry.COM>
;; Keywords: extension, zenicb
;; Created: 11-23-1997

;; $Id: zenicb-away.el,v 1.6 1999/01/06 20:35:48 fn Exp $

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

(defvar zenicb-away-mode nil
  "*If non-nil, away mode is engaged.")
(make-variable-buffer-local 'zenicb-away-mode)

(defvar zenicb-away-message nil
  "*Away message.")
(make-variable-buffer-local 'zenicb-away-message)

(defvar zenicb-command-away-hook '(zenicb-command-away))

(defun zenicb-command-away (proc parsedcmd)
  (cond ((string= "" (cdr parsedcmd))
	 (zenicb-remove-hook 'zenicb-server-c-hook 'zenicb-away)
	 (zenicb-remove-hook 'zenicb-command-m-hook 'zenicb-away-remind)
	 (setq zenicb-away-mode nil))
	(t
	 (setq zenicb-away-message (cdr parsedcmd))
	 (setq zenicb-away-mode t)
	 (zenicb-add-hook 'zenicb-server-c-hook 'zenicb-away)
	 (zenicb-add-hook 'zenicb-command-m-hook 'zenicb-away-remind)))
  (force-mode-line-update))

(defun zenicb-away (proc parsedmsg)
  ; prevent a loop
  (if (not (or (string-match "^away: " (nth 1 parsedmsg))
	       (string= zenicb-nick (nth 0 parsedmsg))))
      (zenicb-send-string proc ?h
			  (concat "m\C-a" (car parsedmsg)
				  " " "away: " zenicb-away-message))))

(defun zenicb-away-remind (proc parsedmsg)
  ; remind users that they are /msg'ing someone while they're /away.
  (zenicb-message proc 'alreadyaway))

(or (assq 'zenicb-away-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons (list 'zenicb-away-mode " Away") minor-mode-alist)))


(provide 'zenicb-away)

(zenicb-lang-define-catalog 'english
  '((alreadyaway  . "[note] You are away.")))

;;; End of zenicb-away.el
