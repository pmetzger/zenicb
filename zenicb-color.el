;;; zenicb-color.el --- color messages in zenicb

;; Copyright (C) 1996 John Wiegley
;; Copyright (C) 1996, 1998 Per Persson
;; Copyright (C) 1999 Faried Nawaz

;; Author: John Wiegley <johnw@borland.com>
;;         Per Persson <pp@sno.pp.se>
;;         Faried Nawaz <fn@Hungry.COM>
;; Maintainer: <fn-icb@LISP-READER.Hungry.COM>
;; Keywords: zenicb,extensions
;; Created: 1999-01-03

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

;;; This ZenICB extensions allows you to colorize input from specific
;;; sources.  Use the "/color victim <COLOR>" command to start
;;; colorizing a certain victim's output.

;;; This is based almost entirely on zenirc-color.el, from the ZenIRC
;;; package.

;;; Code:

(require 'zenicb)

(defun zenicb-color-install-message-catalogs ()
  (zenicb-lang-define-catalog 'english
   '((color-lame-args . "[info] %s: not enough arguments.")
     (color-nonexistant . "[info] %s is not an allowed color.")
     (color-not-found . "[info] %s: not found in color list."))))

(defvar zenicb-color-mode nil
  "*If non-nil, then color messages fitting `zenicb-color-message-categories'.
This is buffer-local.")
(make-variable-buffer-local 'zenicb-color-mode)

(defvar zenicb-color-region-function 'zenicb-colorize-region
  "*Function to use for coloring.")

(defvar zenicb-facename-index 1
  "Used for creating new zenicb face names")

(defvar zenicb-color-alist
  '()
  "*A list of elements, each of which is (REGEXP FACE), where both are
strings.  Any string containing REGEXP in the output will have it's face
property set to FACE.")
(make-variable-buffer-local 'zenicb-color-alist)

(defvar zenicb-color-message-categories
  '(public private beep)
  "*ZenICB message categories to color.  This should be a list
consisting of symbols corresponding to the type of messages in the
message catalog which should be colored.  For example, private
messages (`privmsg') and notices (`notice') are good choices.

If this variable is set to `t', then all messages are colored.
If this variable is set to `nil', then no messages are colored.")
(make-variable-buffer-local 'zenicb-color-message-categories)

(defvar zenicb-color-nonstandard-message-categories-p nil
  "If non-nil, then color messages that are not in a standard category.
That is, color messages which did not originate from the message catalog,
and thus have no category symbol.")
(make-variable-buffer-local 'zenicb-color-nonstandard-message-categories-p)

;; Check whether a given color really exists as a color.
(defun zenicb-color-name-p (color)
  (x-color-defined-p color))


(defun zenicb-color-mode (&optional prefix)
  "Enable or disable colorization of icb messages.

A negative prefix argument disables this mode.
No argument or any non-negative argument enables it.

The user may also enable or disable this mode simply by setting the
variable of the same name."
  (interactive "P")
  (cond
   ((null prefix)
    (setq zenicb-color-mode (not zenicb-color-mode)))
   ((>= (prefix-numeric-value prefix) 0)
    (setq zenicb-color-mode t))
   (t
    (setq zenicb-color-mode nil)))
  (cond ((not (called-interactively-p t)))
        (zenicb-color-mode
         (message "zenicb-color-mode is enabled"))
        (t
         (message "zenicb-color-mode is disabled")))
  zenicb-color-mode)

(defvar zenicb-face nil)

(defun zenicb-colorize-region (beg end)
  (interactive "r")
  (save-match-data
    (save-excursion
      (goto-char beg)
      (mapcar
       (function
        (lambda (elem)
          (if (re-search-forward (car elem) end t)
              (put-text-property beg end 'face (car (cdr elem))))))
       zenicb-color-alist))))

(defun zenicb-color-message (proc sym string)
  (and zenicb-color-mode
       (cond ((eq zenicb-color-message-categories t))
             ((null sym)
              zenicb-color-nonstandard-message-categories-p)
             ((memq sym zenicb-color-message-categories))
             (t nil))
       (funcall zenicb-color-region-function (point-min) (- (point-max) 1))))


(defvar zenicb-command-color-hook '(zenicb-command-color))

;; /color #victim <color>
(defun zenicb-command-color (proc cmd)
  (let* ((arg (zenicb-parse-firstword (cdr parsedcmd)))
         (victim (format "%s" (car arg)))
         (color (cdr arg)))
    (if (or (string= "" victim)
            (string= "" color))
        (zenicb-message proc 'color-lame-args "/color")
      (if (zenicb-color-name-p color)
	  (let ((newface (make-symbol
			  (concat "zenicb-color-face-"
				  (number-to-string zenicb-facename-index)))))
	    (setq zenicb-facename-index (1+ zenicb-facename-index))
	    (copy-face 'default newface)
	    (set-face-foreground newface color)
	    (setq zenicb-color-alist
		  (cons (list victim newface)
			zenicb-color-alist)))
	(zenicb-message proc 'color-nonexistant color)))))

(defvar zenicb-command-uncolor-hook '(zenicb-command-uncolor))

;; /uncolor #victim
(defun zenicb-command-uncolor (proc cmd)
  (let* ((arg (zenicb-parse-firstword (cdr parsedcmd)))
         (victim (format "%s" (car arg))))
    (if (string= "" victim)
        (zenicb-message proc 'color-lame-args "/uncolor"))
    (let ((pointer zenicb-color-alist) last found)
      (while pointer
        (if (string= (car (car pointer)) victim)
            (progn
              (setq found t)
              (if (= (length zenicb-color-alist) 1)
                  (setq zenicb-color-alist nil)
                (if last
                    (setcdr last (cdr pointer))
                  (setq zenicb-color-alist (cdr pointer))))))
        (setq last pointer)
        (setq pointer (cdr pointer)))
      (if (not found)
          (zenicb-message proc 'color-not-found victim)))))

(provide 'zenicb-color)

(zenicb-add-hook 'zenicb-message-hook 'zenicb-color-message)

(or (assq 'zenicb-color-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons (list 'zenicb-color-mode " Zcolor") minor-mode-alist)))

(zenicb-color-install-message-catalogs)

;;; zenicb-color.el ends here
