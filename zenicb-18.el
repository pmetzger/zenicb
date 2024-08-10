;;; zenicb-18.el --- compatibility functions for Emacs 18

;;; Copyright (C) 1998, 1999 Faried Nawaz
;;; Copyright (C) 1994, 1995 Noah S. Friedman
;;; Copyright (C) 1985, 1986, 1992, 1994 Free Software Foundation, Inc.

;; Author: Faried Nawaz <fn@hungry.org>
;; Maintainer: fn@hungry.org
;; Keywords: extensions, zenicb
;; Created: 1998-12-29

;; $Id: zenicb-18.el,v 1.3 1999/01/04 09:55:57 fn Exp $

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

;; This is based almost entirely on zenirc-18.el from the ZenIRC package.
;; It  which carries the copyright info:

;; Copyright (C) 1994, 1995 Noah S. Friedman
;; Copyright (C) 1985, 1986, 1992, 1994 Free Software Foundation, Inc.

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions, zenirc
;; Created: 1995-01-01

;; This file mainly just defines functions used by ZeniCB which don't exist
;; in emacs 18, but do exist in emacs 19.  Everything here was written by
;; Noah Friedman unless indicated otherwise.

;;; Code:


;; From GNU Emacs 19.27 subr.el
(defmacro zenicb-save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list
     'let (list (list original '(match-data)))
     (list 'unwind-protect
           (cons 'progn body)
           (list 'store-match-data original)))))

(or (fboundp 'save-match-data)
    (fset 'save-match-data 'zenicb-save-match-data))


(defun zenicb-delete (elt list)
  "Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `equal'.
If the first member of LIST is ELT, deleting it is not a side effect;
it is simply using a different list.
Therefore, write `(setq foo (delete element foo))'
to be sure of changing the value of `foo'."
  (let ((p list)
        (l (cdr list)))
    (while l
      (if (equal elt (car l))
          (setcdr p (cdr l))
        (setq p (cdr p)))
      (setq l (cdr l))))
  (if (equal elt (car list))
      (cdr list)
    list))

(or (fboundp 'delete)
    (fset 'delete 'zenicb-delete))


(defun zenicb-force-mode-line-update (&optional all)
  "Force the mode-line of the current buffer to be redisplayed.
With optional non-nil ALL then force then force redisplay of all mode-lines."
  (if all (save-excursion (set-buffer (other-buffer))))
  (set-buffer-modified-p (buffer-modified-p)))

(or (fboundp 'force-mode-line-update)
    (fset 'force-mode-line-update 'zenicb-force-mode-line-update))


(defun zenicb-member (x y)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT."
  (while (and y (not (equal x (car y))))
      (setq y (cdr y)))
    y)

(or (fboundp 'member)
    (fset 'member 'zenicb-member))


(defun zenicb-char-to-int (item)
  item)
(or (fboundp 'char-to-int)
    (fset 'char-to-int 'zenicb-char-to-int))


(or (fboundp 'read-passwd)
    (fset 'read-passwd 'read-from-minibuffer))


(provide 'zenicb-18)

;;; zenicb-18.el ends here.
