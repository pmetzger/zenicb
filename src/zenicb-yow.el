;;; zenirc-yow.el --- important pinheadery for ZenIRC

;; Copyright (C) 1998 Faried Nawaz

;; Author: Faried Nawaz <fn@Hungry.COM>
;; Maintainer: <fn-icb@LISP-READER.Hungry.COM>
;; Keywords: zenicb, extensions, oink, yow
;; Created: 1998-03-27

;; $Id: zenicb-yow.el,v 1.1 1998/03/28 00:51:54 fn Exp $

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

;; ANN JILLIAN'S HAIR makes LONI ANDERSON'S HAIR look like
;; RICARDO MONTALBAN'S HAIR!


;; original ZenIRC code carries the following copyright info

;; Copyright (C) 1994, 1995 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: zenirc, extensions, oink, yow
;; Created: 1994-06-30

;;; Code:

(require 'zenicb-trigger)

;; v18 yow.el didn't have a `provide'
(or (fboundp 'yow) (load "yow"))

;; Strip newlines and excess whitespace from string.
(defun zenicb-yow-format-string (s)
  (save-match-data
    (cond ((string-match "[ \t\n\r][ \t\n\r]+" s)
           (let ((orig-buffer (current-buffer))
                 (temp-buffer (generate-new-buffer " *Yow*")))
             (unwind-protect
                 (progn
                   (set-buffer temp-buffer)
                   ;; don't make undo records in temp buffer
                   (let ((buffer-undo-list t))
                     (insert s)
                     (goto-char (point-min))
                     (while (re-search-forward "[ \t\n\r]+" nil t)
                       (replace-match " "))
                     (setq s (buffer-substring (point-min) (point-max)))))
               (set-buffer orig-buffer)
               (kill-buffer temp-buffer))))))
  s)

(defun zenicb-yow ()
  (zenicb-yow-format-string (yow)))

(zenicb-trigger-register "yow" 'zenicb-yow "\\byow\\b")

(provide 'zenicb-yow)

;;; zenicb-yow.el ends here
