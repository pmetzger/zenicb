;;; zenicb-stamp.el --- timestamping for ZenICB

;; Copyright (C) 1998, 1999 Faried Nawaz

;; Author: Faried Nawaz <fn@Hungry.COM>
;; Maintainer: Faried Nawaz <fn-icb@LISP-READER.Hungry.COM>
;; Keywords: extensions
;; Created: 1998/03/27

;; $Id: zenicb-stamp.el,v 1.3 1999/01/04 09:55:58 fn Exp $

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

;; This code is meant as a demonstration of how to use the ZenICB
;; hook mechanism and timer code to cause ZenICB to do something at a
;; regular interval.  Since it was adapted from ZenIRC code, it may
;; be non-optimal.

;; The copyright/author info on the ZenIRC code is

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>

;;; Code:

(require 'zenicb)

(defvar zenicb-timestamp-interval 600
  "How often to insert timestamps into the ZenICB buffer, in seconds.
The default is 600 seconds, or 10 minutes.")

(defvar zenicb-last-timestamp 0
  "The time the last timestamp was inserted into the ZenICB buffer.
You shouldn't have to frob this yourself.")

(defun zenicb-timestamp (proc now)
  "Insert a timestamp into the the ZenICB buffer specified by the
process PROC every zenicb-timestamp-interval seconds."
  (if (zenicb-time< zenicb-timestamp-interval
                    (zenicb-time-diff now zenicb-last-timestamp))
      (progn
        (zenicb-message proc (concat "[time] " (current-time-string)))
        (setq zenicb-last-timestamp now))))

(zenicb-add-hook 'zenicb-timer-hook 'zenicb-timestamp)

(provide 'zenicb-stamp)
