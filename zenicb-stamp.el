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


(defun zenicb-epoch-seconds-to-time (seconds)
  (save-match-data
    (let (millions units high low)
      (if (string-match "^\\(.*\\)\\(......\\)$" seconds)
          (setq millions (string-to-int (substring seconds
                                                   (match-beginning 1)
                                                   (match-end 1)))
                units (string-to-int (substring seconds
                                                (match-beginning 2)
                                                (match-end 2))))
        (setq millions 0
              units (string-to-int seconds)))
      (setq high (+ (* millions 15) (/ (* millions 265) 1024) (/ units 65536))
            low (+ (% (+ (* (% millions 4) 16384) (* millions 576)) 65536)
                   (% units 65536)))
      (if (> low 65535)
          (setq low (- low 65536)
                high (1+ high)))
      (list high low))))


(defvar zenicb-timestamp-interval '(0 600)
  "How often to insert timestamps into the ZenICB buffer. The default
is 600 seconds or 10 minutes. The value of this variable is a 32 bit
integer, expressed as a list of two 16 bit values, ie, the default
value of 600 seconds is expressed as (0 600).")

(defvar zenicb-last-timestamp '(0 0)
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
(setq zenicb-time-last-event (zenicb-time-to-int (current-time-string)))

(provide 'zenicb-stamp)
