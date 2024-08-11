;;; zenicb-meditate.el --- admonish others for disturbing your meditation

;; Copyright (C) 1997, 1998 Faried Nawaz

;; Author: Faried Nawaz <fn@Hungry.COM>
;; Maintainer: <fn-icb@LISP-READER.Hungry.COM>
;; Keywords: zenirc, extensions
;; Created: sometime in 1997

;; $Id: zenicb-meditate.el,v 1.2 1998/12/30 23:57:16 fn Exp $

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

;;; This code is relatively untested.

;;; The original ZenIRC code carries the copyright/author info

;; Copyright (C) 1995, 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: pp@gnu.ai.mit.edu
;; Keywords: zenirc, extensions
;; Created: 1995-04-09

;;; Code:

(require 'zenicb-trigger)

(defconst zenicb-meditate-response-percentage 1)

(defconst zenicb-meditate-response-list
  '("Activity through inactivity."
    "Don't bother."
    "Enlightenment does not come from typing."
    "Enlightenment does not require a keyboard."
    "Hair will grow on your palms if you keep typing."
    "Meditate, or die."
    "Once a student typed too much and died."
    "Stop fidgeting, you're bothering the others."
    "The keyboard is sure to block your mind."
    "Will you stop the infernal racquet and meditate!?"
    "Your fingers will destroy your meditation."
    "Your keyboard is not the path to enlightenment."
    "Your typing detracts from your enlightenment."))

(defun zenicb-meditate ()
  (and (< (zenicb-random 1000) zenicb-meditate-response-percentage)
       (nth (zenicb-random (length zenicb-meditate-response-list))
            zenicb-meditate-response-list)))

(zenicb-trigger-register "meditate" 'zenicb-meditate "^.")

(provide 'zenicb-meditate)

;; zenicb-meditate.el ends here
