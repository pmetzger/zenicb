;;; zenicb-8ball.el --- magic 8 ball for ZenICB

;; Copyright (C) 1998 Faried Nawaz
;; Copyright (C) 1996 Mark S Bailen

;; Author:  Faried Nawaz
;; Maintainer: Faried Nawaz <fn@Hungry.COM>
;; Keywords: zenicb, extensions, magic, 8ball
;; Created: sometime in 1997.

;; $Id: zenicb-8ball.el,v 1.3 1998/12/30 23:57:16 fn Exp $

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

;; Code hacked from Noah's zenirc-yow.el and Eric's zenirc-random-away.el
;; Rewritten 1997-03-03 by Noah to use zenirc-trigger.el.

;; Original (zenirc-8ball.el) carries copyright/author info:

;; Copyright (C) 1996 Mark S Bailen

;; Bozo:  Mark S Bailen
;; Maintainer: Noah Friedman <friedman@prep.ai.mit.edu>
;; Keywords: zenirc, extensions, magic, 8ball
;; Created: 1996-10-4


;;; Code:

(require 'zenicb)
(require 'zenicb-trigger)

(defconst zenicb-8ball-prefix "magic 8-ball says ====>")

(defconst zenicb-8ball-responses
  '("CERTAINLY"
    "DEFINITELY NOT"
    "MAYBE"
    "NO"
    "REPLY HAZY, TRY AGAIN LATER"
    "YES"))

(defun zenicb-8ball ()
  (concat zenicb-8ball-prefix " "
          (nth (zenicb-random (length zenicb-8ball-responses))
               zenicb-8ball-responses)))

(zenicb-trigger-register "8ball" 'zenicb-8ball "\\b8 ball\\b")

(provide 'zenicb-8ball)

;;; zenicb-8ball.el ends here
