;;; zenicb-whereis.el --- store whereis info about users.

;; Copyright (C) 1998 Faried Nawaz

;; Author: Faried Nawaz <fn@Hungry.COM>
;; Maintainer: Faried Nawaz <fn-icb@LISP-READER.Hungry.COM>
;; Keywords: zenicb, extensions
;; Created: 1998/03/15

;; $Id: zenicb-whereis.el,v 1.2 1999/01/05 03:34:12 fn Exp $

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

;; This code does an automatic /whereis on anyone who sends you a message
;; or beeps you.  It displays the whereis output in your ZenICB buffer (once)
;; and in your echo area.  Nick changes are detected and handled correctly.

;; The code is based on ZenIRC's zenirc-format.el.  zenirc-format.el
;; carries the following copyright/author info:

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Copyright (C) 1995 Noah S. Friedman
;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;         Charles Hannum <mycroft@gnu.ai.mit.edu>
;;         Richard Todd <rmtodd@essex.ecn.uoknor.edu>
;;         Noah Friedman <friedman@prep.ai.mit.edu>


;;; Code:

(require 'zenicb)

(defun zenicb-server-whereis (proc parsedmsg)
  (zenicb-send-string proc ?h (concat "whereis\C-a" (nth 0 parsedmsg))))

; Do a /whereis on messages.
(zenicb-add-hook 'zenicb-server-c-hook 'zenicb-server-whereis)
; Do a /whereis on beeps.
(zenicb-add-hook 'zenicb-server-k-hook 'zenicb-server-whereis)

(defvar zenicb-trap-table (make-vector 307 0))

(defun zenicb-trap-whereis-info (proc parsedmsg)
  (let ((str (car (cdr parsedmsg)))
	nick user host nickuserhost)
    (save-match-data
      (string-match "\\([^ ]+\\)\\ +\\([^@]+\\)@\\(.*\\)" str)
      (setq nick (match-string 1 str)
	    user (match-string 2 str)
	    host (match-string 3 str)))
    (if (or (null nick) (null user) (null host))
	(zenicb-display-string proc (format "[info] %s\n" str))
      (progn
	(setq nick (downcase nick))
	(let* ((nicksym (intern nick zenicb-trap-table))
	       (cached (and (boundp nicksym)
			    (symbol-value nicksym)))
	       (nickuserhost (format "%s!%s@%s" nick user host)))
	  (set nicksym nickuserhost)
	  (if (or (not cached)
		  (not (string= cached nickuserhost)))
	      (zenicb-display-string proc (format "[info] %s is %s@%s\n"
						 nick user host))))
	(message "%s is %s@%s" nick user host)))))

(defun zenicb-server-i (proc parsedmsg)
  (let ((reply-type (nth 0 parsedmsg)))
    (cond 
     ((string= reply-type "wh") ; who reply header
      (zenicb-display-string
       proc (format "[info]  Nickname     Idle  Sign-On       Account\n")))
     ((string= reply-type "wl") ; who reply
      (zenicb-display-string 
       proc (format "[info] %s%-12s%5s  %s  %s@%s %s\n"
		    (if (string= (nth 1 parsedmsg) "m") "*" " ")
		    (nth 2 parsedmsg)
		    (zenicb-convert-time (nth 3 parsedmsg))
		    (zenicb-convert-date (nth 5 parsedmsg))
		    (nth 6 parsedmsg)
		    (nth 7 parsedmsg)
		    (nth 8 parsedmsg))))
     ((string= reply-type "co") ; comment
      (zenicb-trap-whereis-info proc parsedmsg))
     (t
      (zenicb-display-string
       proc (format "[debug] packet type i, subtype %s, data %s\n"
		    (prin1-to-string reply-type)
		    (prin1-to-string parsedmsg)))))))


(provide 'zenicb-whereis)

;; end
