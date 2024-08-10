;;; zenicb-add.el --- Add/redefine functions after loading zenicb.el

;;; Copyright (C) 1998 Faried Nawaz

;;; Author: Faried Nawaz <fn@Hungry.COM>
;;; Maintainer: <fn-icb@LISP-READER.Hungry.COM>
;;; Keywords: extensions
;;; Created: 1998/03/27

;;; $Id: zenicb-add.el,v 1.1 1998/03/28 00:56:14 fn Exp $

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's maintainer or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;;; This code is used to redefine functions in zenicb.el.  This is useful
;;; in situations where zenicb.el is stored in a system-public directory
;;; and cannot be modified by the user.
;;;
;;; This file should be loaded via zenicb-mode-hook, as in

;;; (setq zenicb-mode-hook '(lambda ()
;;; 				  (load "zenicb-add")
;;; 				  (setq zenicb-mode-hook nil)))
;;; 

;;; Currently, I use it to display timestamps in public/private messages
;;; that I receive.  I must write a generic timestamp interface one of
;;; these days...

;;; Code:

(defun zenicb-server-b (proc parsedmsg)
  (if (eq zenicb-alert 'all)
      (ding t))
  (zenicb-display-string proc (format "%s <%s> %s\n"
				      (substring (current-time-string) 11 16)
				      (nth 0 parsedmsg) 
				      (nth 1 parsedmsg))))

(defun zenicb-server-c (proc parsedmsg)
  (setq zenicb-msg-last-rec (nth 0 parsedmsg))
  (if (or (eq zenicb-alert 'personal)
	  (eq zenicb-alert 'all))
      (ding t))
  (zenicb-display-string proc (format "%s *%s* %s\n"
				      (substring (current-time-string) 11 16)
				      (nth 0 parsedmsg) 
				      (nth 1 parsedmsg))))

;;; end.