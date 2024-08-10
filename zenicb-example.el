;;; zenicb-example.el --- Example customizations for zenicb.el

;;; Copyright (C) 1997, 1998 Faried Nawaz

;;; Author: Faried Nawaz <fn@Hungry.COM>
;;; Maintainer: Faried Nawaz <fn-icb@LISP-READER.Hungry.COM>
;;; Keywords: extensions
;;; Created: 1997/11/23

;;; $Id: zenicb-example.el,v 1.6 1999/01/06 20:39:32 fn Exp $

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

;; A good way to use this is to add something like
;;
;; (autoload 'zenicb "zenicb-example" "Major mode to waste time" t)
;;
;; to your ~/.emacs file. Then edit and copy this file somewhere in
;; your load-path as "zenicb-example.el".  If you do that you also
;; need to uncomment the last line of this file.

;; Also take a look on all the different scripts not mentioned in this file,
;; they might give you something you'll love.

;;; This file is based on ZenIRC's zenirc-example.el.  The copyright/author
;;; info for that file is

;;; Copyright (C) 1993, 1994 Ben A. Mesander
;;; Copyright (C) 1993, 1994, 1996, 1997 Per Persson

;;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;;         Per Persson <pp@gnu.ai.mit.edu>
;;;         Faried Nawaz <fn@Hungry.COM>


;;; Code:

;; this is a list of ICB servers you use
;; it consists of servername, portnumber, nickname, password, initial-channel
;; only the servername is necessary; if any element is set to nil, zenicb's
;; defaults are used.
(setq zenicb-server-alist
      '(("evolve.icb.net")
        ("empire.icb.net" 7326)
        ("echo.icb.net" 7326 "Erin")
        ("bossanova.icb.net" 7326 "nil" "nil" "Meditation")))


;; zenicb can be told to listen for certain signals.
;; zenicb-signal-list takes a list of regular expressions.
;; the format of signals received is a bit wonky.  for example,
;; a beep from from fn shows up as "beep:fn".  a private message
;; from Image shows up as "private:ImageDon't you SEE?".  to match
;; for beeps from fn or any message from Image, do
(setq zenicb-signal-list '("^beep:fn" "^\\w+:Image"))

;; zenicb can beep when it notices something,
;; nil -> never beep
;; t -> beep when message not seen
;; 'always -> beep on all signals
(setq zenicb-beep-on-signal nil)

;; to make zenicb beep whenever you get a beep, do
(setq zenicb-signal-list '("^beep"))
(setq zenicb-beep-on-signal 'always)

;; if you want timestamps on private or public messages
;; with default prefix and suffix it looks like
;;	*ben[13:31]* lets have some fun
;;	<ben#twilight_zone[13:32]> SLUGS AND KNIGHTS! SLUGS AND KNIGTS!
(setq zenicb-timestamp nil
      zenicb-timestamp-prefix "["
      zenicb-timestamp-suffix "]")


;; to do stuff after zenicb.el has been loaded, use the zenicb-mode-hook,
;; like

;;(setq zenicb-mode-hook '(lambda ()
;;			   (load "zenicb-add") ;give me time-stamps on messages
;;			   (load "zenicb-whereis") ;do auto-whereis
;;			   (setq zenicb-mode-hook nil)))

;; zenicb can beep when someone sends you a /beep
;; nil -> don't beep
;; t -> beep
(setq zenicb-beep t)

;; commandkey in ZenICB
(setq zenicb-command-char ?/)

;;; use the following to enable the /away command
(load-library "zenicb-away")

;;; use the following to ensure you never idle out
;(load-library "zenicb-nologout")

;;; use the following to get history functions on M-p and M-n
(load-library "zenicb-history")

;;; use the following if you want ZenICB to act automatically when it
;;; sees a certain string. if this sounds interesting, read the
;;; comments in zenicb-trigger.el and take a look at zenicb-yow.el,
;;; zenicb-meditate.el, zenicb-8ball.el, and zenicb-fortran.el.
;(load-library "zenicb-trigger")

;;; insert time-stamps into the buffer at least every 10 minutes.
(load-library "zenicb-stamp")

;; this is the second last line of the file, the next line is the last one
(load-library "zenicb")
