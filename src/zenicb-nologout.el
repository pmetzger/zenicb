;;;
;;; zenicb-nologout.el --- Continuously Waste time on International
;;;                        Citizen's Band (ZenICB client)

;;; Copyright (C) 1994 Ben A. Mesander
;;; Copyright (C) 1997 Faried Nawaz

;;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;; Maintainer: <fn-icb@LISP-READER.Hungry.COM>
;;; Keywords: extensions, zenicb
;;; Created: 1994/10/08

;;; $Id: zenicb-nologout.el,v 1.3 1999/01/05 03:32:51 fn Exp $

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

;;; This code is derived from the code to the ZenIRC internet relay chat
;;; client, and the `icb.el' ICB client. The people who helped me write
;;; ZenIRC are:
;;;         Noah Friedman <friedman@prep.ai.mit.edu>
;;;         Charles Hannum <mycroft@gnu.ai.mit.edu>
;;;         Richard Todd <rmtodd@essex.ecn.uoknor.edu>
;;;         Per Persson <pp@solace.mh.se>
;;;         Eric Prestemon <eric@american.edu>
;;;         Mark Bailen <msbailen@msbdcolka.cr.usgs.gov>
;;;         Nicolas Pioch <Nicolas.Pioch@enst.fr>
;;;
;;; The developers of the icb.el client are:
;;;         Matt Rhoten <mrhoten@cs.stanford.edu>
;;;         Greg Williams <greg.williams@gtri.gatech.edu>
;;;
;;; Bits of the ZenIRC client were also derived from the Kiwi and msa
;;; emacs clients; these have their own history and many contributors.
;;; Thanks to all.

;;; Code:

(require 'zenicb)

(zenicb-add-hook 'zenicb-server-d-hook 'zenicb-nologout)

(defvar zenicb-nologout-string "%Z%e%n%I%C%B% %n%o%l%o%g%o%u%t% %s%p%a%m%"
  "*Garbage sent to self to avoid auto-logout.")

(defun zenicb-nologout (proc parsedmsg)
  (if (string= (nth 0 parsedmsg) "Drop")
      (progn
        (zenicb-add-hook 'zenicb-server-c-hook 'zenicb-nologout-hide-spam)
        (setq zenicb-run-next-hook nil)
        (zenicb-send-string
         proc ?h (concat "m\C-a" zenicb-nick " " zenicb-nologout-string)))))

(defun zenicb-nologout-hide-spam (proc parsedmsg)
  (if (and (string= (nth 0 parsedmsg) zenicb-nick)
           (string= (nth 1 parsedmsg) zenicb-nologout-string))
      (progn
        (setq zenicb-run-next-hook nil)
        (zenicb-delete-hook 'zenicb-server-c-hook 'zenicb-nologout-hide-spam))))


(provide 'zenicb-nologout)

;;; End of zenicb-nologout.el
