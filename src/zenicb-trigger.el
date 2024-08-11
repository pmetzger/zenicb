;;; zenicb-trigger.el

;; Copyright (C) 1997 Noah S. Friedman
;; Copyright (C) 1997, 1999 Faried Nawaz

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: <fn-icb@LISP-READER.Hungry.COM>
;; Keywords: zenicb, extensions, oink
;; Created: 1997-03-01

;; $Id: zenicb-trigger.el,v 1.8 1999/01/04 09:55:59 fn Exp $

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

;; Use this package to define "triggers" regexps.
;; For example, you can define a function /time and specify a regexp that
;; sends the current time whenever someone asks "what time is it?":
;;
;; (zenicb-trigger-register "time" 'current-time-string "what time is it\\?")
;;
;; If you want parsedmsg passed to the function, append a 't', like
;;
;; (zenicb-trigger-register "kick" 'kick-luser "LINUX RULES!\\?" t)
;;
;; You can also use the /trigger command in the *zenicb* buffer:
;;
;; /trigger set time "what time is it\\?" current-time-string
;;
;; People have defined more interesting triggers such as horoscopes, zippy
;; quotes, etc.

;; This includes an extension by Ray Jones <rjones@pobox.com> to pass
;; parsedmsg to the trigger function.

;; This package contains a lot of fluff inherited from zenirc-trigger.el.
;; Needs cleanup.

;;; Code:

(require 'zenicb)
(require 'backquote)

(defvar zenicb-trigger-table nil)
(defvar zenicb-command-trigger-hook 'zenicb-command-trigger)

(defun zenicb-trigger-register (name response &optional regexp pass-string)
  "Define a trigger named NAME, that runs RESPONSE when REGEXP is seen.
When a string matching REGEXP is detected in a zenicb buffer, the function
RESPONSE is called with no arguments.  That function should return a string
which is to be sent to the originator of the message matching REGEXP.
It may instead return a list of strings, in which case each string is
sent as a separate message.  if PASS-STRING is t, the parsedmsg will be
sent to the trigger function.

NAME can be used as a key for changing, deleting, activating, and
deactivating the trigger via /trigger subcommands."
  (zenicb-trigger-make-command name response)
  (and regexp
       (zenicb-trigger-set-trigger name regexp response pass-string)))

;;; Users shouldn't generally need to make use of anything below here.

(defun zenicb-trigger-make-command (name response)
  (let* ((symname (format "zenicb-command-%s" name))
         (cmdsym (intern symname))
         (cmdhook (intern (concat symname "-hook"))))
    (zenicb-add-hook cmdhook cmdsym)
    (fset cmdsym
          (` (lambda (proc victim)
               (zenicb-trigger-send-fn-result proc victim '(, response)))))))

(defun zenicb-trigger-set-trigger (name regexp response &optional pass-string)
  (and (stringp name)
       (setq name (intern name)))
  (let ((elt (assq name zenicb-trigger-table)))
    (cond (elt
           (setcar (nthcdr 1 elt) t)
           (setcar (nthcdr 2 elt) regexp)
           (setcar (nthcdr 3 elt) response)
           (setcar (nthcdr 4 elt) pass-string))
          (t
           (setq zenicb-trigger-table
                 (cons (list name t regexp response pass-string)
                       zenicb-trigger-table))))))

;; Returns nil if the named trigger doesn't exist, t otherwise.
(defun zenicb-trigger-activate (name state)
  (and (stringp name) (setq name (intern name)))
  (let ((elt (assq name zenicb-trigger-table)))
    (cond (elt
           (setcar (nthcdr 1 elt) state)
           t)
          (t nil))))

(defun zenicb-trigger-enable (name)
  (zenicb-trigger-activate name t))

(defun zenicb-trigger-disable (name)
  (zenicb-trigger-activate name nil))


;; note: car = user, cadr = message
(defun zenicb-server-public-trigger (proc parsedmsg)
  (if (not (string-match zenicb-nick (nth 0 parsedmsg)))
      (save-match-data
        (let ((case-fold-search t)
              (trigger-table zenicb-trigger-table)
              regexp fn)
          (while trigger-table
            (cond ((nth 1 (car trigger-table))
                   (setq regexp (nth 2 (car trigger-table)))
                   (and regexp
                        (string-match regexp (nth 1 parsedmsg))
                        (let ((victim (nth 0 parsedmsg))
                              (msg (zenicb-trigger-response
                                    (nth 3 (car trigger-table))
                                    (and (nth 4 (car trigger-table))
                                         (match-string 0 (nth 1 parsedmsg))))))
                          (zenicb-trigger-send-response proc victim msg)))))
            (setq trigger-table (cdr trigger-table)))))))


;; (defun zenicb-server-PRIVMSG-trigger (proc parsedmsg)
;;   (save-match-data
;;     (let ((case-fold-search t)
;;           (trigger-table zenicb-trigger-table)
;;           regexp fn)
;;       (while trigger-table
;;         (cond ((nth 1 (car trigger-table))
;;                (setq regexp (nth 2 (car trigger-table)))
;;                (and regexp
;;                     (string-match regexp (aref parsedmsg 3))
;;                     (let ((victim (zenicb-trigger-parse-sender parsedmsg))
;;                           (msg (zenicb-trigger-response
;;                                 (nth 3 (car trigger-table)))))
;;                       (zenicb-trigger-send-response proc victim msg)))))
;;         (setq trigger-table (cdr trigger-table))))))

;; (defun zenicb-trigger-parse-sender (parsedmsg)
;;   (let ((from (aref parsedmsg 2)))
;;     (cond ((zenicb-names-equal-p from zenicb-nick)
;;            (zenicb-extract-nick (aref parsedmsg 1)))
;;           (t from))))

(defun zenicb-trigger-response (fn msg)
  (cond ((stringp fn) fn)
        (t
         (if msg
             (funcall fn msg)
           (funcall fn)))))

(defun zenicb-trigger-send-fn-result (proc victim fn &optional msg)
  (setq victim (cdr victim))
  (cond ((or (null victim)
             (string= "" victim))
         (setq victim zenicb-current-victim)))
  (zenicb-trigger-send-response proc victim (zenicb-trigger-response fn msg)))

(defun zenicb-trigger-send-response (proc victim msg)
  (cond ((stringp msg)
         (zenicb-message proc 'trigger-sent victim msg)
         (zenicb-send-string proc ?b msg))
        (t
         (while msg
           (zenicb-message 'trigger-sent victim (car msg))
           ;;(zenicb-send-public proc ?b (car msg))
           (zenicb-send-public proc (car msg))
           (setq msg (cdr msg))))))


;; Parser for /trigger command.  This figures out the trigger subcommand
;; and calls the appropriate routine to handle it.
;; The function dispatched should be named "zenicb-trigger-do-FOO-command",
;; where FOO is one of `list', `set', `enable', `disable', etc.
;; With no arguments, lists available subcommands.
(defun zenicb-command-trigger (proc parsedcmd)
  (let* ((cmd (zenicb-parse-firstword (cdr parsedcmd)))
         (fn (intern-soft (concat "zenicb-trigger-do-" (car cmd) "-command"))))
    (cond ((and fn (fboundp fn))
           (funcall fn proc cmd))
          ((null (car cmd))
           (zenicb-message proc 'trigger-subcommands
                           (mapconcat 'identity
                                      (zenicb-trigger-subcommand-list)
                                      ", ")))
          (t
           (zenicb-message proc 'trigger-command-undefined (car cmd))))))

;; Returns a list of defined subcommands to /trigger.
(defun zenicb-trigger-subcommand-list ()
  (save-match-data
    (let* ((prefix "zenicb-trigger-do-")
           (suffix "-command")
           (re (concat suffix "$")))
      (sort (mapcar (function (lambda (s)
                                (substring s (length prefix)
                                           (- (length suffix)))))
                    (all-completions prefix obarray
                                     (function
                                      (lambda (s)
                                        (string-match re (symbol-name s))))))
            'string-lessp))))

(defun zenicb-trigger-do-list-command (proc args)
  (let ((table zenicb-trigger-table))
    (zenicb-message proc 'trigger-list-head)
    (zenicb-message proc 'trigger-list-line)
    (while table
      (zenicb-message proc 'trigger-list-item
                      (nth 0 (car table))
                      (nth 1 (car table))
                      (prin1-to-string (nth 2 (car table)))
                      (prin1-to-string (nth 3 (car table))))
      (setq table (cdr table)))
    (zenicb-message proc 'trigger-list-end)))

(defun zenicb-trigger-do-set-command (proc args)
  (let* ((parsed1 (zenicb-parse-firstword (cdr args)))
         (name (car parsed1))
         (parsed2 (read-from-string (cdr parsed1)))
         (regexp (car parsed2))
         (fn (car (read-from-string (substring (cdr parsed1)
                                               (cdr parsed2))))))
    (zenicb-trigger-register name fn regexp)
    (zenicb-message proc 'trigger-enable name)))

(defun zenicb-trigger-do-delete-command (proc args)
  (let ((names (zenicb-parse-words (cdr args)))
        (known nil)
        (unknown nil)
        elt)
    (while names
      ;; If intern-soft returns nil, assq will return nil.
      (setq elt (assq (intern-soft (car names)) zenicb-trigger-table))
      (if (null elt)
          (setq unknown (cons (car names) unknown))
        (setq zenicb-trigger-table (delq elt zenicb-trigger-table))
        (setq known (cons (car names) known)))
      (setq names (cdr names)))
    (and known
         (zenicb-message proc 'trigger-deleted (nreverse known)))
    (and unknown
         (zenicb-message proc 'trigger-undefined (nreverse unknown)))))

(defun zenicb-trigger-do-enable-command (proc args)
  (zenicb-trigger-do-activation proc (cdr args) t))

(defun zenicb-trigger-do-disable-command (proc args)
  (zenicb-trigger-do-activation proc (cdr args) nil))

(defun zenicb-trigger-do-activation (proc args state)
  (let ((msg (if state "[info] Triggers enabled: %s\n" "[info] Triggers disabled: %s\n"))
        (names (zenicb-parse-words args))
        (known nil)
        (unknown nil))
    (while names
      (if (zenicb-trigger-activate (car names) state)
          (setq known (cons (car names) known))
        (setq unknown (cons (car names) unknown)))
      (setq names (cdr names)))
    (and known
         (zenicb-message proc msg (nreverse known)))
    (and unknown
         (zenicb-message proc 'trigger-undefined (nreverse unknown)))))


(provide 'zenicb-trigger)

(zenicb-lang-define-catalog
 'english
 '((trigger-sent      . "[trigger] Sent to %s: %s")
   (trigger-enable    . "[info] Triggers enabled: %s")
   (trigger-disable   . "[info] Triggers disabled: %s")
   (trigger-deleted   . "[info] Triggers deleted: %s")
   (trigger-undefined . "[info] Undefined triggers: %s")
   (trigger-list-head . "[trigger] Name       On? Regexp          Function")
   (trigger-list-line . "[trigger] ----       --- ------          --------")
   (trigger-list-item . "[trigger] %-10s %-3s %-15s %s")
   (trigger-list-end  . "[trigger] End of list.")
   (trigger-subcommands . "[info] Trigger subcommands: %s")
   (trigger-command-undefined . "[info] undefined trigger command: %s")))

(zenicb-add-hook 'zenicb-server-b-hook 'zenicb-server-public-trigger)

;; (defun zenicb-message (proc msg)
;;   (zenicb-display-string proc msg))

(defun zenicb-parse-words (line)
  (let ((list '())
        (posn 0))
    (save-match-data
      (while (string-match "[^ \t\n]+" line posn)
        (setq list (cons (substring line (match-beginning 0) (match-end 0))
                         list))
        (setq posn (match-end 0))))
    (nreverse list)))


;; zenicb-random from youwill.el by Noah Friedman.
(defun zenicb-random (n)
  (and (numberp n)
       (random n)))

;;; zenicb-trigger.el ends here
