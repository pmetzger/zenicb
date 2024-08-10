;;;
;;;
;;; zenicb.el --- Waste time on International Citizen's Band (ZenICB client)

;;; Copyright (C) 1994 Ben A. Mesander
;;; Copyright (C) 1997, 1998, 1999 Faried Nawaz

;;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;;         Faried Nawaz <fn@Hungry.COM>
;;; Maintainer: <fn-icb@LISP-READER.Hungry.COM>
;;; Keywords: extensions
;;; Created: 1994/10/08

;;; $Id: zenicb.el,v 1.31 1999/01/06 20:37:19 fn Exp $

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

;; Current version of ZenICB
(defconst zenicb-version "$Revision: 1.31 $")


;; User options

(defvar zenicb-buffer-name "*zenicb*"
  "*Basic buffer name for Zen Internet Citizen's Band")

(defvar zenicb-signal-list '()
  "*List of regular expressions which cause signal notification.")

(defvar zenicb-beep-on-signal nil
  "*If t, beep on signals when not seen.
If \\='always, beep on all signals.")

(defvar zenicb-timestamp nil
  "*If nil, don't timestamp messages.
If t, timestamp messages.")

(defvar zenicb-timestamp-prefix "["
  "*What to add before the timestamp string.")

(defvar zenicb-timestamp-suffix "]"
  "*What to add after the timestamp string.")

(defvar zenicb-delete-preceding-whitespace nil
  "*Whether ZenIRC should delete any whitespaces before the first word
before sending it off to the server.")

(defvar zenicb-command-char ?/
  "*Char that begins a command at the beginning of a line.")

(defvar zenicb-send-lines t
  "*Can be t (pre-process newlines) or nil (line-by-line).
If set to t and a multi-line paragraph is copied into the buffer, convert
newlines to spaces and send the entire block.  If set to nil, send each
line separately.")

(defvar zenicb-verify-quit nil
  "*Confirm a /quit if set to t.")


;;; ICB connection-related variables.
(defvar zenicb-server-alist nil
  "*Association list of port/password/nick/initial-channel info for each
server.  The format for this alist is

SERVER PORT NICK PASSWORD INITIAL-CHANNEL

If any element is not set, ZenICB-provided defaults are used.")

;; Give a default for this since there's no easy way of guessing a server
;; name if you don't know any.
(defvar zenicb-server-default "evolve.icb.net"
  "*Server to use if no other is specified.
See `zenicb-server-alist' and `zenicb-establish-server-connection'.")

(defvar zenicb-port-default 7326
  "*Default server port to use if no other is specified.
See `zenicb-server-alist' and `zenicb-establish-server-connection'.")

(defvar zenicb-nick-default nil
  "*Nickname to use if no other is specified.
See `zenicb-server-alist' and `zenicb-establish-server-connection'.")

(defvar zenicb-password-default nil
  "*Default server password to use if no other is specified.
See `zenicb-server-alist' and `zenicb-establish-server-connection'.")

(defvar zenicb-channel-default "Meditation"
  "*Default channel to join if no other is specified.
See `zenicb-server-alist' and `zenicb-establish-server-connection'.")

(defvar zenicb-status-default "pil"
  "*Default channel mode to set if no other is specified.
See `zenicb-server-alist' and `zenicb-establish-server-connection'.")

(defvar zenicb-login-name-default nil
  "*Default user name to use if no other is specified.
See `zenicb-server-alist' and `zenicb-establish-server-connection'.")

(defvar zenicb-process-connect-function 'open-network-stream
  "*Function used to establish server connection.
This is called by `zenicb-establish-server-connection' and should take the
same arguments normally given to `open-network-stream'.
This function can be used to make proxy connections.")


;;; Misc variables of interest.
;;; Most of these are reasonable for users to modify.
(defvar zenicb-mode-map '()
  "*Sparse keymap for zenicb-mode")
(cond
 ((null zenicb-mode-map)
  (setq zenicb-mode-map (make-sparse-keymap))
  (define-key zenicb-mode-map "\n" 'zenicb-send-line)
  (define-key zenicb-mode-map "\C-m" 'zenicb-send-line)
  (define-key zenicb-mode-map "\C-c\C-r" 'zenicb-send-msg-last-rec)
  (define-key zenicb-mode-map "\C-c\C-s" 'zenicb-send-msg-last-sent)
  (define-key zenicb-mode-map ":" 'zenicb-self-insert-or-send-msg-last-rec)
  (define-key zenicb-mode-map ";" 'zenicb-self-insert-or-send-msg-last-sent)))

;; Debugging variables -- for the adventurous.
(defvar zenicb-debug-mainloop nil)
(defvar zenicb-debug-commands nil)
(defvar zenicb-debug-timer nil)
(defvar zenicb-bug-address "fn-zenicb@hungry.com")

;; Don't change this unless you know what to do.
(defconst zenicb-message-length-limit 252
  "Maximum length of messages that can be sent on a single line.")


;;; Local state variables.
;;; It's probably not useful for the user to change these, unless necessary
;;; for particular extensions.

(defvar zenicb-server nil)
(make-variable-buffer-local 'zenicb-server)

(defvar zenicb-port nil)
(make-variable-buffer-local 'zenicb-port)

(defvar zenicb-password nil)
(make-variable-buffer-local 'zenicb-password)

(defvar zenicb-nick nil)
(make-variable-buffer-local 'zenicb-nick)

(defvar zenicb-login-name nil)
(make-variable-buffer-local 'zenicb-login-name)

(defvar zenicb-channel nil)
(make-variable-buffer-local 'zenicb-channel)

(defvar zenicb-status nil)
(make-variable-buffer-local 'zenicb-status)

;; current group or user, or nil
(defvar zenicb-current-victim nil)
(make-variable-buffer-local 'zenicb-current-victim)

;; variables to store the nick you last sent to or that last sent to you
(defvar zenicb-msg-last-rec "")
(make-variable-buffer-local 'zenicb-msg-last-rec)

(defvar zenicb-msg-last-sent "")
(make-variable-buffer-local 'zenicb-msg-last-sent)

;; remember last person we saw a msg from.
(defvar zenicb-msg-last-seen nil)
(make-variable-buffer-local 'zenicb-msg-last-seen)

;; We use this marker instead of the process mark, because the latter goes
;; away when a process exits, which is a gratuitous nuisance.
(defvar zenicb-process-mark nil)
(make-variable-buffer-local 'zenicb-process-mark)

;; unprocessed data read from socket
(defvar zenicb-unprocessed-output nil)
(make-variable-buffer-local 'zenicb-unprocessed-output)

(defvar zenicb-time-last-event '(0 0))
(make-variable-buffer-local 'zenicb-time-last-event)

;; server version (set in login packet)
(defvar zenicb-server-version nil)
(make-variable-buffer-local 'zenicb-server-version)


;;; Standard hooks
(defvar zenicb-mode-hook nil
  "*Hook to run at the end of zenicb-mode.")

(defvar zenicb-startup-hook nil
  "*Hook run before establishing a server connection.")

(defvar zenicb-exit-hook nil
  "*Hook to run when zenicb exits.")

(defvar zenicb-connect-hook nil
  "*Hook to run registering with an ICB server.")

(defvar zenicb-timer-hook nil
  "*Timer hook variable.")

(defvar zenicb-signal-hook '(zenicb-signal)
  "*Signal hook variable.")

(defvar zenicb-message-hook nil
  "*Hook to run whenever a message is inserted in the ZenICB buffer.
The buffer is narrowed to the region containing the newly-inserted text,
and is called with two arguments: the process (if known) and the unmodified
string.  This string may not match exactly what is currently in the buffer,
since functions on this hook can easily modify the latter.")

;; Hooks for various server commands.
;; These are commands that the user types, e.g. "/quit" (the best command
;; of all!).  For any given command CMD, the hook zenicb-command-CMD-hook
;; is run.  If the user types a command for which there is no hook, the
;; command is passed directly to the server.

(defvar zenicb-command-beep-hook '(zenicb-command-beep))
(defvar zenicb-command-boot-hook '(zenicb-command-boot))
(defvar zenicb-command-cancel-hook '(zenicb-command-cancel))
(defvar zenicb-command-command-char-hook '(zenicb-command-command-char))
(defvar zenicb-command-delete-hook '(zenicb-command-delete))
(defvar zenicb-command-drop-hook '(zenicb-command-drop))
(defvar zenicb-command-echo-hook '(zenicb-command-echo))
(defvar zenicb-command-echoback-hook '(zenicb-command-echoback))
(defvar zenicb-command-exclude-hook '(zenicb-command-exclude))
(defvar zenicb-command-g-hook '(zenicb-command-g))
(defvar zenicb-command-group-hook '(zenicb-command-group))
(defvar zenicb-command-help-hook '(zenicb-command-help))
(defvar zenicb-command-hush-hook '(zenicb-command-hush))
(defvar zenicb-command-invite-hook '(zenicb-command-invite))
(defvar zenicb-command-join-hook '(zenicb-command-join))
(defvar zenicb-command-language-hook '(zenicb-command-language))
(defvar zenicb-command-m-hook '(zenicb-command-m))
(defvar zenicb-command-msg-hook '(zenicb-command-msg))
(defvar zenicb-command-motd-hook '(zenicb-command-motd))
(defvar zenicb-command-news-hook '(zenicb-command-news))
(defvar zenicb-command-nick-hook '(zenicb-command-nick))
(defvar zenicb-command-nobeep-hook '(zenicb-command-nobeep))
(defvar zenicb-command-nosecure-hook '(zenicb-command-nosecure))
(defvar zenicb-command-notify-hook '(zenicb-command-notify))
(defvar zenicb-command-pass-hook '(zenicb-command-pass))
(defvar zenicb-command-ping-hook '(zenicb-command-ping))
(defvar zenicb-command-query-hook '(zenicb-command-query))
(defvar zenicb-command-quit-hook '(zenicb-command-quit))
(defvar zenicb-command-read-hook '(zenicb-command-read))
(defvar zenicb-command-reg-hook '(zenicb-command-reg))
(defvar zenicb-command-register-hook '(zenicb-command-register))
(defvar zenicb-command-secure-hook '(zenicb-command-secure))
(defvar zenicb-command-shush-hook '(zenicb-command-shush))
(defvar zenicb-command-shuttime-hook '(zenicb-command-shuttime))
(defvar zenicb-command-status-hook '(zenicb-command-status))
(defvar zenicb-command-talk-hook '(zenicb-command-talk))
(defvar zenicb-command-topic-hook '(zenicb-command-topic))
(defvar zenicb-command-version-hook '(zenicb-command-version))
(defvar zenicb-command-w-hook '(zenicb-command-w))
(defvar zenicb-command-who-hook '(zenicb-command-who))
(defvar zenicb-command-whereis-hook '(zenicb-command-whereis))
(defvar zenicb-command-whois-hook '(zenicb-command-whois))
(defvar zenicb-command-write-hook '(zenicb-command-write))

;; Hook run after various kinds of messages are sent.
;; This hook get two args: a process, and a message.

(defvar zenicb-send-line-hook nil
  "*Hook run after a line of input is sent to the server.
Functions on this hook get three args: two integers specifying the
beginning and ending points in the buffer containing the text sent, and a
string representing the formatted text actually sent to the server (the
main difference is that embedded newlines are mapped to spaces).")

;; Hooks run in response to messages from the server.
;; For any message of type TYPE, the hook zenicb-server-TYPE-hook is run.
(defvar zenicb-server-a-hook '(zenicb-server-a) "login")
(defvar zenicb-server-b-hook '(zenicb-server-b) "public")
(defvar zenicb-server-c-hook '(zenicb-server-c) "private")
(defvar zenicb-server-d-hook '(zenicb-server-d) "status")
(defvar zenicb-server-e-hook '(zenicb-server-e) "error")
(defvar zenicb-server-g-hook '(zenicb-server-g) "exit")
(defvar zenicb-server-h-hook '(zenicb-server-h) "command")
(defvar zenicb-server-i-hook '(zenicb-server-i) "command")
(defvar zenicb-server-j-hook '(zenicb-server-j) "protocol")
(defvar zenicb-server-k-hook '(zenicb-server-k) "beep")
(defvar zenicb-server-l-hook '(zenicb-server-l) "ping")
(defvar zenicb-server-m-hook '(zenicb-server-m) "pong")


(defun zenicb-mode ()
  "Major mode for wasting major time on ICB."
  (kill-all-local-variables)

  (setq mode-name "ZenICB")
  (setq major-mode 'zenicb-mode)
  (use-local-map zenicb-mode-map)
  (setq mode-line-process '(":%s"))
  (setq mode-line-format
        '( ""
           mode-line-modified
           mode-line-buffer-identification
           " "
           global-mode-string
           " "
           (-3 . "%p")
           " %[("
           mode-name
           mode-line-process
           "%n"
           minor-mode-alist
           ")%] "
           zenicb-nick
           (zenicb-current-victim ("->" zenicb-current-victim))
           " "
           "%-"))
  (zenicb-run-hook 'zenicb-mode-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code to handle connection to server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun zenicb (&optional prefix)
  "Waste time on ICB.


If an ICB session already exists, switch to that session.
With prefix arg, start a new session even if another exists.

If buffer exists but ZenICB process is not running, make new process.
If buffer exists and ZenICB process is running, just switch to that buffer.
If an explicit numeric prefix argument is given (or this function is called
from lisp with a numeric argument), switch to the buffer named
\"*zenicb*<prefix>\", e.g. \"*zenicb*<2>\".  If there is no process in that
buffer, start one.
If a prefix argument is given but it is not a number, create a new buffer
and start a process in it.  This is the same as calling the function from
lisp with an argument of `t'."
  (interactive "P")
  (let* ((zenicb-buffer (if prefix
                           (generate-new-buffer zenicb-buffer-name)
                         (get-buffer-create zenicb-buffer-name)))
         (process (get-buffer-process zenicb-buffer)))
    (pop-to-buffer zenicb-buffer)
    (set-buffer-multibyte nil)

    (cond
     ((and process
           (memq (process-status process) '(open run))))
     (t
      (zenicb-mode)

      (setq zenicb-unprocessed-output "")
      (setq zenicb-current-victim nil)

      ;; Time of last event in ZenICB -- set it to "now".
      (setq zenicb-time-last-event
            (zenicb-time-to-int (current-time-string)))

      ;; note the semantics here that the current buffer when
      ;; zenicb-startup-hook is run is zenicb-buffer.
      (zenicb-run-hook 'zenicb-startup-hook)

      ;; Do this before opening network stream, if currently unset.
      ;; If already set, preserve so that user can save input.
      (or zenicb-process-mark
          (setq zenicb-process-mark
                (set-marker (make-marker) (point-max) zenicb-buffer)))

      (or (setq process
                (zenicb-establish-server-connection zenicb-buffer))
          (error "zenicb: could not establish any server connection."))

      (set-process-coding-system process 'binary 'iso-latin-1)
      (set-process-buffer process zenicb-buffer)
      (set-process-filter process 'zenicb-filter)
      (set-process-sentinel process 'zenicb-sentinel)
      (zenicb-login process)
      (zenicb-run-hook 'zenicb-connect-hook process)))))

(defun zenicb-select (&optional server port nick channel passwd)
  "Run manually or by issuing /server from a ZenICB buffer.

This function starts a new ZenICB buffer and connects to a given server.
Variables not already given are queried for, using zenicb-server-alist
for default values. zenicb-server-alist is also updated each time this
function is issued."

  (interactive)
  (if (integerp port) (setq port (int-to-string port)))
  (let ((new-server) (new-port) (new-nick) (new-channel) (new-passwd))
    (setq
     new-server
     ; server to connect to
     (or server
     ; server is not given, query user
	 (completing-read "Server: " zenicb-server-alist nil nil
			  (or
			   (car (car zenicb-server-alist))
			   ; last resort default
			   zenicb-server-default)))
     new-port
     ; port to connect to
     (or
      port
      ; port is not given, query user
      (read-string "Port: "
		   (or
		    (if (car (cdr (assoc new-server zenicb-server-alist)))
			(int-to-string
			 (car (cdr (assoc new-server zenicb-server-alist)))))
		    (if zenicb-port (int-to-string zenicb-port))
		    ; last resort default
		    (int-to-string zenicb-port-default))))
     new-nick
     ; nickname to use
     (or
      nick
      ; nickname is not given, query user
      (read-string "Nickname: "
		   (or
		    (car (nthcdr 3 (assoc new-server zenicb-server-alist)))
		    zenicb-nick
		    ; last resort default
		    (user-login-name))))
     new-channel
     ; initial channel to join
     (or
      channel
      (read-string "Channel: "
                   (or
                    (car (nthcdr 4 (assoc new-server zenicb-server-alist)))
                    zenicb-channel
                    ; last resort default
                    zenicb-channel-default)))
     new-passwd
     ; password.
     (or
      passwd
      (read-passwd "Enter password (default is unset): ")))

    ; update zenicb-server-alist
    (let ((new-list (list new-server (string-to-number new-port) new-nick new-passwd new-channel)))
      (if (not (member new-list zenicb-server-alist))
	  ; a new entry is given
	  (setq zenicb-server-alist
		(cons new-list zenicb-server-alist))
	; move old entry to the top of zenicb-server-alist
	(setq zenicb-server-alist (delete new-list zenicb-server-alist)
	      zenicb-server-alist (cons new-list zenicb-server-alist)))
      ; make sure we don't try to connect to anything else then the
      ; given server
      (let ((zenicb-server-alist (list new-list)))
	; run the actual connection, at last
	(zenicb t)))))

(defun zenicb-establish-server-connection (buffer &optional alist)
  "Waste time by connecting to an ICB server.
This function takes two arguments: a buffer and an optional alist.
If none is specified, the default is `zenicb-server-alist'.

For each server in the alist, attempt to connect to it on the appropriate
port and with the appropriate nicknames, etc.

If any of the elements in the list for a server is unspecified, one of the
following defaults is used, in the specified order of priority (names in
caps preceded with `$' are environment variables):

   port:     zenicb-port-default
   password: zenicb-password-default
   nickname: zenicb-nick-default, (user-login-name)
   username: zenicb-login-name-default, $USER, (user-login-name)

Finally, if zenicb-server-alist is nil and no other alist is specified,
connect to `zenicb-server-default' using defaults as described above."
  (with-current-buffer buffer
    (save-excursion
      (or alist
          (setq alist zenicb-server-alist)
          (setq alist (list (list zenicb-server-default))))
      (let ((procname (concat "zenicb:" (buffer-name)))
            ent server port proc)
        (while alist
	  (setq ent (car alist))
	  (setq alist (cdr alist))

	  (setq server (or (car ent)
			   zenicb-server-default
			   (error "no server specified.")))

	  (setq port (or (nth 1 ent)
		         zenicb-port-default
		         7326))

	  (condition-case data
	      (progn
	        (zenicb-message buffer 'connect-try server port)
	        ;; Do a redisplay before connecting, in case the server is
	        ;; slow to respond.
	        (sit-for 0)
	        (setq proc (funcall zenicb-process-connect-function
				    procname buffer server port))
	        ;; Update connection status in modeline.
	        (force-mode-line-update)
	        (setq alist nil)
	        (setq zenicb-server          server)

	        (setq zenicb-port            port)
                (setq zenicb-nick            (or (nth 2 ent)
                                                 zenicb-nick-default
                                                 (user-login-name)
                                                 "nil")) ; it -is- funny
	        (setq zenicb-password        (or (nth 3 ent)
					         zenicb-password-default))
                (setq zenicb-channel (or (nth 4 ent)
                                         zenicb-channel-default
                                         "Meditation"))
	        (setq zenicb-login-name (or (nth 5 ent)
                                            zenicb-login-name-default
                                            (getenv "USER")
                                            (user-login-name)
                                            "nil")))
	    (quit
	     (setq alist nil)
	     (zenicb-message buffer 'connect-abort))

	    (file-error
	     ;; file-error "connection failed" "connection timed out" host proc
	     ;; file-error "connection failed" "connection refused" host proc
	     (if (string= (nth 1 data) "connection failed")
	         (zenicb-message buffer 'connect-failed server port
			         (nth 2 data))
	       (signal 'file-error data)))
	    (error
	     ;; data == (error "Unknown host \"foo\"")
	     (if (string-match "^Unknown host" (nth 1 data))
	         (zenicb-message buffer 'connect-failed server port
			         (nth 1 data))
	       (apply 'signal data)))))
        proc))))

;; send nick, user-name, initial-channel, initial-status, and password
(defun zenicb-login (proc)
  (zenicb-send-string proc ?a
                      (format "%s\C-a%s\C-a%s\C-a%s\C-a%s\C-a%s\C-a%d"
                               zenicb-login-name
                               zenicb-nick
                               zenicb-channel
                               "login"
                               zenicb-password
                               zenicb-status
                               0)))

(defun zenicb-sentinel (proc str)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (zenicb-run-hook 'zenicb-exit-hook proc str)
      (zenicb-message proc 'sentinel (current-time-string)))))

;; This function takes a chunk of text from the server, and any text
;; left over from the last chunk, and passes it to zenicb-parse-output
;; to be interpreted.
(defun zenicb-filter (proc string)
  (let ((orig-buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer (process-buffer proc))
          (setq zenicb-unprocessed-output
                (zenicb-parse-output
                 proc (concat zenicb-unprocessed-output string))))
      (set-buffer orig-buffer))))

;; This routine takes a bunch of text from the server, and any remnants
;; from the last bunch, and splits it into lines. The lines are passed to
;; zenicb-parse-server-message to be parsed and then whatever needs to be
;; done for that server message is done.
(defun zenicb-parse-output (proc string)
  (while (let ((length (aref string 0)))
	   (and (> (length string) length)
		(let ((type (aref string 1))
		      (line (substring string 2 length)))
		  (setq string (substring string (1+ length)))
		  (zenicb-parseline proc type line)
		  (not (string-equal string ""))))))
  string)

(defun zenicb-parseline (proc type string)
  (let* ((hook-name (format "zenicb-server-%c-hook" type))
         (hook (intern-soft hook-name))
         (parsedmsg (zenicb-split-string string))
         (hook-docstring
          (documentation-property hook 'variable-documentation))
         (signal-string
          (format "%s:%s" hook-docstring string)))
    (cond
     (zenicb-debug-mainloop
      (zenicb-message proc 'debug (concat "Hook: " hook-name))
      (zenicb-message proc 'debug
                      (concat "Parsed: "
                              (prin1-to-string parsedmsg)))))
    (zenicb-timer-handler proc)
    (if (boundp hook)
        (zenicb-run-hook hook proc parsedmsg)
      (zenicb-message proc 'server string))
    (if (zenicb-signal-p signal-string)
        (zenicb-run-hook 'zenicb-signal-hook proc signal-string))))


;; split a line along ^A's
(defun zenicb-split-string (line &optional s)
  (or s
      (setq s "\C-a"))
  (let ((posn (string-match s line)))
    (if posn
	(cons (substring line 0 posn)
	      (zenicb-split-string (substring line (1+ posn)) s))
      (cons line nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Send data to the ICB server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zenicb-send-string (proc type str)
  (let ((message (format "%c%c%s\0" (+ 2 (length str)) type str)))
;    (zenicb-message proc 'debug (concat "Message: "
;                                        (prin1-to-string message)))
    (process-send-string proc message)))

;;
;; parse a line into the first word and the rest.
;;
;; This returns ("word" . "rest"), where word means adjacent non-space
;; characters. Any amount of whitespace is skipped after the first word,
;; and "rest" is the rest of the line. If there is no "rest", a "rest"
;;  of "" is constructed.
;;
;;
(defun zenicb-parse-firstword (string)
  (save-match-data
    (if (string-match "\\( +\\)" string)
	(cons (substring string 0 (match-beginning 1))
	      (substring string (match-end 1)))
      (cons string ""))))

(defun zenicb-send-line ()
  "Send current line to zenicb server."
  (interactive)
  (end-of-line)
  (insert "\n")
  (let* ((proc (get-buffer-process (current-buffer)))
         (proc-mark zenicb-process-mark)
	 (string (buffer-substring proc-mark (point)))
	 (posn 0))
    (if (< proc-mark (point))
	(progn
	  (set-marker proc-mark (point))
	  (save-match-data
	    (if (not (string-match "\\`\\s-*\\'" string)) ; No blank strings
		(progn
		  ;; Handle newlines in input.
		  (while (string-match "\\(\\`\\|\n\\)\\(\\s-*\n\\)" string)
		    (setq string (concat (substring
					  string 0 (match-beginning 2))
					 (substring string (match-end 2)))))
		  (if zenicb-send-lines
		      (progn
			(while (string-match "\n" string 0)
			  (aset string (match-beginning 0) 32)
			  (setq posn (match-end 0)))
			(zenicb-run-hook 'zenicb-send-line-hook
					 (substring string 0 -1))
			(cond ((eq (aref string 0) zenicb-command-char)
			       (zenicb-do-command proc
						  (substring string 1 -1)))
			      ((not (eq zenicb-current-victim nil))
			       (zenicb-split (substring string 0 -1) 220
					     'zenicb-send-private proc
					     zenicb-current-victim))
			      (t
			       (zenicb-split (substring string 0 -1) 220
					     'zenicb-send-public proc))))
		    (for-each
		     (function
		      (lambda (ln)
			(zenicb-run-hook 'zenicb-send-line-hook ln)
			(cond ((eq (aref ln 0) zenicb-command-char)
			       (zenicb-do-command proc (substring ln 1)))
			      ((not (eq zenicb-current-victim nil))
			       (zenicb-split ln 220
					     'zenicb-send-private proc
					     zenicb-current-victim))
			      (t
			       (zenicb-split ln 220
					     'zenicb-send-public proc)))))
		     (zenicb-split-string (substring string 0 -1) "\n")))))))
      ;; if the user presses enter, jump to the bottom of the buffer
      (goto-char (point-max)))))
(defun zenicb-send-public (message proc)
  (zenicb-send-string proc ?b message))

;; Handle a zenicb / command typed by the user.  Check to see if there's a
;; hook for the command and if so, execute the hook, otherwise just send the
;; command line unaltered to the server.
(defun zenicb-do-command (proc cmdline)
  (let* ((parsedcmd (zenicb-parse-firstword cmdline))
	 (cmdname (car parsedcmd))
	 (hook (intern (concat "zenicb-command-" cmdname "-hook"))))
    (cond
     (zenicb-debug-commands
      (zenicb-message proc 'debug (concat "Hook: " (symbol-name hook)))
      (zenicb-message proc 'debug (concat "Parsed: "
                                          (prin1-to-string parsedcmd)))))
    ;; Call the hook, if it's there.
    (if (boundp hook) (zenicb-run-hook hook proc parsedcmd)
      ;; Otherwise, signal error
      (zenicb-message proc 'nocmd cmdline))))

;; Send messages to a) the person whom you last /m'd, and
;; b) the person who last /m'd you.
;; Copied from zenirc.el.
(defun zenicb-insert-at-proc-mark (&rest args)
  (let ((proc zenicb-process-mark)) ;(process-mark zenicb-process)))
    (cond ((= (point) proc)
           (apply 'insert args))
          (t
           (let ((point (point-marker)))
             (goto-char proc)
             (apply 'insert args)
             (goto-char point))))))

(defun zenicb-send-msg-last-rec ()
  (interactive)
  (zenicb-insert-at-proc-mark (concat (char-to-string zenicb-command-char)
				      "m ")
			      zenicb-msg-last-rec " "))

(defun zenicb-send-msg-last-sent ()
  (interactive)
  (zenicb-insert-at-proc-mark (concat (char-to-string zenicb-command-char)
				      "m ")
			      zenicb-msg-last-sent " "))

(defun zenicb-self-insert-or-send-msg-last-rec ()
  (interactive)
  (save-match-data
    (if (= (point) zenicb-process-mark) ;(process-mark zenicb-process))
	(zenicb-send-msg-last-rec)
      (insert (this-command-keys)))))

(defun zenicb-self-insert-or-send-msg-last-sent ()
  (interactive)
  (save-match-data
    (if (= (point) zenicb-process-mark) ;(process-mark zenicb-process))
	(zenicb-send-msg-last-sent)
      (insert (this-command-keys)))))

(defun zenicb-timestamp-string ()
  (substring (current-time-string) 11 16))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Signal-handling code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; determine if an event is worthy of a signal
(defun zenicb-signal-p (msg)
  (zenicb-string-match-list msg zenicb-signal-list))

;; Try matching msg in regexp-list.
;; If no match is found, preserve old match data and return nil.
;; Otherwise, return value of sucessful string-match and leave modified
;; match-data intact.
;; do a signal (pop up buffer, beep, whatever)
(defun zenicb-signal (proc msg)
  (let ((proc-window (get-buffer-window (process-buffer proc))))
    (cond ((and proc-window
                (pos-visible-in-window-p zenicb-process-mark proc-window)
                (not (string-equal zenicb-beep-on-signal 'always))))
          (t
           (and zenicb-beep-on-signal (ding t))
           (zenicb-message nil 'signal (buffer-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert strings into the ZenICB buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zenicb-message (proc-or-buffer string &rest args)
  (let ((proc nil)
        (buffer nil)
        (sym nil))

    (cond ((processp proc-or-buffer)
           (setq buffer (process-buffer proc-or-buffer))
           (setq proc proc-or-buffer))
          ((or (bufferp proc-or-buffer)
               (stringp proc-or-buffer))
           (setq buffer (get-buffer proc-or-buffer))
           (setq proc (get-buffer-process buffer))))

    (cond
     ((symbolp string)
      (setq sym string)
      (setq string (zenicb-lang-retrieve-catalog-entry string))))
    (and args
         (if string
             (setq string (apply 'format string args))
	   (setq string (format "[raw] %s" args))))
    (cond
     ((null proc-or-buffer)
      (message "%s" string))
     (t
      (setq string (concat string "\n"))
      (let ((orig-buffer (current-buffer))
            region-begin
            window
            window-point
            current-point-mark)
        (unwind-protect
            (progn
              (set-buffer buffer)
              (setq window (get-buffer-window buffer))
              (setq region-begin (marker-position zenicb-process-mark))
              (setq current-point-mark (point-marker))

              ;; If process mark is at window start, insert-before-markers
              ;; will insert text off-window since it's also inserting before
              ;; the start window mark.  Preserve window start's point in
              ;; that case.
              (and window
                   (= zenicb-process-mark (window-start window))
                   (setq window-point region-begin))

              (goto-char zenicb-process-mark)
              (insert-before-markers string)
              (goto-char region-begin)
              (while (search-forward "\C-m" zenicb-process-mark t)
                (delete-char -1))
              (and zenicb-message-hook
                   (save-restriction
                     (narrow-to-region region-begin zenicb-process-mark)
                     (zenicb-run-hook 'zenicb-message-hook proc sym string)))
              (goto-char current-point-mark)
              (and window-point
                   (set-window-start window window-point 'noforce)))
          (set-buffer orig-buffer)))))))
(defun zenicb-display-string (proc string)
  (let ((len (- (length string) 1)))
    (if (char-equal (aref string len) ?\n)
        (setq string (substring string 0 -1))))
  (zenicb-message proc string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ZenICB hook handling functions
;;;
;;; ZenICB uses a somewhat nonstandard hook mechanism. Hook sysmbols
;;; are manipulated with zenicb-add-hook and zenicb-delete hook, and
;;; are executed with zenicb-run-hook. A hook symbol is a list of
;;; symbols that are function names. When a hook is run with
;;; zenicb-run-hook, each symbol in the list is run in turn - unless
;;; one of the hooks sets the variable zenicb-run-next-hook to nil. In
;;; this case, zenicb-run-hook immediatelly returns to the caller.
;;; Unlike emacs 19 hooks, ZenICB hooks are called with arguments.
;;; ZenICB hooks return the value of the last hook run.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; run a hook
;;
(defun zenicb-run-hook (hooksym &rest args)
  "Take hook name HOOKSYM and run it, passing optional args ARGS.
HOOKSYM should be a symbol, a hook variable.
If the hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with args ARGS.
If it is a list, the elements are called, in order, with ARGS, if
zenicb-run-next-hook is t (the default). Otherwise, the hooks after
the one that set zenicb-run-next-hook are not called, and control is
returned to the caller. (zenicb-run-hook) returns the value returned
from the last hook run."
      (let ((zenicb-run-next-hook t)
            (result))
        (and (boundp hooksym)
             (symbol-value hooksym)
             (let ((value (symbol-value hooksym)))
               (if (and (listp value)
                        (not (eq (car value) 'lambda)))
                   (while (and value zenicb-run-next-hook)
                     (setq result (apply (car value) args))
                     (setq value (cdr value)))
                 (setq result (apply value args)))))
        result))
;;
;; add a function to a hook symbol
;;
(defun zenicb-add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (or (boundp hook) (set hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(set hook (list old))))
  (or (if (consp function)
	  (member function (symbol-value hook))
	(memq function (symbol-value hook)))
      (set hook
	   (if append
	       (nconc (symbol-value hook) (list function))
	     (cons function (symbol-value hook))))))
;;
;; remove a function from a hook symbol
;;
(defun zenicb-delete-hook (hook function)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `zenicb-add-hook'."
  (if (or (not (boundp hook))		;unbound symbol, or
	  (null (symbol-value hook))	;value is nil, or
	  (null function))		;function is nil, then
      nil				;Do nothing.
    (let ((hook-value (symbol-value hook)))
      (if (consp hook-value)
	  (setq hook-value (delete function hook-value))
	(if (equal hook-value function)
	    (setq hook-value nil)))
      (set hook hook-value))))

(fset 'zenicb-remove-hook 'zenicb-delete-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ZenICB time handling functions
;;;
;;; These functions are used to implement time handling in ZenICB.
;;; Much of this code was lifted from the ZenIRC irc client, which,
;;; in turn, got them from Kiwi 4.30 irc client.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zenicb-time-to-int (timestr)
  "Convert from time in string format as returned by current-time-string
to a double integer format, as returned by file-attributes.

Written by Stephen Ma <ma_s@maths.su.oz.au>"
  (let* ((norm+ '(lambda (num1 num2)
		  (let ((sumh (+ (car num1) (car num2)))
			(suml (+ (car (cdr num1)) (car (cdr num2)))))
		    (list (+ sumh (/ suml 65536)) (% suml 65536)))))
	 (norm* '(lambda (num1 num2)
		  (let ((prodh (* num1 (car num2)))
			(prodl (* num1 (car (cdr num2)))))
		    (list (+ prodh (/ prodl 65536)) (% prodl 65536)))))
	 (seconds (string-to-number (substring timestr 17 19)))
	 (minutes (string-to-number (substring timestr 14 16)))
	 (hours (string-to-number (substring timestr 11 13)))
	 (partdays (1- (string-to-number (substring timestr 8 10))))
	 (years (string-to-number (substring timestr 20 24)))
	 (days (+ partdays
		  (cond ((and (= (% years 4) 0)
			      (/= (% years 100) 0))
			 (cdr (assoc (substring timestr 4 7)
				     '(("Jan" . 0)
				       ("Feb" . 31)
				       ("Mar" . 60)
				       ("Apr" . 91)
				       ("May" . 121)
				       ("Jun" . 152)
				       ("Jul" . 182)
				       ("Aug" . 213)
				       ("Sep" . 244)
				       ("Oct" . 274)
				       ("Nov" . 305)
				       ("Dec" . 335)))))
			(t (cdr (assoc (substring timestr 4 7)
				       '(("Jan" . 0)
					 ("Feb" . 31)
					 ("Mar" . 59)
					 ("Apr" . 90)
					 ("May" . 120)
					 ("Jun" . 151)
					 ("Jul" . 181)
					 ("Aug" . 212)
					 ("Sep" . 243)
					 ("Oct" . 273)
					 ("Nov" . 304)
					 ("Dec" . 334))))))
		  (* (- years 1970) 365)
		  (/ (- years 1969) 4)
		  (- (/ (- years 1901) 100)))))
    (funcall norm+
	     (funcall norm*
		      60
		      (funcall norm+
			       (funcall norm*
					60
					(funcall norm+
						 (funcall norm*
							  24
							  (list 0 days))
						 (list 0 hours)))
			       (list 0 minutes)))
	     (list 0 seconds))))

(defun zenicb-time= (a b)
  "Compare two times, and return true if they are equal."
  (and (= (nth 0 a) (nth 0 b))
       (= (nth 1 a) (nth 1 b))))

(defun zenicb-time< (a b)
  "Compare two times, and return t if the first is earlier than the second."
  (or (< (nth 0 a) (nth 0 b))
      (and (= (nth 0 a) (nth 0 b))
	   (< (nth 1 a) (nth 1 b)))))

(defun zenicb-time-diff (a b)
  "Return the difference between two times. This function requires
the second argument to be earlier in time than the first argument."
  (cond ((= (nth 0 a) (nth 0 b)) (list 0 (- (nth 1 a) (nth 1  b))))
	((> (nth 1 b) (nth 1 a)) (list (- (nth 0 a) (nth 0 b) 1)
				       (- (+ 65536 (nth 1 a)) (nth 1 b))))
	(t (list (- (nth 0 a) (nth 0 b))
		 (- (nth 1 a) (nth 1 b))))))

;; Convert a number of seconds since the epoch (in ASCII) into an
;; ASCII string representing the time.
;;
(defun zenicb-convert-date (seconds)
  (let (millions units high low)
    (if (string-match "\\(......\\)$" seconds)
        (setq millions (string-to-number (substring seconds 0 (match-beginning 1)))
              units (string-to-number (substring seconds (match-beginning 1))))
      (setq millions 0
            units (string-to-number seconds)))
    (setq high (+ (* millions 15) (/ (* millions 265) 1024) (/ units 65536))
          low (+ (% (+ (* (% millions 4) 16384) (* millions 576)) 65536)
                 (% units 65536)))
    (if (> low 65535)
        (setq low (- low 65536)
              high (1+ high)))
    (substring (current-time-string (cons high low)) 4 16)))

(defun zenicb-timer-handler (proc)
  "Call zenicb-timer-hook as often as possible. The maximum delay between
calls of zenicb-timer-hook is how often a server pings the client."
  (let ((now (zenicb-time-to-int (current-time-string))))
    (if (zenicb-time< '(0 0) (zenicb-time-diff now zenicb-time-last-event))
	(progn
	  (and zenicb-debug-timer
               (zenicb-message proc 'debug
                               (concat "Timer: %s" (current-time-string))))
	  (zenicb-run-hook 'zenicb-timer-hook proc now)
	  (setq zenicb-time-last-event now)))))

;;
;; Display the idle time in a nice format.
;;
(defun zenicb-convert-time (seconds)
  (let ((units (string-to-number seconds)))
    (cond
     ((< units 90) "-")
     (t (format "%dm" (/ (+ units 30) 60))))))
;;
;; Split MESSAGE into MAXLENGTH sized chunks, and call FUNCTION with each
;; piece, plus the additional ARGS.
;;
(defun zenicb-split (message maxlength function &rest args)
  (let ((length (length message)))
    (while (> length maxlength)
      (let ((string (substring message 0 maxlength)))
	(setq message (substring message maxlength))
	(setq length (- length maxlength))
	(apply function string args))))
  (apply function message args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server message hooks
;;;
;;; Server message hooks are called with two arguments. The first
;;; is the ZenICB process. The second is a list of the form:
;;;    (command arg1 arg2 ... argN)
;;; `command' is a character corresponding to the server message
;;; letter (a-m). arg1 through argN are strings, the arguments to
;;; the command that the server sent, which were separated by C-a's
;;; in the unparsed message.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zenicb-server-a (proc parsedmsg)
  (zenicb-message proc 'loginok))
;;
;; b - public message
;;
(defun zenicb-server-b (proc parsedmsg)
  (zenicb-message proc 'public
                  (if zenicb-timestamp
                      (concat (nth 0 parsedmsg)
                              zenicb-timestamp-prefix
                              (zenicb-timestamp-string)
                              zenicb-timestamp-suffix)
                    (nth 0 parsedmsg))
                  (nth 1 parsedmsg)))
;;
;; c - private message
;;
(defun zenicb-server-c (proc parsedmsg)
  (setq zenicb-msg-last-rec (nth 0 parsedmsg))
  (zenicb-message proc 'private
                  (if zenicb-timestamp
                      (concat (nth 0 parsedmsg)
                              zenicb-timestamp-prefix
                              (zenicb-timestamp-string)
                              zenicb-timestamp-suffix)
                    (nth 0 parsedmsg))
                  (nth 1 parsedmsg)))
;;
;; d - status message
;;
(defun zenicb-server-d (proc parsedmsg)
  (zenicb-message proc 'status (nth 0 parsedmsg) (nth 1 parsedmsg)))
;;
;; e - error packet
;;
(defun zenicb-server-e (proc parsedmsg)
  (zenicb-message proc 'errormsg (nth 0 parsedmsg)))
;;
;; g - signoff
;;
(defun zenicb-server-g (proc parsedmsg)
  (zenicb-message proc 'goaway))
;;
;; i - command output
;;
(defun zenicb-server-i (proc parsedmsg)
  (let ((reply-type (nth 0 parsedmsg)))
    (cond
     ((string= reply-type "wh") ; who reply header
      (zenicb-display-string
       proc (format "[info]  Nickname     Idle  Sign-On       Account")))
     ((string= reply-type "wl") ; who reply
      (zenicb-display-string
       proc (format "[info] %s%-12s%5s  %s  %s@%s %s"
		    (if (string= (nth 1 parsedmsg) "m") "*" " ")
		    (nth 2 parsedmsg)
		    (zenicb-convert-time (nth 3 parsedmsg))
		    (zenicb-convert-date (nth 5 parsedmsg))
		    (nth 6 parsedmsg)
		    (nth 7 parsedmsg)
		    (nth 8 parsedmsg))))
     ((string= reply-type "co") ; comment
      (let ((message (nth 1 parsedmsg)))
	(if message
	    (let ((message
		   (if (string-match "\\( +\\)$" message)
		       (substring message 0 (match-beginning 1))
		     message)))
	      (zenicb-display-string proc (format "[info] %s" message)))
	  (zenicb-display-string proc "[info]"))))
     (t
      (zenicb-display-string
       proc (format "[debug] packet type i, subtype %s, data %s"
		    (prin1-to-string reply-type)
		    (prin1-to-string parsedmsg)))))))
(fset 'zenicb-server-h 'zenicb-server-i)
;;
;; j - protocol message
;;
(defun zenicb-server-j (proc parsedmsg)
  (zenicb-message proc 'protocol
                  (nth 1 parsedmsg)
                  (nth 2 parsedmsg)
                  (nth 0 parsedmsg)))
;;
;; k - beep
;;
(defun zenicb-server-k (proc parsedmsg)
  (zenicb-message proc 'beep (nth 0 parsedmsg)))
;;
;; l - ping
;;
(defun zenicb-server-l (proc parsedmsg)
  (zenicb-message proc 'ping (nth 0 parsedmsg))
  (zenicb-send-string proc ?m ""))

;;
;; m - pong
;;
(defun zenicb-server-m (proc parsedmsg)
  (zenicb-message proc 'pong (nth 1 parsedmsg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; display byte count, time connected, etc.
;; /bcount [victim]
;;
;(defun zenicb-command-bcount (proc parsedcmd)
;  (zenicb-send-string proc ?h (concat "m\C-aserver bcount " (cdr parsedcmd))))
;;
;; Reach out and beep someone
;;
(defun zenicb-command-beep (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "beep\C-a" (cdr parsedcmd))))
;;
;; Boot someone out of a group
;; /boot victim
;;
(defun zenicb-command-boot (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "boot\C-a" (cdr parsedcmd))))
;;
;; Cancel an invitation
;; /cancel victim
;;
(defun zenicb-command-cancel (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "cancel\C-a" (cdr parsedcmd))))
;;
;; Change the command character dynamically.
;; /command-char command-char
;; No string does nothing.
;;
(defun zenicb-command-command-char (proc parsedcmd)
  (if (not (string= "" (cdr parsedcmd)))
	   (setq zenicb-command-char (string-to-char (cdr parsedcmd)))))
;;
;; Delete a nickname from the server's database.
;; /delete password
;;
(defun zenicb-command-delete (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "delete\C-a" (cdr parsedcmd))))
;;
;; Drop connection
;; /drop nick passwd
;;
(defun zenicb-command-drop (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "drop\C-a" (cdr parsedcmd))))
;;
;; Change echoback status
;; /echo [on|off|verbose]
;;
(defun zenicb-command-echo (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "echoback\C-a" (cdr parsedcmd))))
(fset 'zenicb-command-echoback 'zenicb-command-echo)
;;
;; Exclude someone from getting a public message
;; /exclude nick message
;;
(defun zenicb-command-exclude (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "exclude\C-a" (cdr parsedcmd))))
;;
;; change current group
;; /group groupname (or /g, or /join)
;;
(defun zenicb-command-group (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "g\C-a" (cdr parsedcmd))))
(fset 'zenicb-command-g 'zenicb-command-group)
(fset 'zenicb-command-join 'zenicb-command-group) ; for the irc-impaired
;;
;; get help
;; /help
;;
(defun zenicb-command-help (proc parsedcmd)
  (zenicb-send-string proc ?h "m\C-aserver help")
  (zenicb-send-string proc ?h "m\C-aserver ?"))
;;
;; Reach out and hush someone
;; /hush [-n] [-qop] user
;; /hush -s [-qop] site (usually a regexp, like "kamikaze@*")
;; -n is for nicknames, -q is quiet, -o is public messages only,
;; -p is for private messages only, -s is for site hushes
;; type the exact same command string to unhush.
;;
(defun zenicb-command-hush (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "hush\C-a" (cdr parsedcmd))))
(fset 'zenicb-command-shush 'zenicb-command-hush) ; is this legal?
;;
;; invite a victim, or show who is invited
;; /invite [victim]
;;
(defun zenicb-command-invite (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "invite\C-a" (cdr parsedcmd))))
;;
;; Set the message catalog.
;; /language languagename
;;
(defun zenicb-command-language (proc parsedcmd)
  (let ((lang (car (zenicb-parse-firstword (cdr parsedcmd)))))
    (zenicb-lang-set-current-language lang)))
;;
;; private message
;; /m victim message or /msg victim message
;;
(defun zenicb-command-m (proc parsedcmd)
  (let* ((tmp (zenicb-parse-firstword (cdr parsedcmd)))
	 (victim (car tmp))
	 (message (cdr tmp)))
    (zenicb-split message 220 'zenicb-send-private proc victim)))
(defun zenicb-send-private (message proc victim)
  (setq zenicb-msg-last-sent victim)
  (zenicb-send-string proc ?h (concat "m\C-a" victim " " message)))
;; compatability for irc refugees
(fset 'zenicb-command-msg 'zenicb-command-m)
;;
;; Read some weird messages (I have no idea what this is)
;;
;(defun zenicb-command-mess (proc parsecmd)
;  (zenicb-send-string proc ?h "m\C-aserver mess"))
;;
;; Read the message-of-the-day
;; /motd
;;
(defun zenicb-command-motd (proc parsedcmd)
  (zenicb-send-string proc ?h "motd\C-a"))
;;
;; Notify on a login/logout
;; /notify [-q] [-n nick] [-s site]
;; -q = quiet
;;
(defun zenicb-command-notify (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "notify\C-a" (cdr parsedcmd))))
;;
;; Show a particular news message
;; /news item
;;
(defun zenicb-command-news (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "news\C-a" (cdr parsedcmd))))
;;
;; Change nicknames
;; /nick newnick
;;
(defun zenicb-command-nick (proc parsedcmd)
  ;; actually, we should parse return strings from the server to determine
  ;; what our nick is, and not keep state here, as this loses if someone
  ;; chooses an invalid nick, but there seems to be little standardization
  ;; between icb servers on the messages returned.
  (setq zenicb-nick (cdr parsedcmd))
  (zenicb-send-string proc ?h (concat "name\C-a" (cdr parsedcmd))))
;;
;; Make the server stop beeping you
;; /nobeep [off|on|verbose]
;; verbose - other people can't beep you, and you are informed of their attempt
;;
(defun zenicb-command-nobeep (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "nobeep\C-a" (cdr parsedcmd))))
;; Set autoregister
;; /nosecure
;;
(defun zenicb-command-nosecure (proc parsedcmd)
  (zenicb-send-string proc ?h "m\C-aserver nosecure"))
;;
;; Pass moderator status
;; /pass newmoderator
;;
(defun zenicb-command-pass (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "pass\C-a" (cdr parsedcmd))))
;;
;; send messages to another user w/o typing /m user
;; /query user  (or just /query to stop)
;;
(defun zenicb-command-query (proc parsedcmd)
  (cond ((equal (cdr parsedcmd) "")
	 (setq zenicb-current-victim nil)
         (zenicb-message proc 'queryoff))
	(t
	 (setq zenicb-current-victim (cdr parsedcmd))
         (zenicb-message proc 'queryon (cdr parsedcmd))))
  (force-mode-line-update))
;;
;; ping a luser.
;; /ping victim
;;
(defun zenicb-command-ping (proc parsedcmd)
  (zenicb-send-string proc (concat "ping\C-a" (cdr parsedcmd))))
;;
;; quit icb
;; /quit
;;
(defun zenicb-command-quit (proc parsedcmd)
  (if (or (and zenicb-verify-quit
	       (zenicb-confirm-quit))
	  (not zenicb-verify-quit))
      (progn
        (zenicb-message proc 'quit)
	(delete-process proc))))
(defun zenicb-confirm-quit ()
  (interactive)
  (yes-or-no-p "Do you really want to quit? "))
;;
;; Read stored message
;; /read
;;
(defun zenicb-command-read (proc parsedcmd)
  (zenicb-send-string proc ?h "m\C-aserver read"))
;;
;; Register a nick
;; /reg[ister]
;;
(defun zenicb-command-register (proc parsedcmd)
  (let ((passwd (read-passwd "Enter password: ")))
    (zenicb-send-string proc ?h (concat "m\C-aserver p " passwd))))
(fset 'zenicb-command-reg 'zenicb-command-register)
;;
;; "secure" nick registration
;;
(defun zenicb-command-secure (proc parsedcmd)
  (zenicb-send-string proc ?h "m\C-aserver secure"))
;;
;; Display time to shutdown
;; /shuttime
;;
(defun zenicb-command-shuttime (proc parsedcmd)
  (zenicb-send-string proc ?h "M\C-aserver shuttime"))
;;
;; Change/view channel status
;; /status [mode]
;;
(defun zenicb-command-status (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "status\C-a" (cdr parsedcmd))))
(fset 'zenicb-command-mode 'zenicb-command-status)
;;
;; Select who can talk in a controlled group.
;; /talk [-qard] nickname
;; -q = quiet, -a = add nickname to talk list,
;; -r = add nickname only if registered, -d = remove nicname from list
;;
(defun zenicb-command-talk (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "talk\C-a" (cdr parsedcmd))))
;;
;; set the topic for a group
;; /topic new-topic-string
;;
(defun zenicb-command-topic (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "topic\C-a" (cdr parsedcmd))))
;;
;; check version of icb
;; /version
;;
(defun zenicb-command-version (proc parsedcmd)
  (zenicb-send-string proc ?h "v\C-a"))
;;
;; Show username and hostname info for a user
;; /whereis user
;;
(defun zenicb-command-whereis (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "whereis\C-a" (cdr parsedcmd))))
;;
;; See who's on ICB
;; /who [groupname] (groupname of . means current group)
;;
(defun zenicb-command-who (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "w\C-a" (cdr parsedcmd))))
(fset 'zenicb-command-w 'zenicb-command-who)
;;
;; Get info about a user
;; /whois victim
;;
(defun zenicb-command-whois (proc parsedcmd)
  (zenicb-send-string
   proc ?h (concat "m\C-aserver whois " (cdr parsedcmd))))
;;
;; Save a message for a victim
;; /write victim message
;;
(defun zenicb-command-write (proc parsedcmd)
  (zenicb-send-string
   proc ?h (concat "m\C-aserver write " (cdr parsedcmd))))


;;; Catalog-related code.
(defvar zenicb-lang-catalogs (make-vector 13 0)
  "Obarray used to store message for all languages, indexed by symbol.")

;; 211 buckets should be more than enough for message catalogs (remember to
;; use a prime number to get good hashing characteristics).
;; This is not the total number of messages you can store, but just the number
;; of "buckets" in which they can go.  Even if the catalog eventually
;; contains more entries than this, it isn't really necessary to increase
;; the size of this table.
(defconst zenicb-lang-obarray-size 211
  "*The default hash table size for newly created message catalogs")

(defvar zenicb-lang-current-language 'english
  "Current language in use in zenicb.")

;; This works on existing catalogs, but will overwrite any entry already in
;; the catalog.
(defun zenicb-lang-define-catalog (lang alist)
  (let* ((catalog-name (if (stringp lang)
                           lang
                         (symbol-name lang)))
         (catalog-sym (intern catalog-name zenicb-lang-catalogs))
         catalog)

    (or (boundp catalog-sym)
        (set catalog-sym (make-vector zenicb-lang-obarray-size 0)))
    (setq catalog (symbol-value catalog-sym))

    (while alist
      (set (intern (symbol-name (car (car alist))) catalog) (cdr (car alist)))
      (setq alist (cdr alist)))))

;; This creates a new catalog if none exists for the language specified.
;; It is more efficient to use zenicb-lang-define-catalog if defining many
;; entries at once.
(defun zenicb-lang-store-catalog-entry (sym str lang)
  (or lang (setq lang zenicb-lang-current-language))
  (let* ((catalog-name (if (stringp lang)
                           lang
                         (symbol-name lang)))
         (sym-name (if (stringp sym)
                       sym
                     (symbol-name sym)))
         (catalog-sym (intern catalog-name zenicb-lang-catalogs)))
    (or (boundp catalog-sym)
        (set catalog-sym (make-vector zenicb-lang-obarray-size 0)))
    (set (intern sym-name (symbol-value catalog-sym)) str)))

;; This returns nil for any undefined entry type, or if there is no
;; catalog for the language specified.
(defun zenicb-lang-retrieve-catalog-entry (sym &optional lang)
  (if (not lang) (setq lang zenicb-lang-current-language))
  (or (zenicb-lang-retrieve-catalog-entry-1 sym lang)
      ;; For now, if a message entry isn't defined for the
      ;; current language, default to english.  There are
      ;; many new message types and the other catalogs
      ;; aren't completely up to date.
      (and (not (string-equal lang 'english))
 	   (zenicb-lang-retrieve-catalog-entry-1 sym 'english))))

(defun zenicb-lang-retrieve-catalog-entry-1 (sym lang)
  (or lang (setq lang zenicb-lang-current-language))
  (let* ((catalog-name (if (stringp lang)
                           lang
                         (symbol-name lang)))
         (catalog-sym (intern-soft catalog-name zenicb-lang-catalogs))
         catalog
         (sym-name (if (stringp sym)
                       sym
                     (symbol-name sym)))
         msg-sym)
    (cond ((or (null catalog-sym)
               (not (boundp catalog-sym)))
           nil)
          (t
           (setq catalog (symbol-value catalog-sym))
           (setq msg-sym (intern-soft sym-name catalog))
           (and msg-sym
                (boundp msg-sym)
                (symbol-value msg-sym))))))

;; If called interactively and language is undefined, signal an error.
(defun zenicb-lang-set-current-language (lang)
  (interactive (list (completing-read "Switch to language: "
                                      zenicb-lang-catalogs 'boundp t)))
  (let* ((name (if (stringp lang)
                   lang
                 (symbol-name lang)))
         (catalog (intern-soft name zenicb-lang-catalogs)))

    (cond ((and catalog (boundp catalog))
           ;; Set the current language to a symbol interned in the global
           ;; obarray.  This makes it more convenient to compare against
           ;; other symbols with eq.
           (setq zenicb-lang-current-language (intern name))
           (zenicb-message (current-buffer) 'newcatalog name))
          (t
           (zenicb-message (current-buffer) 'nocatalog name)
           nil))))


;; English is the default catalog.  Other catalogs are available in
;; separate files.
(defun zenicb-lang-define-english-catalog ()
  (zenicb-lang-define-catalog 'english
    '((beep     . "[beep] %s wants to annoy you.") ; nickname sent a beep
      (connect-abort . "[info] Aborted attempt to connect to an icb server.")
      (connect-failed . "[error] Couldn't connect to %s port %d, reason: %s"); host, port, reason
      (connect-try . "[info] Connecting to %s port %d...") ; host, port
      (debug    . "[debug] %s")         ; displayed by debugging code
      (errormsg . "[error] %s")         ; an error reported by the server
      (goaway   . "[info] Server wants you to go away.")
      (loginok  . "[info] You are wasting time.")
      (nocatalog . "[error] No message catalog defined for %s") ; unknown catalog name
      (nocmd    . "[info] No such command: %s") ; unknown command
      (ping     . "[info] You were pinged by %s!")
      (pong     . "[info] Pong: %s")
      (private  . "*%s* %s")            ; nickname, message
      (protocol . "[info] Connected to server %s version %s\n[info] running ICB protocol version %s") ; nth 0, nth 2, nth 1 of parsedmsg
      (public   . "<%s> %s")            ; nickname, message
      (queryoff . "[info] Directing output to current channel.")
      (queryon  . "[info] Directing output to %s.") ; nickname
      (quit     . "[info] You are wasting time elsewhere.")
      (newcatalog . "[info] Current message catalog set to %s") ; message catalog name
      (sentinel . "\nZenICB ended at %s") ; process sentinel message
      (server   . "[server] %s")         ; unknown server message
      (signal   . "[signal in %s]")     ; signal in echo area
      (status   . "[info] %s: %s"))))   ; server status message


;;; misc code

;; Code to send a bug report.
(defun zenicb-bug ()
  "Send a bug report to the ZenICB maintainers."
  (interactive)
  (require 'sendmail)
  (switch-to-buffer "*ZenICB bug*")
  (erase-buffer)
  (insert (concat "To: " zenicb-bug-address "\n"
		  "Subject: Found a showstopper in ZenICB-" zenicb-version "\n"
		  mail-header-separator "\n\n"))
  (insert
   (concat
    "Describe the bug you encountered as good as possible.\n"
    "When you're ready hit C-cC-c to send away the bug report.\n\n"))
  (mail-mode))

;; for-each -- from Noah Friedman.
(defun for-each (fn &rest lists)
  "Like mapcar, but don't cons a list of return values.
This function also handles multiple list arguments.
The first arg, a function, is expected to take as many arguments as there
are subsequent list arguments to for-each, and each argument list is
assumed to be the same length."
  (cond ((consp (cdr lists))
         (let ((listrun (make-list (length lists) nil))
               listsl listrunl)
           (while (car lists)
             (setq listrunl listrun)
             (setq listsl lists)
             (while listsl
               (setcar listrunl (car (car listsl)))
               (setcar listsl (cdr (car listsl)))
               (setq listrunl (cdr listrunl))
               (setq listsl (cdr listsl)))
             (apply fn listrun))))
        (t
         ;; Speed/minimal-consing hack for when there is only one arglist.
         (setq lists (car lists))
         (while lists
           (funcall fn (car lists))
           (setq lists (cdr lists))))))

(defun zenicb-string-match-list (msg regexp-list)
  (let ((match-data (match-data))
        (found nil))
    (while (and (not found) regexp-list)
      (setq found (string-match (car regexp-list) msg))
      (setq regexp-list (cdr regexp-list)))
    (or found
        (store-match-data match-data))
    found))



(provide 'zenicb)

(zenicb-lang-define-english-catalog)

;;; End of zenicb.el
