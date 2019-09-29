;;; org-onit.el --- Easy org-clock-in and org-clock-out -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Takaaki ISHIKAWA

;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Keywords: convenience
;; Version: 1.0.6
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; URL: https://github.com/takaxp/org-onit
;; Package-Requires: ((emacs "25.1"))
;; Twitter: @takaxp

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides mainly two capabilities:
;;  1. Toggle to switch `org-clock-in' and `org-clock-out'.
;;  2. Toggle to activate auto clocking for the current org heading.
;;
;; Install:
;;  - Get org-onit.el from GitHub.
;;
;; Setup:
;;  - After installing this package, you will be able to call interactively
;;    `org-onit-toggle-doing' and `org-onit-toggle-auto'.
;;  - Once these commands are called, minor mode `org-onit-mode' is
;;    automatically activated for the buffer.
;;
;; Keybindings:
;;  - No default keybindings are configured.
;;  - Assigning following keybindings are recommended:
;;    (global-set-key (kbd \"C-<f11>\") 'org-clock-goto)
;;    (define-key org-mode-map (kbd \"<f11>\") 'org-onit-toggle-doing)
;;    (define-key org-mode-map (kbd \"M-<f11>\") 'org-onit-toggle-auto)
;;    (define-key org-mode-map (kbd \"S-<f11>\") 'org-onit-goto-anchor)
;;

;;; Change Log:

;;; Code:

(require 'org-clock)
(when (require 'bookmark nil t)
  (bookmark-maybe-load-default-file))

(defgroup org-onit nil
  "Commands to toggle `org-clock-in' and `org-clock-out'."
  :group 'convenience)

(defcustom org-onit-todo-state (or
                                (when (functionp org-clock-in-switch-to-state)
                                  (funcall org-clock-in-switch-to-state))
                                org-clock-in-switch-to-state
                                "TODO")
  "The default todo state."
  :type 'string
  :group 'org-onit)

(defcustom org-onit-doing-tag "Doing"
  "Tag name to show the current task now clocking."
  :type 'string
  :group 'org-onit)

(defcustom org-onit-toggle-options '(:wakeup nil :nostate nil :unfold nil)
  "Combined options for `org-onit-toggle-doing' and `org-onit-toggle-auto'.

This variable will be buffer local.

Following two options can take {doing, auto, both, nil}:
:wakeup    If non-nil, start clocking even if the task is marked DONE.
:nostate   If non-nil, clock the task even if it doesn't have todo state.

Following option can take {t, nil}:
:unfold    If non-nil, try to clock-in when unfolding a subturee.

Note - :wakeup and :nonstate options are given priority over :unfold."
  :type 'plist
  :group 'org-onit)
;;;###autoload (put 'org-onit-toggle-options 'safe-local-variable 'stringp)

(defcustom org-onit-keep-no-state t
  "If non-nil, do not change TODO state even when :nostat is non-nil."
  :type 'boolean
  :group 'org-onit)

(defcustom org-onit-encure-clock-out-when-exit t
  "If non-nil, `org-clock-out' will be called when killing Emacs."
  :type 'boolean
  :group 'org-onit)

(defcustom org-onit-switch-task-hook nil
  "Hook runs after activating new clock-in when auto clocking."
  :type 'hook
  :group 'org-onit)

(defcustom org-onit-start-autoclock-hook nil
  "Hook runs after starting auto clock-in."
  :type 'hook
  :group 'org-onit)

(defcustom org-onit-stop-autoclock-hook nil
  "Hook runs after stopping auto clock-in."
  :type 'hook
  :group 'org-onit)

(defcustom org-onit-after-jump-hook nil
  "Hook runs after jumping to a bookmark."
  :type 'hook
  :group 'org-onit)

(defcustom org-onit-clocking-sign-alist
  '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "▇" "▇" "▆" "▅" "▄" "▃" "▂" "▁" "▁" "▁")
  "List of signs to show now clocking in a heading."
  :type 'list
  :group 'org-onit)

(defcustom org-onit-bookmark "org-onit-last-clock-in"
  "Bookmark for the heading last clock-in."
  :type 'string
  :group 'org-onit)

(defcustom org-onit-bookmark-anchor "org-onit-anchor"
  "Bookmark to store an anchor position."
  :type 'string
  :group 'org-onit)

(defcustom org-onit-wakeup-done nil
  "[deprecated] If non-nil, start clocking even if the task is marked done.
This flag is not utilized for `org-onit-toggle-auto'."
  :type 'boolean
  :group 'org-onit)

(defcustom org-onit-include-no-state-heading nil
  "[deprecated] If non-nil, clock the task even if it doesn't have todo state.
This flag is utilized for `org-onit-toggle-auto'."
  :type 'boolean
  :group 'org-onit)

(defcustom org-onit-use-unfold-as-doing nil
  "[deprecated] If non-nil, clock-in when a heading is changed to unfold and not clocking."
  :type 'boolean
  :group 'org-onit)

;; internal functions

(defun org-onit--rotate-list (list)
  "Rotate the provided LIST."
  (if (listp list)
      (append (cdr list)
              (list (car list)))
    list))

(defvar org-onit--auto-clocking nil)
(defvar org-onit--heading nil)
(defvar org-onit--state nil)
(defvar org-onit--org-bookmark-heading-p (require 'org-bookmark-heading nil t))
(defvar org-onit--clock-in-last-pos nil)
(defvar org-onit--anchor-last-pos nil)
(defvar org-onit--frame-title-format nil)
(defvar org-onit--lighter " Doing")

(defun org-onit--lighter ()
  "Lighter."
  org-onit--lighter)

(defun org-onit--switched-p ()
  "Return t if the current heading was changed."
  (if (org-before-first-heading-p)
      (progn
        (org-onit--clock-out)
        (setq org-onit--heading nil)
        (setq org-onit--state nil))
    (save-excursion
      (save-restriction
        (org-back-to-heading t)
        (let* ((element (cadr (org-element-at-point)))
               (heading (plist-get element :title))
               (todo (plist-get element :todo-keyword))
               (switched nil))
          (unless (equal org-onit--state todo)
            (when (or (not org-onit--state) ;; nil -> something
                      (not ;; todo1 -> done or nil, except todo1 -> todo2
                       (and (not (member org-onit--state org-done-keywords))
                            (not (member todo org-done-keywords)))))
              (setq switched t))
            (setq org-onit--state todo))
          (unless (equal org-onit--heading heading)
            (setq switched t)
            (setq org-onit--heading heading))
          switched)))))

(defun org-onit--auto-target-p ()
  "Return non-nil if the heading is valid task for clock-in."
  (cond
   ((org-entry-is-done-p)
    (memq (plist-get org-onit-toggle-options :wakeup) '(auto both)))
   ((not (org-get-todo-state))
    (memq (plist-get org-onit-toggle-options :nostate) '(auto both)))
   ((org-entry-is-todo-p) t)
   (t nil)))

(defun org-onit--tagged-p ()
  "Return t if the current heading tagged with `org-onit-doing-tag'."
  (member org-onit-doing-tag (org-get-tags (point) t)))

(defun org-onit--bookmark-set ()
  "Save the bookmark for the current heading."
  (save-excursion
    (save-restriction
      (org-back-to-heading t)
      (setq org-onit--clock-in-last-pos (point))
      (when (bookmark-get-bookmark org-onit-bookmark 'noerror)
        (bookmark-delete org-onit-bookmark))
      (bookmark-set org-onit-bookmark)
      (when (bookmark-get-bookmark org-onit-bookmark-anchor 'noerror)
        (setq org-onit--anchor-last-pos nil)
        (bookmark-delete org-onit-bookmark-anchor)))))

(defun org-onit--remove-tag ()
  "Remove `org-onit-doing-tag' tag from the current heading."
  (when (org-onit--tagged-p)
    (org-toggle-tag org-onit-doing-tag 'off)))

(defun org-onit--remove-tag-not-todo ()
  "Remove `org-onit-doing-tag' tag if the heading is done or no state."
  (when (or (org-entry-is-done-p)
            (not (org-get-todo-state)))
    (org-onit--clock-out)))

(defun org-onit--post-action (&optional switched)
  "A combined action of clock-out and clock-in.
If SWITCHED is non-nil, then do not check `org-onit--switched-p'."
  (when (and org-onit-mode
             (or switched
                 (org-onit--switched-p)))
    (org-onit--clock-out)
    (when (org-onit--auto-target-p)
      (org-onit--clock-in)
      (run-hooks 'org-onit-switch-task-hook))
    (org-cycle-hide-drawers 'children)
    (org-reveal)))

(defun org-onit--bookmark-jump (bookmark)
  "Jump to BOOKMARK."
  (bookmark-jump bookmark)
  (when (eq major-mode 'org-mode)
    (unless org-onit--org-bookmark-heading-p
      (org-back-to-heading t))
    (run-hooks 'org-onit-after-jump-hook)))

(defun org-onit--clock-goto (f &optional select)
  "Go to the current clocking task.
Even after restart of Emacs, try to restore the current task from a bookmark.
F is the original `org-clock-goto'.
SELECT is the optional argument of `org-clock-goto'."
  (let ((bm (bookmark-get-bookmark org-onit-bookmark 'noerror)))
    (if (eq (point) org-onit--clock-in-last-pos)
        (message "Already at the last clocked in.")
      (when (bookmark-get-bookmark org-onit-bookmark-anchor 'noerror)
        (bookmark-delete org-onit-bookmark-anchor))
      (ignore-errors (bookmark-set org-onit-bookmark-anchor))
      (if (bookmark-get-bookmark org-onit-bookmark-anchor 'noerror)
          (message "Anchor bookmark was recorded in a file.")
        (message "Anchor bookmark was not recorded for the buffer."))
      (when (and (eq major-mode 'org-mode)
                 (not (org-before-first-heading-p)))
        (org-back-to-heading t))
      (setq org-onit--anchor-last-pos (point)))
    (cond
     ((and org-onit--org-bookmark-heading-p ;; most reliable
           bm)
      (org-onit--bookmark-jump org-onit-bookmark)) ;; call org-bookmark-jump
     (org-clock-history
      (apply f select)
      (show-children))
     (bm
      (org-onit--bookmark-jump org-onit-bookmark)) ;; use normal bookmark
     (t (message "No clock is found to be shown")))))

(defun org-onit-clock-out-when-kill-emacs ()
  "Save buffers and stop clocking when killing Emacs."
  (bookmark-delete org-onit-bookmark-anchor)
  (when (and org-onit-encure-clock-out-when-exit
             (org-clocking-p)
             (memq org-clock-persist '(history nil)))
    (org-onit--clock-out)
    (save-some-buffers t)))

(defun org-onit--clock-in ()
  "Clock-in and adding `org-onit-doing-tag' tag."
  (when (or (org-entry-is-done-p)
            (not org-onit-keep-no-state))
    (org-todo org-onit-todo-state))
  (org-clock-in)
  (org-toggle-tag org-onit-doing-tag 'on))

(defun org-onit--clock-out ()
  "Clock-out and remove `org-onit-doing-tag' tag."
  (if (org-clocking-p)
      (org-clock-out)
    (org-onit--remove-tag)))

(defun org-onit--backup-title-format ()
  "Backup `the-title-format'."
  (setq org-onit--frame-title-format frame-title-format))

(defun org-onit--restore-title-format ()
  "Restore the original title format."
  (setq frame-title-format org-onit--frame-title-format))

(defun org-onit--setup ()
  "Setup."
  ;; For buffer-local
  (setq org-onit-toggle-options org-onit-toggle-options)

  ;; This section will be removed based on the availability of `org-onit-wakeup-done' and `org-onit-include-no-state-heading'
  (when (and (not (plist-get org-onit-toggle-options :wakeup))
             (not (plist-get org-onit-toggle-options :nostate))
             (not (plist-get org-onit-toggle-options :unfold)))
    (plist-put org-onit-toggle-options
               :wakeup (when org-onit-wakeup-done 'doing))
    (plist-put org-onit-toggle-options
               :nostate (when org-onit-include-no-state-heading 'auto))
    (plist-put org-onit-toggle-options
               :unfold org-onit-use-unfold-as-doing))

  (org-onit--backup-title-format)
  (advice-add 'org-clock-goto :around #'org-onit--clock-goto)
  (add-hook 'org-cycle-hook #'org-onit-clock-in-when-unfold)
  (add-hook 'org-after-todo-state-change-hook #'org-onit--remove-tag-not-todo)
  (add-hook 'kill-emacs-hook #'org-onit-clock-out-when-kill-emacs)
  (add-hook 'org-clock-in-hook #'org-onit--bookmark-set)
  (add-hook 'org-clock-out-hook #'org-onit--remove-tag)
  (add-hook 'org-clock-out-hook #'org-onit--restore-title-format))

(defun org-onit--abort ()
  "Cleanup."
  (when (and (org-clocking-p)
             (memq org-clock-persist '(history nil)))
    (org-onit--clock-out))
  (org-onit--restore-title-format)
  (bookmark-delete org-onit-bookmark-anchor)
  (setq org-onit--clock-in-last-pos nil)
  (setq org-onit--anchor-last-pos nil)
  (advice-remove 'org-clock-goto #'org-onit--clock-goto)
  (remove-hook 'org-cycle-hook #'org-onit-clock-in-when-unfold)
  (remove-hook 'org-after-todo-state-change-hook
               #'org-onit--remove-tag-not-todo)
  (remove-hook 'kill-emacs-hook #'org-onit-clock-out-when-kill-emacs)
  (remove-hook 'org-clock-in-hook #'org-onit--bookmark-set)
  (remove-hook 'org-clock-out-hook #'org-onit--remove-tag)
  (remove-hook 'org-clock-out-hook #'org-onit--restore-title-format))

;; public functions

(defun org-onit-goto-anchor ()
  "Go to the anchor position if recorded."
  (interactive)
  (if (bookmark-get-bookmark org-onit-bookmark-anchor 'noerror)
      (progn
        (if (eq (point) org-onit--anchor-last-pos)
            (message "Already at the anchor.")
          (message "Jumped to the anchor."))
        (org-onit--bookmark-jump org-onit-bookmark-anchor))
    (message "No anchor is recorded.")))

(defun org-onit-get-sign ()
  "Return the first item of `org-onit-clocking-sign-alist'."
  (car (setq org-onit-clocking-sign-alist
             (org-onit--rotate-list
              org-onit-clocking-sign-alist))))

;;;###autoload
(defun org-onit-toggle-auto ()
  "Toggle auto clocking."
  (interactive)
  (when (eq major-mode 'org-mode)
    (unless org-onit-mode
      (org-onit-mode 1))
    (setq org-onit--auto-clocking (not org-onit--auto-clocking))
    (cond (org-onit--auto-clocking
           (setq org-onit--lighter " Doing:auto")
           (add-hook 'post-command-hook #'org-onit--post-action)
           (unless (org-before-first-heading-p)
             (org-onit--post-action t))
           (run-hooks 'org-onit-start-autoclock-hook))
          (t
           (setq org-onit--lighter " Doing")
           (remove-hook 'post-command-hook #'org-onit--post-action)
           (org-onit--clock-out)
           (run-hooks 'org-onit-stop-autoclock-hook)))
    (redraw-modeline)))

;;;###autoload
(defun org-onit-toggle-doing ()
  "Toggle `org-onit-doing-tag' tag.
This command also switches `org-clock-in' and `org-clock-out'."
  (interactive)
  (when (eq major-mode 'org-mode)
    (unless org-onit-mode
      (org-onit-mode 1))
    (save-excursion
      (save-restriction
        (org-back-to-heading t)
        (cond
         ((org-onit--tagged-p)
          (org-onit--clock-out))
         ((org-entry-is-done-p)
          (if (memq (plist-get org-onit-toggle-options :wakeup) '(doing both))
              (org-onit--clock-in)
            (message "Prevent `org-clock-in'. And not switching to TODO.")))
         ((not (org-get-todo-state))
          (if (memq (plist-get org-onit-toggle-options :nostate) '(doing both))
              (org-onit--clock-in)
            (message "Prevent `org-clock-in'. Heading has no todo state.")))
         ((org-entry-is-todo-p)
          (org-onit--clock-in)))))
    (org-cycle-hide-drawers 'children)
    (org-reveal)))

;;;###autoload
(defun org-onit-clock-in-when-unfold (state)
  "Clock-in when a heading is switched to unfold and not clocking.
STATE should be one of the symbols listed in the docstring of
`org-cycle-hook'."
  (when (and (not (org-clocking-p))
             (memq state '(children subtree))
             (plist-get org-onit-toggle-options :unfold)
             (or (and (org-entry-is-done-p)
                      (plist-get org-onit-toggle-options :wakeup))
                 (and (not (org-get-todo-state))
                      (plist-get org-onit-toggle-options :nostate))
                 (org-entry-is-todo-p)))
    (unless org-onit-mode
      (org-onit-mode 1))
    (org-onit--clock-in)
    (org-cycle-hide-drawers 'children)
    (org-reveal)))

;;;###autoload
(define-minor-mode org-onit-mode
  "
This minor mode expands `org-clock-in', `org-clock-out' and `org-clock-goto'
to support \"Doing\" functionality. When you toggle a heading, a clock for
the heading is automatically activated and the heading is tagged with
`org-doing-tag'. Toggling the same heading, the clock is stopped, and the tag
is removed. Basically, a single heading tagged with `org-doing-tag' will
appear in org buffers.

The tagged heading can be easily revisited by calling the extended
`org-clock-goto' command whether you are editing another heading in any
org buffers.

An automated `org-clock-in' capability is also provided by this package.
A heading that you are currently visiting in an org buffer will be
automatically clocked with executing `org-clock-in'. After you switch to
other headings, the active clock will be automatically updated without any
additional actions.

Recommended settings for `org-clock':
  (setq org-clock-out-remove-zero-time-clocks t) ;; you should apply this.
  (setq org-clock-clocked-in-display 'frame-title) ;; or 'both
  (setq org-clock-frame-title-format
    '((:eval (format \"%s\" org-mode-line-string))))

Recommended keybindings:
  (global-set-key (kbd \"C-<f11>\") 'org-clock-goto)
  (define-key org-mode-map (kbd \"<f11>\") 'org-onit-toggle-doing)
  (define-key org-mode-map (kbd \"M-<f11>\") 'org-onit-toggle-auto)
  (define-key org-mode-map (kbd \"S-<f11>\") 'org-onit-goto-anchor)
"
  :init-value nil
  :lighter (:eval (org-onit--lighter))
  :require 'org-clock
  :group 'org-onit
  (if org-onit-mode
      (org-onit--setup)
    (org-onit--abort)))

(provide 'org-onit)

;;; org-onit.el ends here
