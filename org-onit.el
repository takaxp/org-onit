;;; org-onit.el --- Recording your task all the time -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Takaaki ISHIKAWA

;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Keywords: convenience
;; Version: 0.9.2
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
;;  2. Toggle to activate auto clocking of the current org heading.
;;
;; Install:
;;  - Get org-onit.el from Github.
;;
;; Setup:
;;  - After installing this package, you will be able to call interactively
;;    `org-onit-toggle-doing' and `org-onit-toggle-auto'.
;;  - Once these commands are called, `org-onit-mode' is automatically
;;    activated for the buffer.
;;
;; Keybindings:
;;  - No default keybindings are configured.
;;  - Assigning following keybindings are recommended:
;;    (define-key org-mode-map (kbd \"<f11>\") 'org-onit-toggle-doing)
;;    (define-key org-mode-map (kbd \"M-<f11>\") 'org-onit-toggle-auto)
;;    (global-set-key (kbd \"C-<f11>\") 'org-clock-goto)
;;

;;; Change Log:

;;; Code:

(require 'org-clock)
(when (require 'bookmark nil t)
  (bookmark-maybe-load-default-file))

(defgroup org-onit nil
  "Commands to toggle `org-clock-in' and `org-clock-out'."
  :group 'convenience)

(defcustom org-onit-todo-state "TODO"
  "The default todo state."
  :type 'string
  :group 'org-onit)

(defcustom org-onit-tag "Doing"
  "Tag name to show the current task now clocking."
  :type 'string
  :group 'org-onit)

(defcustom org-onit-bookmark "org-onit-last-clock-in"
  "Bookmark for last clock in heading."
  :type 'string
  :group 'org-onit)

(defcustom org-onit-bookmark-anchor "org-onit-anchor"
  "Bookmark to store anchor position."
  :type 'string
  :group 'org-onit)

(defcustom org-onit-wakeup-done nil
  "If non-nil, start clocking even if the task is marked done in `org-onit-toggle-doing'.  This flag is not utilized for `org-onit-toggle-auto'."
  :type 'boolean
  :group 'org-onit)

(defcustom org-onit-include-no-status-heading nil
  "If non-nil, clock the task even if it doesn't have todo status.
This flag is utilized for `org-onit-toggle-auto'."
  :type 'boolean
  :group 'org-onit)

(defcustom org-onit-encure-clock-out-when-exit t
  "If non-nil, `org-clock-out' will be called when killing Emacs.")

(defcustom org-onit-switch-task-hook nil
  "Hook runs after activating new clock in auto clocking."
  :type 'hook
  :group 'org-onit)

(defcustom org-onit-start-autoclock-hook nil
  "Hook runs after starting auto clock in."
  :type 'hook
  :group 'org-onit)

(defcustom org-onit-stop-autoclock-hook nil
  "Hook runs after stopping auto clock in."
  :type 'hook
  :group 'org-onit)

(defcustom org-onit-clocking-sign-alist
  '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "▇" "▇" "▆" "▅" "▄" "▃" "▂" "▁" "▁" "▁")
  "List of signs to show a clock is active."
  :type 'list
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
(defvar org-onit--status nil)
(defvar org-onit--org-bookmark-heading-p (require 'org-bookmark-heading nil t))

(defun org-onit--switched-p ()
  "Return t if the current heading was changed."
  (if (org-before-first-heading-p)
      (progn
        (when (org-clocking-p)
          (org-clock-out))
        (setq org-onit--heading nil)
        (setq org-onit--status nil))
    (save-excursion
      (save-restriction
        (org-back-to-heading t)
        (let* ((element (cadr (org-element-at-point)))
               (heading (plist-get element :title))
               (todo (plist-get element :todo-keyword))
               (switched nil))
          (unless (equal org-onit--status todo)
            (when (or (not org-onit--status) ;; nil -> something
                      (not ;; todo1 -> done or nil, except todo1 -> todo2
                       (and (not (member org-onit--status org-done-keywords))
                            (not (member todo org-done-keywords)))))
              (setq switched t))
            (setq org-onit--status todo))
          (unless (equal org-onit--heading heading)
            (setq switched t)
            (setq org-onit--heading heading))
          switched)))))

(defun org-onit--target-p ()
  "Return t if the heading is valid task for clock in."
  (or (org-entry-is-todo-p)
      (and (not (org-entry-is-done-p))
           org-onit-include-no-status-heading)))

(defun org-onit--tagged-p ()
  "Return t if the current heading tagged with `org-onit-tag'."
  (member org-onit-tag (org-get-tags (point) t)))

(defun org-onit--bookmark-set ()
  "Save the bookmark of the current heading."
  (save-excursion
    (save-restriction
      (org-back-to-heading t)
      (bookmark-set org-onit-bookmark))))

(defun org-onit--remove-tag ()
  "Remove `org-onit-tag' tag from the current heading."
  (when (org-onit--tagged-p)
    (org-toggle-tag org-onit-tag 'off)))

(defun org-onit--remove-tag-not-todo ()
  "Remove `org-onit-tag' tag if the heading is done or no status."
  (when (or (org-entry-is-done-p)
            (not (org-entry-is-todo-p)))
    (org-toggle-tag org-onit-tag 'off)))

(defun org-onit--post-action (&optional switched)
  "Clock in.
If SWITCHED is non-nil, then do not check `org-onit--switched-p'."
  (when (and org-onit-mode
             (or switched
                 (org-onit--switched-p)))
    (when (org-clocking-p)
      (org-clock-out))
    (when (org-onit--target-p)
      (org-toggle-tag org-onit-tag 'on)
      (org-clock-in)
      (run-hooks 'org-onit-switch-task-hook))
    (org-cycle-hide-drawers 'children)
    (org-reveal)))

(defun org-onit--bookmark-jump (bookmark)
  "Jump to BOOKMARK."
  (bookmark-jump bookmark)
  (when (eq major-mode 'org-mode)
    (unless org-onit--org-bookmark-heading-p
      (org-back-to-heading t))
    (org-show-entry)
    (show-children)))

(defun org-onit--clock-goto (f &optional select)
"Go to the current clocking task.  Even after restart of Emacs, try to restore the current task from `bookmark'.  F is the original `org-clock-goto'.  SELECT is the optional argument of `org-clock-goto'."
(if (bookmark-get-bookmark org-onit-bookmark-anchor 'noerror)
    (progn
      (org-onit--bookmark-jump org-onit-bookmark-anchor)
      (bookmark-delete org-onit-bookmark-anchor))
  (let ((bm (bookmark-get-bookmark org-onit-bookmark 'noerror)))
    (bookmark-set org-onit-bookmark-anchor)
    (cond
     ((and org-onit--org-bookmark-heading-p ;; most reliable
           bm)
      (org-onit--bookmark-jump org-onit-bookmark)) ;; call org-bookmark-jump
     (org-clock-history
      (apply f select)
      (show-children))
     (bm
      (org-onit--bookmark-jump org-onit-bookmark)) ;; use normal bookmark
     (t (message "No clock is found to be shown"))))))

(defun org-onit-clock-out-when-kill-emacs ()
  "Save buffers and stop clocking when killing Emacs."
  (bookmark-delete org-onit-bookmark-anchor)
  (when (and org-onit-encure-clock-out-when-exit
             (org-clocking-p)
             (not org-clock-persist))
    (org-clock-out)
    (save-some-buffers t)))

(defun org-onit--setup ()
  "Setup."
  (advice-add 'org-clock-goto :around #'org-onit--clock-goto)
  (add-hook 'org-after-todo-state-change-hook #'org-onit--remove-tag-not-todo)
  (add-hook 'kill-emacs-hook #'org-onit-clock-out-when-kill-emacs)
  (add-hook 'org-clock-in-hook #'org-onit--bookmark-set)
  (add-hook 'org-clock-out-hook #'org-onit--remove-tag))

(defun org-onit--abort ()
  "Cleanup."
  (when (and (org-clocking-p)
             (not org-clock-persist))
    (org-clock-out))
  (bookmark-delete org-onit-bookmark-anchor)
  (advice-remove 'org-clock-goto #'org-onit--clock-goto)
  (remove-hook 'org-after-todo-state-change-hook
               #'org-onit--remove-tag-not-todo)
  (remove-hook 'kill-emacs-hook #'org-onit-clock-out-when-kill-emacs)
  (remove-hook 'org-clock-in-hook #'org-onit--bookmark-set)
  (remove-hook 'org-clock-out-hook #'org-onit--remove-tag))

(defvar org-onit--lighter " Doing")
(defun org-onit--lighter ()
  "Lighter."
  org-onit--lighter)

;; public functions

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
           (when (org-clocking-p)
             (org-clock-out))
           (run-hooks 'org-onit-stop-autoclock-hook)))
    (redraw-modeline)))

;;;###autoload
(defun org-onit-toggle-doing ()
  "Toggle `org-onit-tag' tag.
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
          (when (org-clocking-p)
            (org-clock-out))
          (org-toggle-tag org-onit-tag 'off))
         (t
          (if (org-entry-is-done-p)
              (if (not org-onit-wakeup-done)
                  (message "Prevent `org-clock-in' and switching to TODO.")
                (org-todo org-onit-todo-state)
                (org-clock-in)
                (org-toggle-tag org-onit-tag 'on))
            (org-clock-in)
            (org-toggle-tag org-onit-tag 'on))))))
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
`org-clock-goto' command whether you edit another heading in any org buffers.

An automated `org-clock-in' capability is also provided by this package.
A heading that you currently visit in an org buffer will be automatically
clocked with executing `org-clock-in'. After you switch to other headings,
the active clock will be automatically updated without any additional actions.

Recommended settings for `org-clock':
  (setq org-clock-out-remove-zero-time-clocks t) ;; you should apply this.
  (setq org-clock-clocked-in-display 'frame-title) ;; or 'both
  (setq org-clock-frame-title-format
    '((:eval (format \"%s\" org-mode-line-string))))

Recommended keybindings:
  (global-set-key (kbd \"C-<f11>\") 'org-clock-goto)
  (define-key org-mode-map (kbd \"<f11>\") 'org-onit-toggle-doing)
  (define-key org-mode-map (kbd \"M-<f11>\") 'org-onit-toggle-auto)
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
