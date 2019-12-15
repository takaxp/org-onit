;;; org-onit-test.el --- Test definitions for org-onit  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Takaaki ISHIKAWA

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/takaxp/org-onit

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

;; Test definitions for `org-onit'.


;;; Code:

(require 'buttercup)
(require 'org-onit)

(defconst org-onit-test-dir (file-name-directory
                            (cond
                             (load-in-progress load-file-name)
                             ((and (boundp 'byte-compile-current-file)
                                   byte-compile-current-file)
                              byte-compile-current-file)
                             (:else (buffer-file-name)))))

(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))

;; (provide 'org-onit-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; org-onit-test.el ends here
