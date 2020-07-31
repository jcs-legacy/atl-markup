;;; atl-markup.el ---  Automatically truncate lines for markup languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-07-22 17:11:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automatically truncate lines for markup languages.
;; Keyword: automatic truncate visual lines
;; Version: 0.1.2
;; Package-Requires: ((emacs "24.3") (auto-rename-tag "0.2.9"))
;; URL: https://github.com/jcs-elpa/atl-markup

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Automatically truncate lines for markup languages.
;;

;;; Code:

(require 'auto-rename-tag)

(defgroup atl-markup nil
  "Automatically truncate lines for markup languages."
  :prefix "atl-markup-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/atl-markup"))

(defcustom atl-markup-ignore-regex "[ \t\r\n]"
  "Regular expression string that will ignore auto truncate lines' action."
  :type 'string
  :group 'atl-markup)

;;; Util

(defun atl-markup--get-current-char-string ()
  "Get the current character as the 'string'."
  (if (char-before) (string (char-before)) ""))

(defun atl-markup--current-char-string-match-p (c)
  "Check the current character string match to C."
  (string-match-p c (atl-markup--get-current-char-string)))

(defun atl-markup--inside-comment-block-p ()
  "Check if current cursor point inside the comment block."
  (nth 4 (syntax-ppss)))

(defun atl-markup--enable-truncate-lines ()
  "Enable truncate lines."
  (unless truncate-lines (toggle-truncate-lines)))

(defun atl-markup--disable-truncate-lines ()
  "Disable truncate lines."
  (when truncate-lines (toggle-truncate-lines)))

;;; Core

(defun atl-markup--web-truncate-lines-by-face ()
  "Enable/Disable the truncate lines mode depends on the face cursor currently on."
  (when (and (not (= (point) (point-min))) (not (= (point) (point-max)))
             (not (atl-markup--current-char-string-match-p
                   atl-markup-ignore-regex))
             (not (atl-markup--inside-comment-block-p))
             (not (eolp)))
    (let ((message-log-max nil) (inhibit-message t))
      (if (auto-rename-tag--inside-tag-p)
          (atl-markup--enable-truncate-lines)
        (atl-markup--disable-truncate-lines)))))

(defun atl-markup--post-command-hook ()
  "Post command hook to do auto truncate lines in current buffer."
  (atl-markup--web-truncate-lines-by-face))

(defun atl-markup--enable ()
  "Enable 'atl-markup-mode'."
  (add-hook 'post-command-hook 'atl-markup--post-command-hook nil t))

(defun atl-markup--disable ()
  "Disable 'atl-markup-mode'."
  (remove-hook 'post-command-hook 'atl-markup--post-command-hook t))

;;;###autoload
(define-minor-mode atl-markup-mode
  "Minor mode 'atl-markup-mode'."
  :lighter " ATL"
  :group atl-markup
  (if atl-markup-mode (atl-markup--enable) (atl-markup--disable)))

(provide 'atl-markup)
;;; atl-markup.el ends here
