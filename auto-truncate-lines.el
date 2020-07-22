;;; auto-truncate-lines.el ---  Automatically truncate lines for markup languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-07-22 17:11:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automatically truncate lines for markup languages.
;; Keyword: automatic truncate visual lines
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (auto-rename-tag "0.2.9"))
;; URL: https://github.com/jcs-elpa/auto-truncate-lines

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

(defgroup auto-truncate-lines nil
  "Automatically truncate lines for markup languages."
  :prefix "auto-truncate-lines-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/auto-truncate-lines"))

;;; Util

(defun auto-truncate-lines--get-current-char-string ()
  "Get the current character as the 'string'."
  (if (char-before) (string (char-before)) ""))

(defun auto-truncate-lines--current-char-string-match-p (c)
  "Check the current character string match to C."
  (if (= (point) (point-max))
      ;; No character at the beginning of the buffer, just return `nil'.
      nil
    (string-match-p c (auto-truncate-lines--get-current-char-string))))

(defun auto-truncate-lines--inside-comment-block-p ()
  "Check if current cursor point inside the comment block."
  (nth 4 (syntax-ppss)))

(defun auto-truncate-lines--enable-truncate-lines ()
  "Enable truncate lines."
  (unless truncate-lines (toggle-truncate-lines)))

(defun auto-truncate-lines--disable-truncate-lines ()
  "Disable truncate lines."
  (when truncate-lines (toggle-truncate-lines)))

;;; Core

(defun auto-truncate-lines--web-truncate-lines-by-face ()
  "Enable/Disable the truncate lines mode depends on the face cursor currently on."
  (when (and (not (auto-truncate-lines--current-char-string-match-p "[ \t\r\n]"))
             (not (auto-truncate-lines--inside-comment-block-p))
             (not (eolp)))
    (let ((message-log-max nil) (inhibit-message t))
      (if (auto-rename-tag--inside-tag-p)
          (auto-truncate-lines--enable-truncate-lines)
        (auto-truncate-lines--disable-truncate-lines)))))

(defun auto-truncate-lines--post-command-hook ()
  "Post command hook to do auto truncate lines in current buffer."
  (auto-truncate-lines--web-truncate-lines-by-face))

(defun auto-truncate-lines--enable ()
  "Enable 'auto-truncate-lines-mode.'"
  (add-hook 'post-command-hook 'auto-truncate-lines--post-command-hook nil t))

(defun auto-truncate-lines--disable ()
  "Disable 'auto-truncate-lines-mode.'"
  (remove-hook 'post-command-hook 'auto-truncate-lines--post-command-hook t))

;;;###autoload
(define-minor-mode auto-truncate-lines-mode
  "Minor mode 'auto-truncate-lines-mode'."
  :lighter " ATL"
  :group auto-truncate-lines
  (if auto-truncate-lines-mode (auto-truncate-lines--enable) (auto-truncate-lines--disable)))

(provide 'auto-truncate-lines)
;;; auto-truncate-lines.el ends here
