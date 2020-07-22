;;; auto-truncate-lines.el ---  Automatically truncate lines for markup languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-07-22 17:11:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automatically truncate lines for markup languages.
;; Keyword: automatic truncate visual lines
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
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

(defgroup auto-truncate-lines nil
  "Automatically truncate lines for markup languages."
  :prefix "auto-truncate-lines-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/auto-truncate-lines"))


(defun auto-truncate-lines--enable ()
  "Enable 'auto-truncate-lines-mode.'")

(defun auto-truncate-lines--disable ()
  "Disable 'auto-truncate-lines-mode.'")

;;;###autoload
(define-minor-mode auto-truncate-lines-mode
  "Minor mode 'auto-truncate-lines-mode'."
  :lighter " ATL"
  :group auto-truncate-lines
  (if auto-truncate-lines-mode (auto-truncate-lines--enable) (auto-truncate-lines--disable)))

(provide 'auto-truncate-lines)
;;; auto-truncate-lines.el ends here
