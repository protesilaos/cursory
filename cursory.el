;;; cursory.el --- Manage cursor styles using presets -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Cursory Development <~protesilaos/cursory@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/cursory
;; Mailing-List: https://lists.sr.ht/~protesilaos/cursory
;; Version: 1.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, cursor

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Cursory provides a thin wrapper around built-in variables that affect
;; the style of the Emacs cursor on graphical terminals.  The intent is
;; to allow the user to define preset configurations such as "block with
;; slow blinking" or "bar with fast blinking" and set them on demand.
;; The use-case for such presets is to adapt to evolving interface
;; requirements and concomitant levels of expected comfort, such as in
;; the difference between writing and reading.
;;
;; The user option `cursory-presets' holds the presets.  The command
;; `cursory-set-preset' is applies one among them.  The command supports
;; minibuffer completion when there are multiple presets, else sets the
;; single preset outright.
;;
;; Presets consist of an arbitrary symbol broadly described the style set
;; followed by a list of properties that govern the cursor type in the
;; active and inactive windows, as well as cursor blinking variables.
;; They look like this:
;;
;;     (bar
;;      :cursor-type (bar . 2)
;;      :cursor-in-non-selected-windows hollow
;;      :blink-cursor-mode 1
;;      :blink-cursor-blinks 10
;;      :blink-cursor-interval 0.5
;;      :blink-cursor-delay 0.2)
;;
;; The car of the list is an arbitrary, user-defined symbol that identifies
;; (and can describe) the set.  Each of the properties corresponds to
;; built-in variables: `cursor-type', `cursor-in-non-selected-windows',
;; `blink-cursor-blinks', `blink-cursor-interval', `blink-cursor-delay'.
;; The value each property accepts is the same as the variable it
;; references.
;;
;; A property of `:blink-cursor-mode' is also available.  It is a numeric
;; value of either `1' or `-1' and is given to the function
;; `blink-cursor-mode': `1' is to enable, `-1' is to disable the mode.
;;
;; Presets can inherit from each other.  Using the special `:inherit'
;; property, like this:
;;
;;     (bar
;;      :cursor-type (bar . 2)
;;      :cursor-in-non-selected-windows hollow
;;      :blink-cursor-mode 1
;;      :blink-cursor-blinks 10
;;      :blink-cursor-interval 0.5
;;      :blink-cursor-delay 0.2)
;;
;;     (bar-no-other-window
;;      :inherit bar
;;      :cursor-in-non-selected-windows nil)
;;
;; In the above example, the `bar-no-other-window' is the same as `bar'
;; except for the value of `:cursor-in-non-selected-windows'.
;;
;; The value given to the `:inherit' property corresponds to the name of
;; another named preset (unquoted).  This tells the relevant Cursory
;; functions to get the properties of that given preset and blend them
;; with those of the current one.  The properties of the current preset
;; take precedence over those of the inherited one, thus overriding them.
;;
;; A preset whose car is `t' is treated as the default option.  This
;; makes it possible to specify multiple presets without duplicating
;; their properties.  Presets beside `t' act as overrides of the
;; defaults and, as such, need only consist of the properties that
;; change from the default.  In the case of an `:inherit', properties
;; are first taken from the inherited preset and then the default one.
;; See the original value of this variable for how that is done:
;;
;;     (defcustom cursory-presets
;;       '((box
;;          :blink-cursor-interval 0.8)
;;         (box-no-blink
;;          :blink-cursor-mode -1)
;;         (bar
;;          :cursor-type (bar . 2)
;;          :blink-cursor-interval 0.5)
;;         (bar-no-other-window
;;          :inherit bar
;;          :cursor-in-non-selected-windows nil)
;;         (underscore
;;          :cursor-type (hbar . 3)
;;          :blink-cursor-blinks 50)
;;         (underscore-thin-other-window
;;          :inherit underscore
;;          :cursor-in-non-selected-windows (hbar . 1))
;;         (t ; the default values
;;          :cursor-type box
;;          :cursor-in-non-selected-windows hollow
;;          :blink-cursor-mode 1
;;          :blink-cursor-blinks 10
;;          :blink-cursor-interval 0.2
;;          :blink-cursor-delay 0.2))
;;       ;; Omitting the doc string for demo purposes
;;       )
;;
;; When called from Lisp, the `cursory-set-preset' command requires a
;; PRESET argument, such as:
;;
;;     (cursory-set-preset 'bar)
;;
;; The default behaviour of `cursory-set-preset' is to change cursors
;; globally.  The user can, however, limit the effect to the current
;; buffer.  With interactive use, this is done by invoking the command with
;; a universal prefix argument (`C-u' by default).  When called from Lisp,
;; the LOCAL argument must be non-nil, thus:
;;
;;     (cursory-set-preset 'bar :local)
;;
;; The function `cursory-store-latest-preset' is used to save the last
;; selected style in the `cursory-latest-state-file'.  The value can then
;; be restored with the `cursory-restore-latest-preset' function.

;;; Code:

(eval-when-compile (require 'subr-x))

(defgroup cursory ()
  "Manage cursor styles using presets."
  :group 'cursor
  :link '(info-link "(cursory) Top"))

(defcustom cursory-presets
  '((box
     :blink-cursor-interval 0.8)
    (box-no-blink
     :blink-cursor-mode -1)
    (bar
     :cursor-type (bar . 2)
     :blink-cursor-interval 0.5)
    (bar-no-other-window
     :inherit bar
     :cursor-in-non-selected-windows nil)
    (underscore
     :cursor-type (hbar . 3)
     :blink-cursor-blinks 50)
    (underscore-thin-other-window
     :inherit underscore
     :cursor-in-non-selected-windows (hbar . 1))
    (t ; the default values
     :cursor-type box
     :cursor-in-non-selected-windows hollow
     :blink-cursor-mode 1
     :blink-cursor-blinks 10
     :blink-cursor-interval 0.2
     :blink-cursor-delay 0.2))
  "Alist of preset configurations for `blink-cursor-mode'.

The car of each cons cell is an arbitrary, user-specified key
that broadly describes the set (e.g. slow-blinking-box or
fast-blinking-bar).

A preset whose car is t is treated as the default option.  This
makes it possible to specify multiple presets without duplicating
their properties.  The other presets beside t act as overrides of
the defaults and, as such, need only consist of the properties
that change from the default.  See the original value of this
variable for how that is done.

The `cdr' is a plist which specifies the cursor type and blink
properties.  In particular, it accepts the following properties:

    :cursor-type
    :cursor-in-non-selected-windows
    :blink-cursor-blinks
    :blink-cursor-interval
    :blink-cursor-delay

They correspond to built-in variables: `cursor-type',
`cursor-in-non-selected-windows', `blink-cursor-blinks',
`blink-cursor-interval', `blink-cursor-delay'.  The value each of
them accepts is the same as the variable it references.

A property of `:blink-cursor-mode' is also available.  It is a
numeric value of either 1 or -1 and is given to the function
`blink-cursor-mode' (1 is to enable, -1 is to disable the mode).

Finally, the plist takes the special `:inherit' property.  Its
value is contains the name of another named preset (unquoted).
This tells the relevant Cursory functions to get the properties
of that given preset and blend them with those of the current
one.  The properties of the current preset take precedence over
those of the inherited one, thus overriding them.  In practice,
this is a way to have something like an underscore style with a
hallow cursor for the other window and the same with a thin
underscore for the other window (see the default value of this
user option for concrete examples).  Remember that all named
presets fall back to the preset whose name is t.  The `:inherit'
is not a substitute for that generic fallback but rather an extra
method of specifying font configuration presets."
  :group 'cursory
  :package-version '(cursory . "1.0.0")
  :type `(alist
          :value-type
          (plist :options
                 (((const :tag "Cursor type"
                          :cursor-type)
                   ,(get 'cursor-type 'custom-type))
                  ((const :tag "Cursor in non-selected windows"
                          :cursor-in-non-selected-windows)
                   ,(get 'cursor-in-non-selected-windows 'custom-type))
                  ((const :tag "Number of blinks"
                          :blink-cursor-blinks)
                   ,(get 'blink-cursor-blinks 'custom-type))
                  ((const :tag "Blink interval"
                          :blink-cursor-interval)
                   ,(get 'blink-cursor-interval 'custom-type))
                  ((const :tag "Blink delay"
                          :blink-cursor-delay)
                   ,(get 'blink-cursor-delay 'custom-type))
                  ((const :tag "Blink Cursor Mode"
                          :blink-cursor-mode)
                   (choice :value 1
                           (const :tag "Enable" 1)
                           (const :tag "Disable" -1)))
                  ((const :tag "Inherit another preset" :inherit) symbol)))
          :key-type symbol))

(defcustom cursory-latest-state-file
  (locate-user-emacs-file "cursory-latest-state.eld")
  "File to save the value of `cursory-set-preset'.
Saving is done by the `cursory-store-latest-preset' function."
  :type 'file
  :package-version '(cursory . "0.1.0")
  :group 'cursory)

(defcustom cursory-set-preset-hook nil
  "Normal hook that runs after `cursory-set-preset'."
  :type 'hook
  :package-version '(cursory . "1.1.0")
  :group 'cursory)

(defvar cursory--style-hist '()
  "Minibuffer history of `cursory--set-cursor-prompt'.")

(defun cursory--preset-p (preset)
  "Return non-nil if PRESET is one of the named `cursory-presets'."
  (let ((presets (delq t (mapcar #'car cursory-presets))))
    (memq preset presets)))

(defun cursory--get-inherit-name (preset)
  "Get the `:inherit' value of PRESET."
  (when-let* ((inherit (plist-get (alist-get preset cursory-presets) :inherit))
              (cursory--preset-p inherit))
    inherit))

(defun cursory--get-preset-properties (preset)
  "Return list of properties for PRESET in `cursory-presets'."
  (let ((presets cursory-presets))
    (append (alist-get preset presets)
            (when-let ((inherit (cursory--get-inherit-name preset)))
              (alist-get inherit presets))
            (alist-get t presets))))

(defun cursory--presets-no-fallback ()
  "Return list of `cursory-presets', minus the fallback value."
  (seq-remove
   (lambda (symbol)
     (eq (car symbol) t))
   cursory-presets))

(defun cursory--set-cursor-prompt ()
  "Promp for `cursory-presets' (used by `cursory-set-preset')."
  (let* ((def (nth 1 cursory--style-hist))
         (prompt (if def
                     (format "Apply cursor configurations from PRESET [%s]: " def)
                   "Apply cursor configurations from PRESET: ")))
    (completing-read
     prompt
     (cursory--presets-no-fallback)
     nil t nil 'cursory--style-hist def)))

(defun cursory--get-preset-as-symbol (preset)
  "Return PRESET as a symbol."
  (if (stringp preset)
      (intern preset)
    preset))

(defun cursory--delete-local-variables ()
  "Delete all cursor-related local variables."
  (dolist (var '( cursor-type cursor-in-non-selected-windows
                  blink-cursor-blinks blink-cursor-interval
                  blink-cursor-delay))
    (kill-local-variable var)))

(defun cursory--set-preset-with-scope (preset &optional local)
  "Set PRESET of `cursory-presets' to the global scope.
With optional non-nil LOCAL, set STYLES scoped locally to the
current buffer."
  (if-let ((styles (cursory--get-preset-properties preset)))
      ;; We do not include this in the `if-let' because we also accept
      ;; nil values for :cursor-type, :cursor-in-non-selected-windows.
      (let ((type (plist-get styles :cursor-type))
            (type-no-select (plist-get styles :cursor-in-non-selected-windows))
            (blinks (plist-get styles :blink-cursor-blinks))
            (interval (plist-get styles :blink-cursor-interval))
            (delay (plist-get styles :blink-cursor-delay)))
        (if local
            (setq-local cursor-type type
                        cursor-in-non-selected-windows type-no-select
                        blink-cursor-blinks blinks
                        blink-cursor-interval interval
                        blink-cursor-delay delay)
          (cursory--delete-local-variables)
          (setq-default cursor-type type
                        cursor-in-non-selected-windows type-no-select
                        blink-cursor-blinks blinks
                        blink-cursor-interval interval
                        blink-cursor-delay delay)
          ;; We only want to save global values in `cursory-store-latest-preset'.
          (add-to-history 'cursory--style-hist (format "%s" preset)))
        (blink-cursor-mode (plist-get styles :blink-cursor-mode))
        (run-hooks 'cursory-set-preset-hook))
    (user-error "Cannot determine styles of preset `%s'" preset)))

;;;###autoload
(defun cursory-set-preset (style &optional local)
  "Set cursor preset associated with STYLE.

STYLE is a symbol that represents the car of a list in
`cursory-presets'.

With optional LOCAL as a prefix argument, set the
`cursory-presets' only for the current buffer.  This does not
cover the `blink-cursor-mode', which is always global.

Call `cursory-set-preset-hook' as a final step."
  (interactive
   (list
    (cursory--set-cursor-prompt)
    current-prefix-arg))
  (if-let ((preset (cursory--get-preset-as-symbol style)))
      (cursory--set-preset-with-scope preset local)
    (user-error "Cannot determine preset `%s'" preset)))

;;;###autoload
(defun cursory-store-latest-preset ()
  "Write latest cursor state to `cursory-latest-state-file'.
Can be assigned to `kill-emacs-hook'."
  (when-let ((hist cursory--style-hist))
    (with-temp-file cursory-latest-state-file
      (insert ";; Auto-generated file; don't edit -*- mode: "
              (if (<= 28 emacs-major-version)
                  "lisp-data"
                "emacs-lisp")
              " -*-\n")
      (pp (intern (car hist)) (current-buffer)))))

(defvar cursory-recovered-preset nil
  "Recovered value of latest store cursor preset.")

;;;###autoload
(defun cursory-restore-latest-preset ()
  "Restore latest cursor style."
  (when-let* ((file cursory-latest-state-file)
              ((file-exists-p file)))
    (setq cursory-recovered-preset
          (unless (zerop
                   (or (file-attribute-size (file-attributes file))
                       0))
            (with-temp-buffer
              (insert-file-contents file)
              (read (current-buffer)))))))

(provide 'cursory)
;;; cursory.el ends here
