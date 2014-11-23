;;; cal-china-plus.el --- extensions to cal-china

;; Copyright (C) 2008-2014  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: calendar, convenience, local

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In the diary the cycle and year of a Chinese date is combined using
;; this formula: (+ (* cycle 100) year).
;;
;; These two functions convert to and back from this representation:
;; `calendar-chinese-from-absolute-for-diary' and
;; `calendar-chinese-to-absolute-for-diary'.
;;
;; The features that were implemented in this package have been merged
;; upstream and will appear in emacs 24.5; see
;; http://debbugs.gnu.org/17393. Thus the package is now providing the
;; upstream features for emacs < 24.5.

;;; Usage

;; (require 'cal-china-plus)
;; (add-hook 'diary-nongregorian-listing-hook #'diary-chinese-list-entries)
;; (add-hook 'diary-nongregorian-marking-hook #'diary-chinese-mark-entries)

;; Optionally you may restart Emacs to have Chinese dates properly
;; fontified in diary mode.

;; With the setup above, the following keys are available in Calendar
;; window:

;;  KEY      COMMAND
;;  ======   ======================================
;; `i C a'   diary-chinese-insert-anniversary-entry
;; `i C d'   diary-chinese-insert-entry
;; `i C m'   diary-chinese-insert-monthly-entry
;; `i C y'   diary-chinese-insert-yearly-entry
;;  ======   ======================================

;; Please send me any comments that you may have. Thank you.

;;; Code:

(require 'cal-china)
(eval-when-compile (require 'diary-lib))

(defcustom diary-chinese-entry-symbol "C"
  "Symbol indicating a diary entry according to the Chinese calendar."
  :type 'string
  :group 'diary)

(define-key calendar-mode-map "iCa" 'diary-chinese-insert-anniversary-entry)
(define-key calendar-mode-map "iCd" 'diary-chinese-insert-entry)
(define-key calendar-mode-map "iCm" 'diary-chinese-insert-monthly-entry)
(define-key calendar-mode-map "iCy" 'diary-chinese-insert-yearly-entry)

(autoload 'calendar-mark-1         "diary-lib")
(autoload 'diary-mark-entries-1    "diary-lib")
(autoload 'diary-list-entries-1    "diary-lib")
(autoload 'diary-insert-entry-1    "diary-lib")
(autoload 'diary-date-display-form "diary-lib")
(autoload 'diary-make-date         "diary-lib")
(autoload 'diary-ordinal-suffix    "diary-lib")
(defvar diary-sexp-entry-symbol)
(defvar date)
(defvar entry)                    ;used by `diary-chinese-anniversary'

;; Don't set this to ["1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"]
(defvar calendar-chinese-month-name-array
  ["正月" "二月" "三月" "四月" "五月" "六月"
   "七月" "八月" "九月" "十月" "冬月" "臘月"])

;;; NOTE: In the diary the cycle and year of a Chinese date is
;;; combined using this formula: (+ (* cycle 100) year).
;;;
;;; These two functions convert to and back from this representation.
(defun calendar-chinese-from-absolute-for-diary (date)
  (pcase-let ((`(,c ,y ,m ,d) (calendar-chinese-from-absolute date)))
    ;; Note: For leap months M is a float.
    (list (floor m) d (+ (* c 100) y))))

(defun calendar-chinese-to-absolute-for-diary (date &optional prefer-leap)
  (pcase-let* ((`(,m ,d ,y) date)
               (cycle (floor y 100))
               (year (mod y 100))
               (months (calendar-chinese-months cycle year))
               (lm (+ (floor m) 0.5)))
    (calendar-chinese-to-absolute
     (if (and prefer-leap (memql lm months))
         (list cycle year lm d)
       (list cycle year m d)))))

(defun calendar-chinese-mark-date-pattern (month day year &optional color)
  (calendar-mark-1 month day year
                   #'calendar-chinese-from-absolute-for-diary
                   #'calendar-chinese-to-absolute-for-diary
                   color)
  (unless (zerop month)
    (calendar-mark-1 month day year
                     #'calendar-chinese-from-absolute-for-diary
                     (lambda (date) (calendar-chinese-to-absolute-for-diary date t))
                     color)))

;;;###autoload
(defun diary-chinese-mark-entries ()
  "Mark days in the calendar window that have Chinese date diary entries.
Marks each entry in `diary-file' (or included files) visible in the calendar
window.  See `diary-chinese-list-entries' for more information.

This function is provided for use with `diary-nongregorian-marking-hook'."
  (diary-mark-entries-1 #'calendar-chinese-mark-date-pattern
                        calendar-chinese-month-name-array
                        diary-chinese-entry-symbol
                        #'calendar-chinese-from-absolute-for-diary))

;;;###autoload
(defun diary-chinese-list-entries ()
  "Add any Chinese date entries from the diary file to `diary-entries-list'.
Chinese date diary entries must be prefixed by `diary-chinese-entry-symbol'
\(normally a `C').  The same `diary-date-forms' govern the style
of the Chinese calendar entries.  If a Chinese date diary entry begins with
`diary-nonmarking-symbol', the entry will appear in the diary listing,
but will not be marked in the calendar.

This function is provided for use with `diary-nongregorian-listing-hook'."
  (diary-list-entries-1 calendar-chinese-month-name-array
                        diary-chinese-entry-symbol
                        #'calendar-chinese-from-absolute-for-diary))

;;;###autoload
(defun diary-chinese-anniversary (month day &optional year mark)
  "Like `diary-anniversary' (which see) but accepts Chinese date."
  (pcase-let* ((ddate (diary-make-date month day year))
               (`(,dc ,dy ,dm ,dd)      ;diary chinese date
                (if year
                    (calendar-chinese-from-absolute
                     (calendar-chinese-to-absolute-for-diary ddate))
                  (list nil nil (calendar-extract-month ddate)
                        (calendar-extract-day ddate))))
               (`(,cc ,cy ,cm ,cd)      ;current chinese date
                (calendar-chinese-from-absolute
                 (calendar-absolute-from-gregorian date)))
               (diff (if (and dc dy)
                         (+ (* 60 (- cc dc)) (- cy dy))
                       100)))
    (and (> diff 0)
         (or (= dm cm) (= (+ 0.5 dm) cm))
         (= dd cd)
         (cons mark (format entry diff (diary-ordinal-suffix diff))))))

;;;###autoload
(defun diary-chinese-insert-anniversary-entry (&optional arg)
  "Insert an anniversary diary entry for the Chinese date at point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form (diary-date-display-form)))
    (diary-make-entry
     (format "%s(diary-chinese-anniversary %s)"
             diary-sexp-entry-symbol
             (calendar-date-string
              (calendar-chinese-from-absolute-for-diary
               (calendar-absolute-from-gregorian (calendar-cursor-to-date t)))))
     arg)))

;;;###autoload
(defun diary-chinese-insert-entry (&optional arg)
  "Insert a diary entry for the Chinese date at point."
  (interactive "P")
  (diary-insert-entry-1 nil arg calendar-chinese-month-name-array
                        diary-chinese-entry-symbol
                        #'calendar-chinese-from-absolute-for-diary))

;;;###autoload
(defun diary-chinese-insert-monthly-entry (&optional arg)
  "Insert a monthly diary entry for the Chinese date at point."
  (interactive "P")
  (diary-insert-entry-1 'monthly arg calendar-chinese-month-name-array
                        diary-chinese-entry-symbol
                        #'calendar-chinese-from-absolute-for-diary))

;;;###autoload
(defun diary-chinese-insert-yearly-entry (&optional arg)
  "Insert a yearly diary entry for the Chinese date at point."
  (interactive "P")
  (diary-insert-entry-1 'yearly arg calendar-chinese-month-name-array
                        diary-chinese-entry-symbol
                        #'calendar-chinese-from-absolute-for-diary))

(declare-function diary-font-lock-date-forms "diary-lib")

;;; font lock support for Chinese dates; see also
;;; `diary-font-lock-keywords'.
;;;###autoload
(defun diary-chinese-font-lock ()
  (font-lock-add-keywords nil
                          (append
                           (diary-font-lock-keywords-1 diary-chinese-mark-entries
                                                       diary-chinese-list-entries
                                                       cal-china-plus
                                                       calendar-chinese-month-name-array
                                                       diary-chinese-entry-symbol)
                           `((,(format "^%s?\\(%s\\)" (regexp-quote diary-nonmarking-symbol)
                                       (regexp-quote diary-chinese-entry-symbol))
                              1 font-lock-reference-face nil t)))))

;;;###autoload
(add-hook 'diary-mode-hook 'diary-chinese-font-lock)

(provide 'cal-china-plus)
;;; cal-china-plus.el ends here
