(require 'cl)
(require 'el-mock)

(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'blitzmax-mode)

;; Use tabs for indenting by default.
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

(defvar blitzmax-mode-test-directory
  (expand-file-name ""
                    (if load-file-name
                        (file-name-directory load-file-name)
                        default-directory))
  "Test file directory.")

(defvar blitzmax-mode-fixture-directory
  (expand-file-name "fixtures"
                    (if load-file-name
                        (file-name-directory load-file-name)
                        default-directory))
  "Test fixture directory.")


;; --------------------------------------------------
;; -- Test helper functions

(defun test-blitzmax-mode-indentation (file)
  "Test that FILE is indented correctly."
  (with-blitzmax-mode-test
   (file :indent t)
   (compare-file-lines file (cleaned-buffer-string))))

(defun compare-file-lines (filename actual-buffer)
  "Compare FILENAME contents against ACTUAL-BUFFER on a line-by-line basis."
  (let* ((expected-buffer (fixture file))
         (actual-lines    (split-string actual-buffer "\n"))
         (expected-lines  (split-string expected-buffer "\n"))
         (line-number     0))
    (while (< line-number (- (length actual-lines) 1))
      (unless (string= (elt actual-lines line-number)
                       (elt expected-lines line-number))
        (ert-fail
         (format "Indentation of %s failed at line %s. Expected indentation level %s but got %s"
                 file
                 line-number
                 (indentation-level (elt expected-lines line-number))
                 (indentation-level (elt actual-lines line-number)))))
      (setq line-number (+ 1 line-number)))))

(defun indentation-level (line)
  "Get indentation level of LINE."
  (let ((char "")
        (pos  0)
        (indent-level 0)
        (finished))
    (while (not finished)
      (setq char (substring line pos (+ 1 pos)))
      (cond ((string= char "\t")
             (setq indent-level (+ 4 indent-level)))
            ((string= char " ")
             (setq indent-level (+ 1 indent-level)))
            (t
             (setq finished t)))
      (setq pos (+ 1 pos))
      (when (= pos (length line))
        (setq finished t)))
    (/ indent-level 4)))

;; Based on code from php-mode (https://github.com/ejmr/php-mode)
(cl-defmacro with-blitzmax-mode-test ((filename &key indent custom) &rest body)
  "Set up environment for testing `blitzmax-mode'.

Execute BODY in a temporary buffer containing the contents of
FILENAME from the fixtures directory and with `blitzmax-mode'
enabled.

If the `:custom' keyword is set, customized variables are not reset to
their default state prior to starting the test. Use this if the test should
run with specific customizations set."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents (expand-file-name ,filename blitzmax-mode-fixture-directory))
     (blitzmax-mode)

     ,(if (fboundp 'font-lock-ensure)
          '(font-lock-ensure)
          '(with-no-warnings (font-lock-fontify-buffer)))

     ,(if indent
          '(let ((inhibit-message t))
             (indent-region (point-min) (point-max))))

     (goto-char (point-min))
     (let ((case-fold-search nil))
       ,@body)))

(defun fixture (file)
  "Load FILE from the fixtures directory and return as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file blitzmax-mode-fixture-directory))
    (buffer-string)))

(defun cleaned-buffer-string ()
  "Get \"buffer-string\" without any fontification data."
  (let ((buffer (buffer-string)))
    (set-text-properties 0 (length buffer) nil buffer)
    buffer))
