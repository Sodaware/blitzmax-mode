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
  "Get buffer-string without any fontification data."
  (let ((buffer (buffer-string)))
    (set-text-properties 0 (length buffer) nil buffer)
    buffer))
