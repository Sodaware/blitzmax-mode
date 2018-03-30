;;; blitzmax-mode-capitalization-test.el --- Tests for capitalization -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for blitzmax-mode automatic capitalization.

;;; Code:

;; --------------------------------------------------
;; -- Keyword capitalization

(ert-deftest blitzmax-mode-capitalization-test/capitalizes-keywords ()
  (with-temp-buffer
    (blitzmax-mode)
    (should (string= "SuperStrict" (abbrev-expansion "superstrict")))
    (should (string= "True" (abbrev-expansion "true")))
    (should (string= "String" (abbrev-expansion "string")))))


;; --------------------------------------------------
;; -- Internal Tests

(ert-deftest blitzmax-mode-capitalization-test/creates-abbrev-table-if-empty ()
  (let ((blitzmax-mode-abbrev-table nil))
    (should (blitzmax-mode--create-abbrev-table))))

(ert-deftest blitzmax-mode-capitalization-test/does-not-create-abbrev-table-if-exists ()
  (let ((blitzmax-mode-abbrev-table (list "a" "b" "c")))
    (should-not (blitzmax-mode--create-abbrev-table))))

(ert-deftest blitzmax-mode-capitalization-test/creates-lookup-for-all-keywords ()
  (let ((blitzmax-mode-all-keywords '("KeyWord"))
        (blitzmax-mode-type-keywords '("Type"))
        (blitzmax-mode-constant-keywords '("Constant"))
        (lookup))
    (setq lookup (blitzmax-mode--abbrev-keyword-lookup))
    (should (eq 3 (length lookup)))
    (should (member '("keyword" "KeyWord") lookup))
    (should (member '("type" "Type") lookup))
    (should (member '("constant" "Constant") lookup))))


;;; blitzmax-mode-capitalization-test.el ends here
