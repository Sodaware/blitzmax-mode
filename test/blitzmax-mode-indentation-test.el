;;; blitzmax-mode-indentation-test.el --- Tests for blitzmax-mode indentation.

;;; Commentary:

;; Tests for blitzmax-mode indentation.  Opens a fixture file (which is already
;; indented correctly) and indents the entire buffer.  Fails if the
;; post-indented file doesn't match the fixture.

;;; Code:

;; --------------------------------------------------
;; -- Type Indentation

(ert-deftest blitzmax-mode-indentation-test/test-empty-type ()
  (with-blitzmax-mode-test
   ("empty_type.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "empty_type.bmx")))))

(ert-deftest blitzmax-mode-indentation-test/test-type-method ()
  (with-blitzmax-mode-test
   ("type_methods.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "type_methods.bmx")))))

(ert-deftest blitzmax-mode-indentation-test/test-type-function ()
  (with-blitzmax-mode-test
   ("type_functions.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "type_functions.bmx")))))

(ert-deftest blitzmax-mode-indentation-test/test-type-complete ()
  (with-blitzmax-mode-test
   ("type_complete.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "type_complete.bmx")))))


;; --------------------------------------------------
;; -- If Then Indentation

(ert-deftest blitzmax-mode-indentation-test/test-if-then-endif ()
  (with-blitzmax-mode-test
   ("if_then_endif.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "if_then_endif.bmx")))))

(ert-deftest blitzmax-mode-indentation-test/test-if-then-else ()
  (with-blitzmax-mode-test
   ("if_then_else.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "if_then_else.bmx")))))

(ert-deftest blitzmax-mode-indentation-test/test-if-oneliner ()
  (with-blitzmax-mode-test
   ("if_then_oneliner.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "if_then_oneliner.bmx")))))


;; --------------------------------------------------
;; -- Select...Case Indentation

(ert-deftest blitzmax-mode-indentation-test/test-select-case ()
  (with-blitzmax-mode-test
   ("select_case.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "select_case.bmx")))))

(ert-deftest blitzmax-mode-indentation-test/test-select-default ()
  (with-blitzmax-mode-test
   ("select_default.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "select_default.bmx")))))


;; --------------------------------------------------
;; -- While / Wend

(ert-deftest blitzmax-mode-indentation-test/while-wend ()
  (with-blitzmax-mode-test
   ("while_wend.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "while_wend.bmx")))))


;; --------------------------------------------------
;; -- Repeat / Until

(ert-deftest blitzmax-mode-indentation-test/repeat-until ()
  (with-blitzmax-mode-test
   ("repeat_until.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "repeat_until.bmx")))))


;; --------------------------------------------------
;; -- Bugs

;; Abstract functions and methods increase indent but shouldn't.
(ert-deftest blitzmax-mode-indentation-test/issue-002-abstract-indentation ()
  (with-blitzmax-mode-test
   ("issue_002.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "issue_002.bmx")))))

;; Extern does not increase indent.
(ert-deftest blitzmax-mode-indentation-test/issue-004-extern-indentation ()
  (with-blitzmax-mode-test
   ("issue_004_a.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "issue_004_a.bmx")))))

;; Extern tries to indent functions
(ert-deftest blitzmax-mode-indentation-test/issue-004-extern-function-indentation ()
  (with-blitzmax-mode-test
   ("issue_004_b.bmx" :indent t)
   (should (string= (cleaned-buffer-string) (fixture "issue_004_b.bmx")))))


;;; blitzmax-mode-indentation-test.el ends here
