;;; blitzmax-mode-indentation-test.el --- Tests for blitzmax-mode indentation.

;;; Commentary:

;; Tests for blitzmax-mode indentation.  Opens a fixture file (which is already
;; indented correctly) and indents the entire buffer.  Fails if the
;; post-indented file doesn't match the fixture.

;;; Code:

;; --------------------------------------------------
;; -- Type Indentation

(ert-deftest blitzmax-mode-indentation-test/test-empty-type ()
  (test-blitzmax-mode-indentation "empty_type.bmx"))

(ert-deftest blitzmax-mode-indentation-test/test-type-method ()
  (test-blitzmax-mode-indentation "type_methods.bmx"))

(ert-deftest blitzmax-mode-indentation-test/test-type-function ()
  (test-blitzmax-mode-indentation "type_functions.bmx"))

(ert-deftest blitzmax-mode-indentation-test/test-type-complete ()
  (test-blitzmax-mode-indentation "type_complete.bmx"))


;; --------------------------------------------------
;; -- If Then Indentation

(ert-deftest blitzmax-mode-indentation-test/test-if-then-endif ()
  (test-blitzmax-mode-indentation "if_then_endif.bmx"))

(ert-deftest blitzmax-mode-indentation-test/test-if-then-else ()
  (test-blitzmax-mode-indentation "if_then_else.bmx"))

(ert-deftest blitzmax-mode-indentation-test/test-if-oneliner ()
  (test-blitzmax-mode-indentation "if_then_oneliner.bmx"))


;; --------------------------------------------------
;; -- Select...Case Indentation

(ert-deftest blitzmax-mode-indentation-test/test-select-case ()
  (test-blitzmax-mode-indentation "select_case.bmx"))

(ert-deftest blitzmax-mode-indentation-test/test-select-default ()
  (test-blitzmax-mode-indentation "select_default.bmx"))


;; --------------------------------------------------
;; -- While / Wend

(ert-deftest blitzmax-mode-indentation-test/while-wend ()
  (test-blitzmax-mode-indentation "while_wend.bmx"))


;; --------------------------------------------------
;; -- Repeat / Until

(ert-deftest blitzmax-mode-indentation-test/repeat-until ()
  (test-blitzmax-mode-indentation "repeat_until.bmx"))

(ert-deftest blitzmax-mode-indentation-test/repeat-forever ()
  (test-blitzmax-mode-indentation "repeat_forever.bmx"))


;; --------------------------------------------------
;; -- Try/Catch/Finally

(ert-deftest blitzmax-mode-indentation-test/try_catch ()
  (test-blitzmax-mode-indentation "try_catch.bmx"))


;; --------------------------------------------------
;; -- Bugs

;; Abstract functions and methods increase indent but shouldn't.
(ert-deftest blitzmax-mode-indentation-test/issue-002-abstract-indentation ()
  (test-blitzmax-mode-indentation "issue_002.bmx"))

;; Extern does not increase indent.
(ert-deftest blitzmax-mode-indentation-test/issue-004-extern-indentation ()
  (test-blitzmax-mode-indentation "issue_004_a.bmx"))

;; Extern tries to indent functions
(ert-deftest blitzmax-mode-indentation-test/issue-004-extern-function-indentation ()
  (test-blitzmax-mode-indentation "issue_004_b.bmx"))

;; Returning a name with `Abstract` in it breaks indentation
(ert-deftest blitzmax-mode-indentation-test/issue-006-abstract-type-indentation ()
  (test-blitzmax-mode-indentation "issue_006.bmx"))

;; Issue #8 - underscore functions + methods don't indent
(ert-deftest blitzmax-mode-indentation-test/issue-008-underscore-indentation ()
  (test-blitzmax-mode-indentation "issue_008.bmx"))

;; Issue #9 - underscore type names don't indent
(ert-deftest blitzmax-mode-indentation-test/issue-009-underscore-types-indentation ()
  (test-blitzmax-mode-indentation "issue_009.bmx"))

;; Issue #12 - Nested single-line if statements break things
(ert-deftest blitzmax-mode-indentation-test/issue-012-nested-if-indentation ()
  (test-blitzmax-mode-indentation "issue_012.bmx"))

;; Issue #19 - Weird indentation error with if/endif
(ert-deftest blitzmax-mode-indentation-test/issue-013-weird-if-indentation ()
  (test-blitzmax-mode-indentation "issue_019.bmx"))

;;; blitzmax-mode-indentation-test.el ends here
