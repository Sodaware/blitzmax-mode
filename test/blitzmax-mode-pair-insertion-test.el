;;; blitzmax-mode-pair-insertion-test.el --- Tests for automatic insertion -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for blitzmax-mode automatic pair insertion.

;;; Code:

;; --------------------------------------------------
;; -- General Behaviour

(ert-deftest blitzmax-mode-pair-insertion-test/does-not-complete-when-at-start-of-line ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "Extern")
   (beginning-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\nExtern" (buffer-string)))))


;; --------------------------------------------------
;; -- Extern/End Extern insertion

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-extern-for-new-extern ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "Extern")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "Extern\n\t\nEnd Extern" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-with-closed-extern-afterwards ()
  (with-blitzmax-mode-text-test
   ("\nExtern\n\t\nEnd Extern")
   (insert "Extern")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "Extern\n\t\nEnd Extern\nExtern\n\t\nEnd Extern" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/does-not-insert-end-with-closed-extern ()
  (with-blitzmax-mode-text-test
   ("Extern\n\t\nEnd Extern")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "Extern\n\t\n\t\nEnd Extern" (buffer-string)))))


;; --------------------------------------------------
;; -- Type/End Type insertion

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-type-for-new-type ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "Type Test")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "Type Test\n\t\nEnd Type" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-with-closed-type-afterwards ()
  (with-blitzmax-mode-text-test
   ("\nType First\n\t\nEnd Type")
   (insert "Type Test")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "Type Test\n\t\nEnd Type\nType First\n\t\nEnd Type" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/does-not-insert-end-with-closed-type ()
  (with-blitzmax-mode-text-test
   ("Type Test\n\t\nEnd Type")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "Type Test\n\t\n\t\nEnd Type" (buffer-string)))))

;; --------------------------------------------------
;; -- Function/End Function insertion

;; TODO: These are weird because writing Function on the first line of a file indents it incorrectly.

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-function-for-new-function ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "Function Test")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tFunction Test\n\t\t\nEnd Function" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-with-closed-function-afterwards ()
  (with-blitzmax-mode-text-test
   ("\nFunction First\n\t\nEnd Function")
   (insert "Function Test")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tFunction Test\n\t\t\nEnd Function\nFunction First\n\t\nEnd Function" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/does-not-insert-end-with-closed-function ()
  (with-blitzmax-mode-text-test
   ("Function Test\n\t\nEnd Function")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tFunction Test\n\t\t\n\t\nEnd Function" (buffer-string)))))

;; --------------------------------------------------
;; -- Method/End Method insertion

;; TODO: These are weird because writing Method on the first line of a file indents it incorrectly.

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-method-for-new-method ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "Method Test")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tMethod Test\n\t\t\nEnd Method" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-with-closed-method-afterwards ()
  (with-blitzmax-mode-text-test
   ("\nMethod First\n\t\nEnd Method")
   (insert "Method Test")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tMethod Test\n\t\t\nEnd Method\nMethod First\n\t\nEnd Method" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/does-not-insert-end-with-closed-method ()
  (with-blitzmax-mode-text-test
   ("Method Test\n\t\nEnd Method")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tMethod Test\n\t\t\n\t\nEnd Method" (buffer-string)))))


;; --------------------------------------------------
;; -- Abstract Function/Method tests

(ert-deftest blitzmax-mode-pair-insertion-test/does-not-insert-end-with-abstract-function ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "Function Test() Abstract")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "Function Test() Abstract\n" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/does-not-insert-end-with-abstract-method ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "Method Test() Abstract")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "Method Test() Abstract\n" (buffer-string)))))


;; --------------------------------------------------
;; -- For/Next

;; TODO: These are weird because writing For on the first line of a file indents it incorrectly.

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-next-with-for-statement ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "For i = 1 to 10")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tFor i = 1 to 10\n\t\t\nNext" (buffer-string)))))


;; --------------------------------------------------
;; -- While/Wend

;; TODO: These are weird because writing For on the first line of a file indents it incorrectly.

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-wend-with-while-statement ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "While True")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tWhile True\n\t\t\nWend" (buffer-string)))))


;; --------------------------------------------------
;; -- While/Wend

;; TODO: These are weird because writing For on the first line of a file indents it incorrectly.

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-wend-with-while-statement ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "While True")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tWhile True\n\t\t\nWend" (buffer-string)))))


;; --------------------------------------------------
;; -- Try/End Try

;; TODO: These are weird because writing For on the first line of a file indents it incorrectly.

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-try-with-try-statement ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "Try")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tTry\n\t\t\nEnd Try" (buffer-string)))))


;; --------------------------------------------------
;; -- Select/End Select

;; TODO: These are weird because writing For on the first line of a file indents it incorrectly.

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-select-with-select-statement ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "Select")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tSelect\n\t\t\nEnd Select" (buffer-string)))))

;;; blitzmax-mode-pair-insertion-test.el ends here
