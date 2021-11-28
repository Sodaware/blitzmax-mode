;;; blitzmax-mode-pair-insertion-test.el --- Tests for automatic insertion -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for blitzmax-mode automatic pair insertion.

;;; Code:

;; --------------------------------------------------
;; -- Extern/End Extern insertion

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-extern-for-new-extern ()
  (with-blitzmax-mode-text-test
   ("")
   (insert "Extern")
   (blitzmax-mode-newline-and-indent)
   (should (string= "Extern\n\t\nEnd Extern" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-with-closed-extern-afterwards ()
  (with-blitzmax-mode-text-test
   ("\nExtern\n\t\nEnd Extern")

   (insert "Extern")
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
   (blitzmax-mode-newline-and-indent)
   (should (string= "Type Test\n\t\nEnd Type" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-with-closed-type-afterwards ()
  (with-blitzmax-mode-text-test
   ("\nType First\n\t\nEnd Type")

   (insert "Type Test")
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
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tFunction Test\n\t\t\nEnd Function" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-with-closed-function-afterwards ()
  (with-blitzmax-mode-text-test
   ("\nFunction First\n\t\nEnd Function")

   (insert "Function Test")
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
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tMethod Test\n\t\t\nEnd Method" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/inserts-end-with-closed-method-afterwards ()
  (with-blitzmax-mode-text-test
   ("\nMethod First\n\t\nEnd Method")

   (insert "Method Test")
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tMethod Test\n\t\t\nEnd Method\nMethod First\n\t\nEnd Method" (buffer-string)))))

(ert-deftest blitzmax-mode-pair-insertion-test/does-not-insert-end-with-closed-method ()
  (with-blitzmax-mode-text-test
   ("Method Test\n\t\nEnd Method")
   (end-of-line)
   (blitzmax-mode-newline-and-indent)
   (should (string= "\tMethod Test\n\t\t\n\t\nEnd Method" (buffer-string)))))


;; --------------------------------------------------
;; -- Abstract Function/Nethod tests

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

;;; blitzmax-mode-pair-insertion-test.el ends here
