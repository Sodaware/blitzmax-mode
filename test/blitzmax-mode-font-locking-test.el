;;; blitzmax-mode-font-locking-test.el --- Tests for blitzmax-mode font-locking.

;;; Commentary:

;; Tests for blitzmax-mode font-locking.  Opens a file with known contents and
;; checks that characters have the correct face.  Not elegant.

;;; Code:

;; --------------------------------------------------
;; -- Keyword Font-Locking

(ert-deftest blitzmax-mode-font-locking-test/test-keywords ()
  (with-blitzmax-mode-test
   ("keyword_font_locking.bmx")

   ;; Test type definition highlights.
   (should (equal 'font-lock-keyword-face (get-text-property 1 'face)))
   (should-not (equal 'font-lock-keyword-face (get-text-property 15 'face)))))


;;; blitzmax-mode-font-locking-test.el ends here
