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


;; --------------------------------------------------
;; -- Comment Font-Locking

(ert-deftest blitzmax-mode-font-locking-test/test-single-line-comments ()
  (with-blitzmax-mode-test
   ("comments_font_locking.bmx")

   ;; First line is a comment.
   (should (equal 'font-lock-comment-face (get-text-property 1 'face)))

   ;; Second line is highlighted.
   (should (equal 'font-lock-keyword-face (get-text-property 21 'face)))

   ;; Third line is a comment.
   (should (equal 'font-lock-comment-face (get-text-property 51 'face)))))

(ert-deftest blitzmax-mode-font-locking-test/test-multi-line-comments ()
  (with-blitzmax-mode-test
   ("comments_font_locking.bmx")

   ;; Opening statement and first line  of REM is a comment.
   (should (equal 'font-lock-comment-face (get-text-property 84 'face)))
   (should (equal 'font-lock-comment-face (get-text-property 88 'face)))

   ;; Nothing is highlighted inside the REM
   (should-not (equal 'font-lock-keyword-face (get-text-property 118 'face)))

   ;; No longer a comment outside rem.
   (should-not (equal 'font-lock-comment-face (get-text-property 166 'face)))))


;; --------------------------------------------------
;; -- Bug fixes

(ert-deftest blitzmax-mode-font-locking-test/issue-11 ()
  (with-blitzmax-mode-test
   ("issue_011.bmx")

   ;; Text after the backslash should not be a string
   (should-not (equal 'font-lock-string-face (get-text-property 50 'face)))))


;;; blitzmax-mode-font-locking-test.el ends here
