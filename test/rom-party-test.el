;;; rom-party-test.el --- Tests for rom-party.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for rom-party.el

;;; Code:

(require 'buttercup)
(require 'extmap)
(require 'widget)

(require 'rom-party)

(defun rom-party--insert-solution (prompt)
  "Insert a solution for PROMPT."
  (insert (car (extmap-get rom-party--extmap (intern prompt))))
  (call-interactively #'widget-field-activate))

(describe "rom-party"
  (it "Can index and input to prompt with classic configuration"
    (let ((rom-party-index-async nil))
      (rom-party)
      (with-current-buffer rom-party-buffer-name
        (let ((prompt rom-party--prompt)
              (used-letters (copy-tree rom-party--used-letters)))
          (rom-party--insert-solution prompt)
          (expect prompt :not :to-be nil)
          (expect rom-party--used-letters :not :to-equal used-letters)))))

  (it "Can index and input to prompt with infinite configuration"
    (let ((rom-party-index-async nil))
      (rom-party-infinite)
      (with-current-buffer rom-party-buffer-name
        (let ((prompt rom-party--prompt)
              (used-letters (copy-tree rom-party--used-letters)))
          (expect prompt :not :to-be nil)
          (rom-party--insert-solution prompt)
          (expect rom-party--used-letters :not :to-equal used-letters))))))
