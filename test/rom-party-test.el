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
  (insert (aref
           (extmap-get rom-party--extmap rom-party--all-words-key)
           (car (extmap-get rom-party--extmap (intern prompt)))))
  (call-interactively #'widget-field-activate))

(defmacro rom-party--test-setup (&rest body)
  "Perform common test setup, and then run BODY."
  `(let ((rom-party-index-async nil)
         (rom-party-config-directory "/tmp/rom-party")
         (rom-party--download-index nil))
     (when (get-buffer rom-party-buffer-name) (kill-buffer rom-party-buffer-name))
     (f-delete rom-party-config-directory t)
     ,@body))

(describe "rom-party"
  (it "Can index and input to prompt with classic configuration"
    (rom-party--test-setup
     (rom-party)
     (with-current-buffer rom-party-buffer-name
       (let ((prompt rom-party--prompt)
             (used-letters (copy-tree rom-party--used-letters)))
         (rom-party--insert-solution prompt)
         (expect prompt :not :to-be nil)
         (expect rom-party--used-letters :not :to-equal used-letters)))))

  (it "All index elements are lists"
    (rom-party--test-setup
     (rom-party)
     (expect (--map (listp (extmap-get rom-party--extmap (intern it))) (rom-party--prompts))
             :not :to-contain nil)))

  (it "Can index and input to prompt with infinite configuration"
    (rom-party--test-setup
     (rom-party-infinite)
     (with-current-buffer rom-party-buffer-name
       (let ((prompt rom-party--prompt)
             (used-letters (copy-tree rom-party--used-letters)))
         (expect prompt :not :to-be nil)
         (rom-party--insert-solution prompt)
         (expect rom-party--used-letters :not :to-equal used-letters)))))

  (it "Can download index"
    (rom-party--test-setup
     (let ((rom-party--download-index t))
       (rom-party)
       (with-current-buffer rom-party-buffer-name
         (let ((prompt rom-party--prompt)
               (used-letters (copy-tree rom-party--used-letters)))
           (expect prompt :not :to-be nil)
           (rom-party--insert-solution prompt)
           (expect rom-party--used-letters :not :to-equal used-letters))))))

  (it "Can set custom word sources"
    (rom-party--test-setup
     (let ((rom-party-word-sources
            (list
             (cons "sowpods.txt" "http://norvig.com/ngrams/sowpods.txt")
             (cons "enable1.txt" "http://norvig.com/ngrams/enable1.txt")
             (cons "word.list" "http://norvig.com/ngrams/word.list"))))
       (rom-party)
       (with-current-buffer rom-party-buffer-name
         (let ((prompt rom-party--prompt)
               (used-letters (copy-tree rom-party--used-letters)))
           (expect prompt :not :to-be nil)
           (rom-party--insert-solution prompt)
           (expect rom-party--used-letters :not :to-equal used-letters)))))))
