;;; rom-party-test.el --- Tests for rom-party.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for rom-party.el

;;; Code:

(require 'buttercup)
(require 'extmap)
(require 'widget)

(require 'rom-party)

(describe "rom-party"
  (it "Can index and input to prompt"
    (rom-party)
    (with-current-buffer rom-party-buffer-name
      (let ((prompt rom-party--prompt)
            (used-letters (copy-tree rom-party--used-letters)))
        (expect prompt :not :to-be nil)
        (insert (car (extmap-get rom-party--extmap (intern prompt))))
        (call-interactively #'widget-field-activate)
        (expect rom-party--used-letters :not :to-equal used-letters)))))
