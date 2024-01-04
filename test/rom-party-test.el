;;; rom-party-test.el --- Tests for rom-party.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for rom-party.el

;;; Code:

(require 'buttercup)

(require 'rom-party)

(describe "rom-party"
  (it "Can index and input to prompt"
    (rom-party)
    (with-current-buffer rom-party-buffer-name
      (let ((prompt rom-party--prompt))
        (expect prompt :not :to-be nil)))))
