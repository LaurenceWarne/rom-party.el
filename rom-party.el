;;; rom-party.el --- Bomb Party... in Emacs -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/LaurenceWarne/rom-party.el
;; Package-Requires: ((emacs "28") (dash "2.17.0") (f "0.2.0") (s "1.12.0") (ht "2.3"))

;;; Commentary:

;; Bomb Party... in Emacs

;;; Code:

(require 'widget)
(require 'wid-edit)
(require 'f)
(require 'dash)
(require 's)
(require 'ht)

(defgroup rom-party nil
  "Bomb Party... in Emacs."
  :group 'applications)

(defcustom rom-party-word-sources
  (list
   (cons "sowpods.txt" "http://norvig.com/ngrams/sowpods.txt"))
  "A list of cons cells each of which define a source of words.

The car of each cell is the name of a file include in the ROM party word list,
and the cdr of each cell is a url to download from in the case the file does
not exist (in `rom-party-config-directory')."
  :group 'rom-party)

(defcustom rom-party-config-directory
  (f-join user-emacs-directory "rom-party")
  "The directory to store rom party configuration."
  :group 'rom-party
  :type 'directory)

(defconst rom-party-version "0.1.0")

(defconst rom-party-buffer-name "*ROM Party*")

(defvar rom-party--table nil)
(defvar rom-party--words nil)

(defvar-keymap rom-party-keymap
  :parent widget-keymap
  "M-s"           #'rom-party-skip
  "C-/"           #'rom-party-hint
  "C-RET"         #'rom-party-skip)

(defvar-keymap rom-party-widget-field-keymap
  :parent widget-field-keymap
  "M-s"           #'rom-party-skip
  "C-/"           #'rom-party-hint
  "C-RET"         #'rom-party-skip)

(defvar-local rom-party--input nil)
(defvar-local rom-party--prompt nil)
(defvar-local rom-party--health 2)

;; Faces

(defface rom-party-health
  '((((class color)) (:foreground "red" :bold t))
    (t (:bold t)))
  "Face used for health in a rom party buffer.")

;; Internal functions

(defun rom-party--index-words ()
  "Using words from `rom-party-word-sources', create an index of words."
  (f-mkdir rom-party-config-directory)
  (let ((words (-flatten
                (--map
                 (-let* (((file . source) it)
                         (path (f-join rom-party-config-directory file)))
                   (unless (f-exists-p path)
                     (message "Downloading %s from %s..." file source)
                     (f-write (with-current-buffer (url-retrieve-synchronously source)
                                (buffer-string))
                              'utf-8
                              path))
                   (s-lines (f-read-text path 'utf-8)))
                 rom-party-word-sources))))
    (message "Indexing words...")
    (setq rom-party--table (rom-party--substring-frequencies words)
          rom-party--words (-map #'downcase words))))

(defun rom-party--substring-frequencies (words)
  "Calculate substring frequences from WORDS as a hash table."
  (let ((substring-table (ht-create #'equal)))
    (-each words
      (lambda (word)
        (let ((as-list (s-split "" word t)))
          (--each (append (-zip-lists as-list (cdr as-list))
                          (-zip-lists as-list (cdr as-list) (cddr as-list)))
            (when-let* ((key (downcase (s-join "" it)))
                        ;; We only want prompts which are alphabetic
                        ((string-match-p (rx bos (+ alpha) eos) key)))
              (ht-set substring-table
                      key
                      (cons (downcase word) (ht-get substring-table key))))))))
    substring-table))

(defun rom-party--select-substring ()
  "Select a substring from the hash table of indexed words."
  (seq-random-elt (ht-keys rom-party--table)))

(defun rom-party--input-activated (&rest _ignore)
  "Process the result of a user enter."
  (let ((user-attempt (downcase (widget-value rom-party--input))))
    (if (and (-contains-p rom-party--words user-attempt)
             (s-contains-p rom-party--prompt user-attempt))
        (progn (message "Correct!")
               (rom-party--draw-buffer))
      (message "Incorrect!"))))

(defun rom-party--draw-buffer ()
  "Draw the rom party buffer."
  (let ((buf (get-buffer-create rom-party-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (when (widgetp rom-party--input) (widget-delete rom-party--input))
      (erase-buffer)
      (remove-overlays)
      (widget-insert "💾 Party ")
      (widget-insert (s-repeat rom-party--health "O"))
      (let ((ov (make-overlay (- (point) rom-party--health) (point))))
        (overlay-put ov 'face 'rom-party-health))
      (widget-insert "\n\n")
      (widget-insert
       (format "Target: %s\n"
               (setq rom-party--prompt (rom-party--select-substring))))
      (setq rom-party--input
            (widget-create 'editable-field
                           :action #'rom-party--input-activated
                           :size 13
                           :format "Input: %v " ; Text after the field!
                           :keymap rom-party-widget-field-keymap
                           ""))
      (widget-insert "\n")
      (use-local-map rom-party-keymap)
      (widget-setup)
      ;; Focus the editable widget
      (widget-move -1 t))
    (display-buffer buf)))

;; Commands

(defun rom-party-skip ()
  (interactive)
  (rom-party--draw-buffer))

(defun rom-party-hint ()
  "Hint solutions for the current target substring to the echo area."
  (interactive)
  (let* ((valid (ht-get rom-party--table rom-party--prompt)))
    (message (s-join ", " (-take 10 (--sort (< (length it) (length other)) valid))))))

(defun rom-party ()
  "Run rom party."
  (interactive)
  (unless rom-party--words (rom-party--index-words))
  (rom-party--draw-buffer))

(provide 'rom-party)

;;; rom-party.el ends here
