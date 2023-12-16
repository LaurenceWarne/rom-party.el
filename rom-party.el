;;; rom-party.el --- Bomb Party... in Emacs -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/LaurenceWarne/rom-party.el
;; Package-Requires: ((emacs "28") (dash "2.17.0") (f "0.2.0") (s "1.12.0"))

;;; Commentary:

;; Bomb Party... in Emacs

;;; Code:

(require 'widget)
(require 'wid-edit)
(require 'f)
(require 'dash)
(require 's)

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

(defvar rom-party--frequency-table nil)

(defvar-local rom-party--input nil)
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
    (setq rom-party--frequency-table (rom-party--substring-frequencies words))))

(defun rom-party--substring-frequencies (words)
  "Calculate substring frequences from WORDS as a hash table."
  (let ((substring-frequencies (make-hash-table :test #'equal)))
    (-each words
      (lambda (word)
        (let ((as-list (s-split "" word t)))
          (--each (append (-zip-lists as-list (cdr as-list))
                          (-zip-lists as-list (cdr as-list) (cddr as-list)))
            (puthash (s-join "" it)
                     (1+ (gethash (s-join "" it) substring-frequencies 0))
                     substring-frequencies)))))
    substring-frequencies))

(defun rom-party--input-activated (a &rest ignore)
  (message (widget-value rom-party--input)))

(defun rom-party--redraw-buffer ()
  (interactive)
  (message (widget-value rom-party--input)))

(defun rom-party--draw-buffer ()
  (let ((buf (get-buffer-create rom-party-buffer-name)))
    (with-current-buffer buf
      (switch-to-buffer buf)
      (erase-buffer)
      (remove-overlays)
      (widget-insert "💾 Party ")
      (widget-insert (s-repeat rom-party--health "O"))
      (let ((ov (make-overlay (- (point) rom-party--health) (point))))
        (overlay-put ov 'face 'rom-party-health))
      (widget-insert "\n\n")
      (setq rom-party--input
            (widget-create 'editable-field
                           :action #'rom-party--input-activated
                           :size 13
                           :format " Name: %v " ; Text after the field!
                           ""))
      (widget-insert "\n")
      (use-local-map widget-keymap)
      (widget-setup)
      ;; Focus the editable widget
      (widget-move -1 t))
    (display-buffer buf)))

(defun widget-example ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer "*Widget Example*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Here is some documentation.\n\n")
  (widget-create 'editable-field
                 :size 13
                 :format "Name: %v " ; Text after the field!
                 "My Name")
  (widget-create 'menu-choice
                 :tag "Choose"
                 :value "This"
                 :help-echo "Choose me, please!"
                 :notify (lambda (widget &rest ignore)
                           (message "%s is a good choice!"
                                    (widget-value widget)))
                 '(item :tag "This option" :value "This")
                 '(choice-item "That option")
                 '(editable-field :menu-tag "No option" "Thus option"))
  (widget-create 'editable-field
                 :format "Address: %v"
                 "Some Place\nIn some City\nSome country.")
  (widget-insert "\nSee also ")
  (widget-create 'link
                 :notify (lambda (&rest ignore)
                           (widget-value-set widget-example-repeat
                                             '("En" "To" "Tre"))
                           (widget-setup))
                 "other work")
  (widget-insert
   " for more information.\n\nNumbers: count to three below\n")
  (setq widget-example-repeat
        (widget-create 'editable-list
                       :entry-format "%i %d %v"
                       :notify
                       (lambda (widget &rest ignore)
                         (let ((old (widget-get widget
                                                ':example-length))
                               (new (length (widget-value widget))))
                           (unless (eq old new)
                             (widget-put widget ':example-length new)
                             (message "You can count to %d." new))))
                       :value '("One" "Eh, two?" "Five!")
                       '(editable-field :value "three")))
  (widget-insert "\n\nSelect multiple:\n\n")
  (widget-create 'checkbox t)
  (widget-insert " This\n")
  (widget-create 'checkbox nil)
  (widget-insert " That\n")
  (widget-create 'checkbox
                 :notify (lambda (&rest ignore) (message "Tickle"))
                 t)
  (widget-insert " Thus\n\nSelect one:\n\n")
  (widget-create 'radio-button-choice
                 :value "One"
                 :notify (lambda (widget &rest ignore)
                           (message "You selected %s"
                                    (widget-value widget)))
                 '(item "One") '(item "Another One.")
                 '(item "A Final One."))
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (if (= (length
                                   (widget-value widget-example-repeat))
                                  3)
                               (message "Congratulation!")
                             (error "Three was the count!")))
                 "Apply Form")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (widget-example))
                 "Reset Form")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))

(provide 'rom-party)

;;; rom-party.el ends here
