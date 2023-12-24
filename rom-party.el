;;; rom-party.el --- Bomb Party... in Emacs -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/LaurenceWarne/rom-party.el
;; Package-Requires: ((emacs "28") (dash "2.17.0") (f "0.2.0") (s "1.12.0") (ht "2.3"))

;;; Commentary:

;; Bomb Party... in Emacs

;;; Code:

(require 'hashtable-print-readable)
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

(defcustom rom-party-input-box-width
  35
  "The width of the rom party user input widget."
  :group 'rom-party
  :type 'integer)

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
(defvar-local rom-party--lives 2)
(defvar-local rom-party--run 0)
(defvar-local rom-party--used-letters nil)

;; Faces

(defface rom-party-health
  '((((class color)) (:foreground "red" :bold t))
    (t (:bold t)))
  "Face used for health in a rom party buffer.")

(defface rom-party-used-letter
  '((((class color)) (:foreground "DimGrey"))
    (t (:bold t)))
  "Face used for used letters in a rom party buffer.")

(defface rom-party-unused-letter
  '((t (:bold t)))
  "Face used for unused letters in a rom party buffer.")

(defface rom-party-input-prompt
  '((t (:bold t :height 200 :width 200)))
  "Face used for the rom party prompt in a rom party buffer.")

;; Internal functions

(defun rom-party--table-to-json (hash-table)
  "Convert HASH-TABLE to a json string."
  (s-join ","
          (ht-map (lambda (k v) (format "%S: [%s]"
                                        k
                                        (s-join "," (-map (lambda (s) (format "%S" s)) v))))
                  hash-table)))

(defun rom-party--index-words ()
  "Using words from `rom-party-word-sources', create an index of words."
  (f-mkdir rom-party-config-directory)
  (let ((merged-table
         (rom-party--merge-hash-tables
          (-map (lambda (source-entry)
                  (-let* (((file . source) source-entry)
                          (path (f-join rom-party-config-directory file)))
                    (unless (f-exists-p path)
                      (message "Downloading %s from %s..." file source)
                      (f-write (with-current-buffer (url-retrieve-synchronously source)
                                 (buffer-string))
                               'utf-8
                               path))
                    (let* ((index-path (rom-party--index-path path)))
                      (if (f-exists-p index-path)
                          (read index-path)
                        (message "Indexing words for %s ..." path)
                        (--doto (rom-party--substring-frequencies
                                 (s-lines (f-read-text path 'utf-8)))
                          ;; (f-write (prin1 it) 'utf-8 index-path)
                          )))))
                rom-party-word-sources))))
    (setq rom-party--table merged-table)))

(defun rom-party--merge-hash-tables (tables)
  "Merge TABLES into one hashmap, concatenating keys where applicable.

The first table is modified in place."
  (when-let* ((first (car tables)))
    (--each (cdr tables)
      (ht-map
       (lambda (k v)
         (ht-set first k (append (ht-get first k v))))))
    first))

(defun rom-party--index-path (file-path)
  "Return the path of the index file for FILE-PATH."
  (concat file-path ".index"))

(defun rom-party--substring-frequencies (words)
  "Calculate substring frequences from WORDS as a hash table."
  (let ((substring-table (ht-create #'equal)))
    (-each words
      (lambda (word)
        (when-let* ((adjusted-word (downcase word))
                    (length (length adjusted-word))
                    ((< 1 length))
                    (last-pair (substring adjusted-word (- length 2) length)))
          (ht-set substring-table
                  last-pair
                  (cons adjusted-word (ht-get substring-table last-pair)))
          (cl-loop for i from 0 below (- length 2)
                   for sub3 = (substring adjusted-word i (+ i 3))
                   for sub2 = (substring adjusted-word i (+ i 2))
                   do (--each (list sub2 sub3)
                        (when (string-match-p (rx bos (+ alpha) eos) it)
                          (ht-set substring-table
                                  it
                                  (cons adjusted-word (ht-get substring-table it)))))))))
    substring-table))

(defun rom-party--select-substring ()
  "Select a substring from the hash table of indexed words."
  (seq-random-elt (ht-keys rom-party--table)))

(defun rom-party--input-activated (&rest _ignore)
  "Process the result of a user enter."
  (let ((user-attempt (downcase (widget-value rom-party--input))))
    (if (and (-contains-p (ht-get rom-party--table rom-party--prompt) user-attempt)
             (s-contains-p rom-party--prompt user-attempt))
        (progn (message "Correct!")
               (cl-incf rom-party--run)
               (--each (string-to-list user-attempt)
                 (setcdr (assoc it rom-party--used-letters) t))
               (when (--all-p (cdr it) rom-party--used-letters)
                 (rom-party--reset-used-letters)
                 (cl-incf rom-party--lives))
               (rom-party--draw-buffer))
      (message "Incorrect!"))))

(defun rom-party--reset-used-letters ()
  "Reset `rom-party--used-letters'."
  (setq rom-party--used-letters
        (-zip-fill nil (number-sequence ?a (+ ?a 25)))))

(defun rom-party--offset-given-width (width w)
  "Get offset for inserting an object of width W in a total width of WIDTH."
  (max 0 (- (/ width 2) (/ w 2))))

(defun rom-party--insert-offset (width w)
  "Insert offset for width W given total width WIDTH."
  (widget-insert (s-repeat (rom-party--offset-given-width width w) " ")))

(defun rom-party--draw-buffer ()
  "Draw the rom party buffer."
  (let ((buf (get-buffer-create rom-party-buffer-name))
        (inhibit-read-only t)
        (width (window-width)))
    (with-current-buffer buf
      (when (widgetp rom-party--input) (widget-delete rom-party--input))
      (erase-buffer)
      (remove-overlays)
      (let ((title (concat "💾 Party " (s-repeat rom-party--lives "O"))))
        (rom-party--insert-offset width (length title))
        (widget-insert title))
      (let ((ov (make-overlay (- (point) rom-party--lives) (point))))
        (overlay-put ov 'face 'rom-party-health))
      (widget-insert "\n\n")
      (rom-party--insert-offset width (length rom-party--prompt))
      (widget-insert (setq rom-party--prompt (rom-party--select-substring)))
      (let ((ov (make-overlay (- (point) (length rom-party--prompt)) (point))))
        (overlay-put ov 'face 'rom-party-input-prompt))
      (widget-insert "\n")
      (rom-party--insert-offset width rom-party-input-box-width)
      (setq rom-party--input
            (widget-create 'editable-field
                           :action #'rom-party--input-activated
                           :size rom-party-input-box-width
                           :format "%v" ; Text after the field!
                           :keymap rom-party-widget-field-keymap
                           ""))
      (widget-insert "\n\n")
      (rom-party--render-used-letters width)
      (use-local-map rom-party-keymap)
      (widget-setup)
      ;; Focus the editable widget
      (widget-move -1 t))
    (display-buffer buf '(display-buffer-same-window))))

(defun rom-party--render-used-letters (width)
  "Render used letters, given a window with of WIDTH."
  (unless rom-party--used-letters (rom-party--reset-used-letters))
  (rom-party--insert-offset width (+ 24 25))
  (--each rom-party--used-letters
    (widget-insert (format " %c" (car it)))
    (let ((ov (make-overlay (1- (point)) (point))))
      (overlay-put ov 'face
                   (if (cdr it) 'rom-party-used-letter 'rom-party-unused-letter)))))

;; Commands

(defun rom-party-skip ()
  "Skip the current rom party prompt."
  (interactive)
  (rom-party--draw-buffer))

(defun rom-party-hint ()
  "Hint solutions for the current target substring to the echo area."
  (interactive)
  (let* ((valid (ht-get rom-party--table rom-party--prompt)))
    (message (s-join ", " (-take 10 (--sort (< (length it) (length other)) valid))))))

;;;###autoload
(defun rom-party ()
  "Run rom party."
  (interactive)
  (unless rom-party--table (rom-party--index-words))
  (rom-party--draw-buffer))

(provide 'rom-party)

;;; rom-party.el ends here
