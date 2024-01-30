;;; rom-party.el --- Bomb Party -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/LaurenceWarne/rom-party.el
;; Package-Requires: ((emacs "28") (dash "2.17.0") (f "0.2.0") (s "1.12.0") (ht "2.3") (extmap "1.3") (compat "29.1.4.4") (async "1.9.7"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The main entry point to the package is the command `rom-party' which
;; will start a new game.  The objective of the game is, given a two or
;; three letter prompt, to find any word containg that prompt as a
;; substring in the given time limit (as in https://jklm.fun/).

;; The difficulty of the game can be configured by the user custom
;; variable `rom-party-prompt-filter', see the docstring for more
;; information.

;; Note by default on first invocation, `rom-party' will download a
;; remote index file asynchronously and so may not start straight away.

;;; Code:

(require 'async)
(require 'cl-lib)
(require 'compat)
(require 'dash)
(require 'ewoc)
(require 'extmap)
(require 'f)
(require 'ht)
(require 's)
(require 'wid-edit)
(require 'widget)
(require 'url)

;;; Classes

(defclass rom-party-configuration ()
  ((name :initarg :name
         :type string
         :custom 'string
         :documentation "The name of this configuration")
   (description :initarg :description
                :type string
                :custom 'string
                :documentation "A short description of this configuration")
   (show-timer :initarg :show-timer
               :type boolean
               :custom 'boolean
               :documentation "If non-nil show a timer in rom-party buffers when using this configuration."))
  "A class holding configuration for rom party, e.g. how to choose prompts.")

(cl-defmethod rom-party-select-prompt ((_configuration rom-party-configuration))
  "Select a rom party prompt for this configuration."
  (rom-party--select-prompt))

;;; Custom variables

(defgroup rom-party nil
  "Bomb Party... in Emacs."
  :group 'games)

(defcustom rom-party-word-sources
  (list
   ;; See also https://www.reddit.com/r/BombParty/comments/3lehxq/a_nearly_exhaustive_subset_of_the_bombparty/
   (cons "sowpods.txt" "http://norvig.com/ngrams/sowpods.txt")
   (cons "enable1.txt" "http://norvig.com/ngrams/enable1.txt"))
  "A list of cons cells each of which define a source of words.

The car of each cell is the name of a file to include in the ROM party word
list, and the cdr of each cell is a url to download from in the case the file
does not exist (the cdr is only required if the file does not exist).

Each file should be a text file with words delimited by a new line.

The file may be an absolute path, else is assumed to be relative to
`rom-party-config-directory'.  Any downloaded files will also be placed in this
directory.

Note changing the value of this variable will prompt a re-indexing, even within
the same Emacs session."
  :group 'rom-party
  :type '(alist :key-type string :value-type string))

(defcustom rom-party-config-directory
  (f-join user-emacs-directory "rom-party")
  "The directory used to store rom party configuration."
  :group 'rom-party
  :type 'directory)

(defcustom rom-party-input-box-width 35
  "The width (in characters) of the rom party user input widget."
  :group 'rom-party
  :type 'integer)

(defcustom rom-party-classic-configuration
  (rom-party-configuration
   :name "Classic"
   :description "Start with two lives, on a five second timer."
   :show-timer t)
  "The \"classic\" rom party configuration."
  :group 'rom-party
  :type 'object)

(defcustom rom-party-infinite-configuration
  (rom-party-configuration
   :name "Infinite"
   :description "No timer or lives."
   :show-timer nil)
  "The \"infinite\" rom party configuration."
  :group 'rom-party
  :type 'object)

(defcustom rom-party-timer-seconds 5
  "The number of starting seconds for the rom party timer."
  :group 'rom-party
  :type 'integer)

(defcustom rom-party-starting-lives 2
  "The number of lives to start off with."
  :group 'rom-party
  :type 'integer)

(defcustom rom-party-skip-on-end-of-timer t
  "If non-nil, issue a new prompt when the timer is up."
  :group 'rom-party
  :type 'boolean)

(defcustom rom-party-prompt-filter
  (lambda (_prompt words) (>= (length words) 50))
  "Function called to filter rom party prompts.

It should take two arguments, the first of which is the prompt itself, and the
second, the words matching the prompt."
  :group 'rom-party
  :type 'function)

(defcustom rom-party-configurations
  (list rom-party-classic-configuration rom-party-infinite-configuration)
  "A list of usable rom party configurations."
  :group 'rom-party
  :type '(repeat object))

(defcustom rom-party-default-configuration
  rom-party-classic-configuration
  "The default rom party configuration.

rom party configurations are used to determine whether to use a timer and how
prompts are selected.

See `rom-party-configurations' for a list of available configurations,
alternatively you may define your own, see `rom-party-configuration'."
  :group 'rom-party
  :type 'object)

(defcustom rom-party-index-async
  t
  "If non-nil run indexing asynchronously."
  :group 'rom-party
  :type 'boolean)

;;; Constants

(defconst rom-party-version "0.1.0")
(defconst rom-party-buffer-name "*ROM Party*")
(defconst rom-party--used-files-key (intern "__used-files"))
(defconst rom-party--all-words-key (intern "__all-words"))
(defconst rom-party--letter-offset (+ 24 25))
(defconst rom-party--index-format-url
  "https://github.com/LaurenceWarne/rom-party.el/releases/download/%s/index.extmap")
(defconst rom-party--compressed-index-format-url
  "https://github.com/LaurenceWarne/rom-party.el/releases/download/%s/index.extmap.gz")
(defconst rom-party-index-extmap-file-name "index.extmap")

(defvar rom-party--extmap nil)
(defvar rom-party--words nil)
(defvar rom-party--download-index t)

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

(defvar-keymap rom-party--game-over-keymap
  "r"             #'rom-party-skip
  "M-s"           #'rom-party-skip
  "q"             #'kill-this-buffer)

(defvar-local rom-party--input nil)
(defvar-local rom-party--prompt nil)
(defvar-local rom-party--lives rom-party-starting-lives)
(defvar-local rom-party--run 0)
(defvar-local rom-party--used-letters nil)
(defvar-local rom-party--timer nil)
(defvar-local rom-party--timer-time nil)
(defvar-local rom-party--timer-node nil)
(defvar-local rom-party--title-node nil)
(defvar-local rom-party--ewoc nil)
(defvar-local rom-party--game-over nil)
(defvar-local rom-party--configuration nil)

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
  '((t (:bold t :height 200)))
  "Face used for the rom party prompt in a rom party buffer.")

(defface rom-party-game-over
  '((t (:bold t :underline t)))
  "Face used for the game over title in a rom party buffer.")

;; Functions

(defun rom-party-log-weight-function (_prompt matching)
  "Return the logarithm of the length of MATCHING.

It's purpose is for use with `rom-party-weight-function'."
  (round (log (length matching))))

;; Internal functions

(defun rom-party--prompts ()
  "Return all possible rom party prompts."
  (let ((key-names (-map #'symbol-name (extmap-keys rom-party--extmap))))
    (--filter (not (s-prefix-p "__" it)) key-names)))

(defun rom-party--select-prompt ()
  "Select a random prompt."
  (let* ((choice (seq-random-elt (rom-party--prompts)))
         (matching (extmap-get rom-party--extmap (intern choice))))
    (or (and (listp matching)
             (funcall rom-party-prompt-filter choice matching)
             choice)
        (rom-party--select-prompt))))

(defun rom-party--desired-source-files ()
  "Get a list of desired source files."
  (--map (car it) rom-party-word-sources))

(defun rom-party--used-source-files ()
  "Get a list of currently used source files."
  (extmap-get rom-party--extmap rom-party--used-files-key))

(defun rom-party-index-path ()
  "Return the path of the rom party index file."
  (f-join rom-party-config-directory rom-party-index-extmap-file-name))

(defun rom-party--word-files-changed ()
  "Return t if word files have changed."
  (unless rom-party--extmap (setq rom-party--extmap (extmap-init (rom-party-index-path))))
  (not (equal (rom-party--desired-source-files) (rom-party--used-source-files))))

(defun rom-party--index-words-async (callback)
  "Index words from `rom-party-word-sources', and then call CALLBACK."
  (message "Starting word indexing, this may take a short while...")
  (async-start
   `(lambda ()
      ,(async-inject-variables "load-path")
      (prog1 (float-time)
        (require 'rom-party)
        (rom-party-index-words)))
   (lambda (start-time)
     (let ((finished-time (float-time)))
       ;; TODO output no words here too
       (message "Indexing complete in %.2f seconds" (- finished-time start-time)))
     (funcall callback))))

(defun rom-party--index-words ()
  "Using words from `rom-party-word-sources', create an index of words."
  (let* ((start-time (float-time))
         (all-words
          ;; We load all words from all sources into memory ahead of time, and then dedupe.
          ;; We could dedupe incrementally instead.
          (-sort #'string<
                 (-distinct (-mapcat (lambda (source-entry)
                                       (-let* (((file . source) source-entry)
                                               (path (if (f-exists-p file) file (f-join rom-party-config-directory file))))
                                         (unless (f-exists-p path)
                                           (message "Downloading %s from %s..." file source)
                                           (f-write (with-current-buffer (url-retrieve-synchronously source)
                                                      (buffer-string))
                                                    'utf-8
                                                    path))
                                         (-map #'downcase (s-lines (f-read-text path 'utf-8)))))
                                     rom-party-word-sources))))
         (word-hashtable (ht<-alist (--map-indexed (cons it  it-index) all-words)))
         (frequencies-table (rom-party--substring-frequencies all-words word-hashtable)))
    ;; We need to re-index if the source words change between invocations
    (ht-set frequencies-table (symbol-name rom-party--used-files-key) (rom-party--desired-source-files))
    (extmap-from-alist (f-join rom-party-config-directory "index.extmap")
                       (append (--map (cons (intern (car it)) (cdr it))
                                      (ht->alist frequencies-table))
                               (list (cons rom-party--all-words-key (vconcat all-words))))
                       :overwrite t)
    (let ((finished-time (float-time)))
      (message "Indexed a total of %s words in %.2f seconds"
               (length all-words)
               (- finished-time start-time)))
    frequencies-table))

(defun rom-party--download-index-async (callback)
  "Download the rom party index and call CALLBACK."
  (let ((start-time (float-time)))
    (async-start
     `(lambda ()
        ,(async-inject-variables "load-path")
        (require 'rom-party)
        (let* ((out-file-name (concat (file-name-as-directory rom-party-config-directory)
                                      rom-party-index-extmap-file-name))
               (compressed-out-file-name (concat out-file-name ".gz")))
          (if (executable-find "gunzip")
              (progn (url-copy-file (format rom-party--compressed-index-format-url rom-party-version)
                                    compressed-out-file-name
                                    t)
                     (shell-command (format "gunzip %s" out-file-name)))
            (message "gunzip not found on path, downloading uncompressed file...")
            (url-copy-file (format rom-party--index-format-url rom-party-version)
                           out-file-name
                           t))))
     (lambda (&rest _)
       (let ((finished-time (float-time)))
         (message "Downloaded index in %.2f seconds" (- finished-time start-time)))
       (funcall callback)))))

(defun rom-party--get-or-create-index (callback)
  "Create an index if necessay and then call CALLBACK."
  (f-mkdir rom-party-config-directory)
  (let* ((index-path (rom-party-index-path))
         (file-exists (f-exists-p index-path))
         (do-overwrite (and file-exists (rom-party--word-files-changed)))
         (create-index (or (null file-exists) do-overwrite)))
    (cl-flet ((finish ()
                (setq rom-party--extmap (extmap-init index-path))
                (funcall callback)))
      (cond
       ;; TODO do a version check here too
       ((and rom-party--download-index
             (null file-exists)
             (equal (eval (car (get 'rom-party-word-sources 'standard-value))) rom-party-word-sources))
        (message "Downloading index...")
        (if rom-party-index-async
            (rom-party--download-index-async #'finish)
          (url-copy-file (format rom-party--index-format-url rom-party-version)
                         index-path
                         t)
          (finish)))
       ;; Check if we need to manually index (async)
       ((and rom-party-index-async create-index)
        (message (if do-overwrite "Word files changed, re-indexing async..."
                   "Performing initial indexing async..."))
        (rom-party--index-words-async #'finish))
       ;; Check if we need to manually index (sync)
       (create-index
        (message (if do-overwrite "Word files changed, re-indexing..."
                   "Performing initial indexing..."))
        (rom-party--index-words)
        (finish))
       ;; Index is already up to date
       (t (finish))))))

(defun rom-party--merge-hash-tables (tables)
  "Merge TABLES into one hashmap, concatenating keys where applicable.

The first table is modified in place."
  (when-let* ((first (car tables)))
    (--each (cdr tables)
      (ht-map
       (lambda (k v)
         (ht-set first k (append (ht-get first k v))))
       it))
    first))

(defun rom-party--substring-frequencies (words word-hashtable)
  "Calculate substring frequences from WORDS as a hash table, using WORD-HASHTABLE."
  (let ((substring-table (ht-create #'equal)))
    (-each words
      (lambda (word)
        (when-let* ((idx (ht-get word-hashtable word))
                    (length (length word))
                    ((< 1 length))
                    (last-pair (substring word (- length 2) length)))
          (ht-set substring-table
                  last-pair
                  (cons idx (ht-get substring-table last-pair)))
          (cl-loop for i from 0 below (- length 2)
                   for sub3 = (substring word i (+ i 3))
                   for sub2 = (substring word i (+ i 2))
                   do (--each (list sub2 sub3)
                        (when (string-match-p (rx bos (+ alpha) eos) it)
                          (ht-set substring-table
                                  it
                                  (cons idx (ht-get substring-table it)))))))))
    substring-table))

(defun rom-party--input-activated (&rest _ignore)
  "Process the result of a user enter."
  (unless rom-party--game-over
    (if-let* ((user-attempt (s-trim (downcase (widget-value rom-party--input))))
              (all-words (extmap-get rom-party--extmap rom-party--all-words-key))
              (idx (rom-party--binary-search user-attempt all-words #'string<))
              ((and (-contains-p (extmap-get rom-party--extmap (intern rom-party--prompt)) idx)
                    (s-contains-p rom-party--prompt user-attempt))))
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
  (max 0 (/ (- width w) 2)))

(defun rom-party--insert-offset (w)
  "Insert spaces so that text of width W will be centred."
  (widget-insert (s-repeat (rom-party--offset-given-width (window-width) w) " ")))

(defun rom-party--insert-text-centrally (s)
  "Insert S in the centre of the current buffer."
  (rom-party--insert-offset (length s))
  (widget-insert s))

(defun rom-party--draw-title ()
  "Draw the title for a rom party buffer."
  (let ((title (concat "💾 Party " (s-repeat rom-party--lives "O"))))
    (rom-party--insert-text-centrally title))
  (let ((ov (make-overlay (- (point) rom-party--lives) (point))))
    (overlay-put ov 'face 'rom-party-health))
  (widget-insert "\n"))

(defun rom-party--draw-timer (time)
  "Draw timer with TIME."
  ;; Aligning with the input box looks nicer than aligning with the window
  (rom-party--insert-offset rom-party--letter-offset)
  (widget-insert (s-repeat (* 2 time) "O")))

(defun rom-party--draw-prompt ()
  "Draw the rom party prompt."
  (rom-party--insert-text-centrally rom-party--prompt)
  (let ((ov (make-overlay (- (point) (length rom-party--prompt)) (point))))
    ;; The :height attribute uncenters the text
    (overlay-put ov 'face 'rom-party-input-prompt)))

(defun rom-party--draw-input ()
  "Draw the rom party input box."
  (rom-party--insert-offset rom-party-input-box-width)
  (setq rom-party--input
        (widget-create 'editable-field
                       :action #'rom-party--input-activated
                       :size rom-party-input-box-width
                       :format "%v" ; Text after the field!
                       :keymap rom-party-widget-field-keymap
                       ""))
  (widget-insert "\n"))

(defun rom-party--draw-letters ()
  "Draw used/unused letters."
  (unless rom-party--used-letters (rom-party--reset-used-letters))
  (rom-party--insert-offset rom-party--letter-offset)
  (--each rom-party--used-letters
    (widget-insert (format "%c " (car it)))
    (let ((ov (make-overlay (- (point) 2) (1- (point)))))
      (overlay-put ov 'face
                   (if (cdr it) 'rom-party-used-letter 'rom-party-unused-letter))))
  (widget-insert "\n"))

(defun rom-party--draw-node (data)
  "Draw a rom party node with DATA."
  (-let* (((id . content) data))
    (cond ((eq id 'rom-party-title) (rom-party--draw-title))
          ((eq id 'rom-party-prompt) (rom-party--draw-prompt))
          ((eq id 'rom-party-input) (rom-party--draw-input))
          ((eq id 'rom-party-letters) (rom-party--draw-letters))
          ((eq id 'rom-party-timer) (rom-party--draw-timer content))
          (t (message "%s not known" id)))))

(defun rom-party--process-timer-update ()
  "Process a timer update."
  (when-let ((buf (get-buffer rom-party-buffer-name))
             ((and (integerp rom-party--timer-time) (<= 0 rom-party--timer-time))))
    (with-current-buffer buf
      (cl-decf rom-party--timer-time)
      (ewoc-set-data rom-party--timer-node (cons 'rom-party-timer rom-party--timer-time))
      (ewoc-invalidate rom-party--ewoc rom-party--timer-node)
      (when (zerop rom-party--timer-time)
        (cl-decf rom-party--lives)
        (if (<= rom-party--lives 0)
            (rom-party--process-no-lives)
          (message "Times up!")
          (when rom-party-skip-on-end-of-timer (rom-party-skip)))))))

(defun rom-party--process-no-lives ()
  "Set the buffer state when the user has no lives."
  (setq rom-party--game-over t)
  (goto-char (point-max))
  (let* ((text "Game over!")
         (offset (rom-party--offset-given-width (window-width) (length text))))
    (rom-party--insert-text-centrally text)
    (let ((ov (make-overlay (- (point) (length text)) (point))))
      (overlay-put ov 'face 'rom-party-game-over))
    (ewoc-invalidate rom-party--ewoc rom-party--title-node)
    (widget-insert (format " Run: %s" rom-party--run))
    (widget-insert "\n\n")
    (widget-insert (format "%s[r] Start Again\n" (s-repeat offset " ")))
    (widget-insert (format "%s[q] Quit\n" (s-repeat offset " ")))
    (forward-line -1)
    (end-of-line))
  (use-local-map rom-party--game-over-keymap)
  (buffer-disable-undo))

(defun rom-party--draw-buffer ()
  "Draw the rom party buffer."
  ;; ATM this function will always clear the buffer and reset the word
  (let ((buf (get-buffer-create rom-party-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf

      ;; Reset buffer vars and widgets if necessary
      (when (widgetp rom-party--input) (widget-delete rom-party--input))
      (when (timerp rom-party--timer) (cancel-timer rom-party--timer))
      (setq rom-party--configuration (or rom-party--configuration
                                         rom-party-default-configuration)
            rom-party--prompt (rom-party-select-prompt rom-party--configuration))
      (when rom-party--game-over  ; Assume we've restarted
        (setq rom-party--game-over nil
              rom-party--lives rom-party-starting-lives
              rom-party--run 0)
        (rom-party--reset-used-letters)
        (buffer-enable-undo))
      (erase-buffer)
      (remove-overlays)
      (when (<= rom-party--lives 0) (setq rom-party--lives rom-party-starting-lives))

      ;; Redraw stuff
      (setq rom-party--ewoc (ewoc-create #'rom-party--draw-node nil))
      ;; Setup prompt and input
      ;; TODO consider how HEADER can be used in `ewoc-create' instead
      (setq rom-party--title-node (ewoc-enter-last rom-party--ewoc (cons 'rom-party-title nil)))
      (ewoc-enter-last rom-party--ewoc (cons 'rom-party-prompt nil))
      (ewoc-enter-last rom-party--ewoc (cons 'rom-party-input nil))
      (ewoc-enter-last rom-party--ewoc (cons 'rom-party-letters nil))

      ;; Setup timer
      (when (oref rom-party--configuration show-timer)
        (let ((timer-max-repeats rom-party-timer-seconds))
          (setq
           rom-party--timer (run-at-time t 1 #'rom-party--process-timer-update)
           rom-party--timer-time rom-party-timer-seconds
           rom-party--timer-node (ewoc-enter-last rom-party--ewoc (cons 'rom-party-timer rom-party-timer-seconds))))
        (add-hook 'kill-buffer-hook (lambda () (cancel-timer rom-party--timer)) nil t))
      
      (use-local-map rom-party-keymap)
      (widget-setup)
      ;; Focus the editable widget
      (widget-move -1 t))
    (display-buffer buf '(display-buffer-same-window))))

;; https://rosettacode.org/wiki/Binary_search#Emacs_Lisp
(defun rom-party--binary-search (value vector cmp)
  "Return the index of VALUE in VECTOR using comparator CMP.

If VALUE is not present in VECTOR, return nil."
  (let ((low 0)
        (high (1- (length vector))))
    (cl-do () ((< high low) nil)
      (let ((middle (floor (+ low high) 2)))
        (cond ((funcall cmp value (aref vector middle))
               (setq high (1- middle)))
              ((funcall cmp (aref vector middle) value)
               (setq low (1+ middle)))
              (t (cl-return middle)))))))

;;; Commands

(defun rom-party-skip ()
  "Skip the current rom party prompt."
  (interactive)
  (rom-party--draw-buffer))

(defun rom-party-hint ()
  "Hint solutions for the current prompt in the echo area."
  (interactive)
  (let* ((valid-idxs (extmap-get rom-party--extmap (intern rom-party--prompt)))
         (valid (--map (aref (extmap-get rom-party--extmap rom-party--all-words-key) it) valid-idxs)))
    (message (s-join ", " (-take 10 (--sort (< (length it) (length other)) valid))))))

;;;###autoload
(defun rom-party ()
  "Run rom party."
  (interactive)
  (if (or (null (f-exists-p (rom-party-index-path)))
          (null rom-party--extmap)
          (rom-party--word-files-changed))
      (rom-party--get-or-create-index #'rom-party--draw-buffer)
    (rom-party--draw-buffer)))

;;;###autoload
(defun rom-party-infinite ()
  "Run rom party with no timer or lives."
  (interactive)
  (let ((rom-party-default-configuration rom-party-infinite-configuration))
    (call-interactively #'rom-party)))

;;;###autoload
(defun rom-party-choose-configuration ()
  "Run rom party from a prompted configuration."
  (interactive)
  (cl-flet ((configuration-from-name (name)
              (--first (string= (oref it name) name) rom-party-configurations)))
    (let* ((mx-width (-max (--map (length (oref it name)) rom-party-configurations)))
           (completion-extra-properties
            `(:annotation-function
              ,(lambda (name)
                 (let ((desc (oref (configuration-from-name name) description)))
                   (format "%s    %s"
                           (make-string (- mx-width (length name)) ?\s)
                           desc)))))
           (configuration-choice
            (completing-read
             "Configuration: "
             (-map (lambda (conf) (oref conf name)) rom-party-configurations)))
           (rom-party-default-configuration (configuration-from-name configuration-choice)))
      (call-interactively #'rom-party))))

(provide 'rom-party)
;;; rom-party.el ends here
