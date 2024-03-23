# rom-party.el
[![Min Emacs Version](https://img.shields.io/badge/Emacs-28+-7F5AB6?logo=gnu-emacs)](https://www.gnu.org/software/emacs/) [![MELPA](https://melpa.org/packages/rom-party-badge.svg)](https://melpa.org/#/rom-party)

[:bomb: Party](https://jklm.fun/)... in Emacs:

https://github.com/LaurenceWarne/rom-party.el/assets/17688577/396f71fb-6f6c-4c88-8dd6-f0526c02463c

## Installation

`rom-party` is available from [Melpa](https://melpa.org/):

```elisp
(use-package rom-party
  :commands (rom-party rom-party-infinite rom-party-choose-configuration)
  :config
  (setq rom-party-timer-seconds 10
        ;; `rom-party-config-directory' is used to store word files and index files
        rom-party-config-directory "my/custom/dir"))
```

## Usage

`M-x rom-party`, in a rom party buffer the following keybindings are also provided:

| Key   | Description                           |
|-------|---------------------------------------|
| `C-/` | Hints solutions to the current prompt |
| `M-s` | Skips the current prompt              |

There is another command `rom-party-infinite` which is the same as above, but with no timer or lives.  To look at all preset configurations see `rom-party-choose-configuration`.

### Chosen Words

You may mark a word as a "goto" for a certain prompt using the command: `rom-party-add-chosen-word`.  From then on, this word will be shown first in `rom-party-hint` and echoed when time runs out for that prompt.

The command `rom-party-train` can be used to take prompts solely from words you have added in this way.  Although changes to `rom-party-chosen-words` are persisted between sessions, it may useful to define in your configuration for when machines are switched:

```elisp
(setq rom-party-chosen-words '("rne" "garner" "tco" "outcome"))
```
## Customization

If you're finding it too difficult (or too easy!), the best way to alter the difficulty is by modifying `rom-party-prompt-filter`.  The function takes a candidate prompt (e.g. `ggi`) and the words matching it - a `nil` value means don't use the prompt.  E.g.

```elisp
(setq rom-party-prompt-filter (lambda (prompt words) (>= (length words) 50)))
```

Means any prompts with less than 10 matching solutions are not shown.

Where `rom-party` gets words from can be customised using `rom-party-word-sources`, note re-indexing may be required whenever the custom variable `rom-party-word-sources` is updated, which may take 20-30s depending on how large the word sources are.  Other useful custom variables are given below:

| Variable                     | Description                                             | Default                                      |
|------------------------------|---------------------------------------------------------|----------------------------------------------|
| `rom-party-config-directory` | Directory to store `rom-party` config and indexes       | `(concat user-emacs-directory "/rom-party")` |
| `rom-party-starting-lives`   | Starting number of lives                                | `2`                                          |
| `rom-party-timer-seconds`    | The number of starting seconds for the rom party timer. | `5`                                          |
