[![Min Emacs Version](https://img.shields.io/badge/Emacs-28+-7F5AB6?logo=gnu-emacs)](https://www.gnu.org/software/emacs/)

# rom-party.el

[:bomb: Party](https://jklm.fun/)... in Emacs:

https://github.com/LaurenceWarne/rom-party.el/assets/17688577/396f71fb-6f6c-4c88-8dd6-f0526c02463c

## Installation

```elisp
(use-package rom-party
  :config
  (setq rom-party-timer-seconds 10
        ;; `rom-party-config-directory' is used to store word files and index files
        rom-party-config-directory "my/custom/dir"))
```

## Usage

`M-x rom-party` - note `rom-party.el` requires some initial indexing when first run and whenever the custom variable `rom-party-word-sources` is updated.  In a rom party buffer the following keybindings are also provided:

| Key   | Description                           |
|-------|---------------------------------------|
| `C-/` | Hints solutions to the current prompt |
| `M-s` | Skips the current prompt              |

There is another command `rom-party-infinite` which is the same as above, but with no timer or lives.  To look at all preset configurations see `rom-party-choose-configuration`.

## Customization

If you're finding it too difficult (or too easy!), the best way to alter the difficulty is by modifying `rom-party-prompt-filter`.  The function takes a candidate prompt (e.g. `ggi`) and the words matching it - a `nil` value means don't use the prompt.  E.g.

```elisp
(setq rom-party-prompt-filter (lambda (prompt words) (>= (length words) 50)))
```

Means any prompts with less than 10 matching solutions are not shown.

Where `rom-party` gets words from can be customised using `rom-party-word-sources`.  Other useful custom variables are given below:

| Variable                     | Description                                             | Default                                      |
|------------------------------|---------------------------------------------------------|----------------------------------------------|
| `rom-party-config-directory` | Directory to store `rom-party` config and indexes       | `(concat user-emacs-directory "/rom-party")` |
| `rom-party-starting-lives`   | Starting number of lives                                | `2`                                          |
| `rom-party-timer-seconds`    | The number of starting seconds for the rom party timer. | `5`                                          |
