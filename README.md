[![Min Emacs Version](https://img.shields.io/badge/Emacs-28+-7F5AB6?logo=gnu-emacs)](https://www.gnu.org/software/emacs/)

# rom-party.el

[:bomb: Party](https://jklm.fun/)... in Emacs.

## Installation

```elisp
(use-package rom-party
  :config
  (setq rom-party-timer-seconds 10
        ;; `rom-party-config-directory' is used to store word files and index files
        rom-party-config-directory "my/custom/dir"))
```

## Usage

`M-x rom-party` - note `rom-party.el` requires some initial indexing when first run and whenever the custom variable `rom-party-word-sources` is updated.

For other customization options, see `M-x customize`.
