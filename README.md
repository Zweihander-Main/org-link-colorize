# org-link-colorize

> Change org link color based on link type

## Overview

Want to tell the difference between `file:` and `https:` links in Org? Using `org-roam` and want to give `id:` links their own color? This package allows you to do just that!

Forked from stardiviner's [org-link-beautify](https://github.com/stardiviner/org-link-beautify) -- check it out if you want something a bit more substantial than just colors.

## Usage

### Install

Add `org-link-colorize.el` into your load path.

Doom users can try something like the following:

```elisp
(package! org-link-colorize
  :recipe '(:host github :repo "Zweihander-Main/org-link-colorize"))
```

### Usage

```elisp
(org-link-colorize 1)
```

Doom users can try something like the following:

```elisp
(use-package! org-link-colorize
  :after org
  :init
  (add-hook! 'org-mode-hook 'org-link-colorize-mode))
```

### Customization

```elisp
(defcustom org-link-colorize-link-colors '(("file" . "orange")
                                           ("attachment" . "orange")
                                           ("pdf" . "orange")
                                           ("http" . "blue")
                                           ("https" . "blue")
                                           ("ftp" . "blue")
                                           ("telnet" . "blue")
                                           ("rss" . "blue")
                                           ("elfeed" . "blue")
                                           ("wikipedia" . "blue")
                                           ("mailto" . "blue")
                                           ("irc" . "blue")
                                           ("doi" . "blue")
                                           ("id" . "green"))
  "Associations between link type and foreground color desired.
Should be a list of cons cells with the link type (excluding the colon) as the
CAR and the foreground color as the CDR. Foreground color can be obtained using
`M-x list-colors-display'."
  :type '(repeat (cons :tag ""
                       (string :tag "File type") (string :tag "Foreground color")))
  :group 'org-link-colorize)
```

```elisp
(defcustom org-link-colorize-exclude-modes '(org-agenda-mode)
  "A list of excluded major modes which wouldn't enable `org-link-colorize'."
  :type 'list
  :safe #'listp
  :group 'org-link-colorize)
```

## Available for Hire

I'm available for freelance, contracts, and consulting both remotely and in the Hudson Valley, NY (USA) area. [Some more about me](https://www.zweisolutions.com/about.html) and [what I can do for you](https://www.zweisolutions.com/services.html).

Feel free to drop me a message at:

```
hi [a+] zweisolutions {●} com
```

## License

Forked from [org-link-beautify](https://github.com/stardiviner/org-link-beautify).

[GPLv3](./LICENSE)

    org-link-colorize
    Copyright (C) stardiviner
    Copyright (C) 2021 Zweihänder
    This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
    This is free software, and you are welcome to redistribute it
    under certain conditions; type `show c' for details.
