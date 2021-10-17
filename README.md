# org-link-colorize

>

## Overview

## Usage

```elisp
(defcustom org-link-colorize-exclude-modes '(org-agenda-mode)
  "A list of excluded major modes which wouldn't enable `org-link-colorize'."
  :type 'list
  :safe #'listp
  :group 'org-link-colorize)

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

## Possible future improvements

## Available for Hire

I'm available for freelance, contracts, and consulting both remotely and in the Hudson Valley, NY (USA) area. [Some more about me](https://www.zweisolutions.com/about.html) and [what I can do for you](https://www.zweisolutions.com/services.html).

Feel free to drop me a message at:

```
hi [a+] zweisolutions {●} com
```

## License

TODO: Note forked.
