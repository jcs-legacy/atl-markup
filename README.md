[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/atl-markup-badge.svg)](https://melpa.org/#/atl-markup)
[![MELPA Stable](https://stable.melpa.org/packages/atl-markup-badge.svg)](https://stable.melpa.org/#/atl-markup)

# atl-markup
> Automatically truncate lines for markup languages.

[![CI](https://github.com/jcs-elpa/atl-markup/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/atl-markup/actions/workflows/test.yml)

<p align="center">
  <img src="./etc/demo.gif" width="600" height="349"/>
</p>

## Usage

You can enable this package by this following command.

```
M-x atl-markup-mode
```

I will prefer customize it with few mode hooks. For example, 

```el
(add-hook 'web-mode-hook (lambda () (atl-markup-mode 1)))
```

I will also recommend to apply this mode to all markup languages like `nxml-mode`,
`xml-mode`, `html-mode`, etc.

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either 
clone and make pull requests to this repository. Or you can 
clone the project and establish your own branch of this tool. 
Any methods are welcome!
