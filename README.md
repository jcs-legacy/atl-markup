[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/atl-markup-badge.svg)](https://melpa.org/#/atl-markup)
[![MELPA Stable](https://stable.melpa.org/packages/atl-markup-badge.svg)](https://stable.melpa.org/#/atl-markup)
[![CI](https://github.com/jcs-elpa/atl-markup/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/atl-markup/actions/workflows/test.yml)

# atl-markup
> Automatically truncate lines for markup languages.

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

## Contribution

If you would like to contribute to this project, you may either 
clone and make pull requests to this repository. Or you can 
clone the project and establish your own branch of this tool. 
Any methods are welcome!
