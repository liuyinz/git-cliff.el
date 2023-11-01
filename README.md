# git-cliff

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA](http://melpa.org/packages/git-cliff-badge.svg)](http://melpa.org/#/git-cliff)

Generate and update changelog using [git-cliff][git-cliff].

This package provides the interface of [git-cliff][git-cliff], built in transient, to
generate and update changelog for project. Call `git-cliff-menu` to start.

<!-- markdown-toc start -->

## Contents

- [git-cliff](#git-cliff)
  - [Screenshot](#screenshot)
  - [Install](#install)
    - [dependencies](#dependencies)
    - [package](#package)
  - [Usage](#usage)
  - [Customization](#customization)
  - [Todo](#todo)
  - [FAQ](#faq)
  - [License](#license)

<!-- markdown-toc end -->

## Screenshot

- Call `M-x git-cliff-menu`

![git-cliff-menu](image/git-cliff-menu.jpg)

## Install

### dependencies

- [git-cliff][git-cliff], version >= 1.4.0
- emacs, version >= 27.1
- transient

### package

- Manually

Clone and add to `load-path`, require the package.

- Melpa

This package is available on [MELPA]. Install with `M-x package-install` `RET` `git-cliff` within Emacs.

## Usage

```elisp
;; Directly
(require 'git-cliff)

;; OPTIONAL
;; Integrate to `magit-tag'
(with-eval-after-load 'magit-tag
  (transient-append-suffix 'magit-tag
    '(1 0 -1)
    '("c" "changelog" git-cliff-menu)))
```

- call `git-cliff-menu`

## Customization

- `git-cliff-enable-examples` : If non-nil, enable examples when choose presets and templates.

- `git-cliff-extra-path` : directory storing user defined [presets](https://git-cliff.org/docs/configuration/) and templates.

## Todo

- [x] support range arguments
- [ ] support .org format

## FAQ

- no `--repository` and `--workdir` options support?

  Actually in the earlier versions, both options are supported. However, `git-cliff-menu` is designed to run in the root dir 
  of current repository by force, which means equivalent to `--workdir . --repository .` always. Due to same reason, multi-value of 
  `--repository` is disabled either in `git-cliff.el`.

## License

See [LICENSE](LICENSE).

[melpa]: http://melpa.org/#/git-cliff
[git-cliff]: https://github.com/orhun/git-cliff
