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
  - [Donate](#donate)

<!-- markdown-toc end -->

## Screenshot

- Call `M-x git-cliff-menu`

![git-cliff-menu](image/git-cliff-menu.jpg)

## Install

### dependencies

- [git-cliff][git-cliff], version >= 2.2
- emacs, version >= 29.1
- transient >= 0.5.0
- [dash](https://github.com/magnars/dash.el), version >= 2.19.1

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

- `git-cliff-extra-path` : directory storing user defined [presets](https://git-cliff.org/docs/configuration/) and templates.

## Todo

- [x] support range arguments
- [ ] support .org format

## FAQ

- no `--repository` and `--workdir` options support?

  Actually in the earlier versions, both options are supported. However, `git-cliff-menu` is designed to run in the root dir 
  of current repository by force, which means equivalent to `--workdir . --repository .` always. Due to same reason, multi-value of 
  `--repository` is disabled either in `git-cliff.el`.

- no `--body` option support?

  Almost useless option, please use `--init` instead

## Donate

If you think the it's helpful for you, please consider paying a cup of coffee
for me. Thank you! :smile:

<a href="https://paypal.me/liuyinz" target="_blank">
<img
src="https://www.paypalobjects.com/digitalassets/c/website/marketing/apac/C2/logos-buttons/optimize/44_Grey_PayPal_Pill_Button.png"
alt="PayPal" width="120" />
</a>

[melpa]: http://melpa.org/#/git-cliff
[git-cliff]: https://github.com/orhun/git-cliff
