# Changelog

## [0.9.0](https://github.com/liuyinz/git-cliff.el/compare/v0.8.0..v0.9.0) - 2024-12-19

### Features

- support option --from-context - ([ca71436](https://github.com/liuyinz/git-cliff.el/commit/ca71436f8cdbdd37d07f91c94231234d3fc9c8b4))
- support option --use-branch-tags - ([ca8b9f6](https://github.com/liuyinz/git-cliff.el/commit/ca8b9f61abc0279a045e10ddb3aaa6f1214dcdfd))

## [0.8.0](https://github.com/liuyinz/git-cliff.el/compare/v0.7.0..v0.8.0) - 2024-07-30

### Bug Fixes

- option -o and -p could coexist now - ([3b7f220](https://github.com/liuyinz/git-cliff.el/commit/3b7f2203e1f022cc2866c9611e5d93b9389881cc))

### Features

- add option of --ignore-tags - ([0faea29](https://github.com/liuyinz/git-cliff.el/commit/0faea2967b36ab378b9b727c42ce1ada7d11add6))

## [0.7.0](https://github.com/liuyinz/git-cliff.el/compare/v0.6.0..v0.7.0) - 2024-05-14

### Documentation

- add donate link - ([6730f40](https://github.com/liuyinz/git-cliff.el/commit/6730f4064e694f6ada2522bf2a83ea7a7fb149d2))

### Miscellaneous Chores

- **(dependency)** add dash library as dependency - ([d36a094](https://github.com/liuyinz/git-cliff.el/commit/d36a09423dc69ae0bba99f4141ff2633c4fe6353))
- **(deps)** update transient version to v0.6.0 - ([813f856](https://github.com/liuyinz/git-cliff.el/commit/813f8569a784352c33200229aa0faafe269e4f16))

## [0.6.0](https://github.com/liuyinz/git-cliff.el/compare/v0.5.0..v0.6.0) - 2024-03-31

### Bug Fixes

- support option --tag-pattern - ([95b2983](https://github.com/liuyinz/git-cliff.el/commit/95b298305877d260f18fa1403c1efd68448ca2e8))

### Features

- **(config)** support project manifest in Cargo.toml - ([af38ef1](https://github.com/liuyinz/git-cliff.el/commit/af38ef18d3c32c9d1b1dbf47422a2f370ea7e6fc))

## [0.5.0](https://github.com/liuyinz/git-cliff.el/compare/v0.4.5..v0.5.0) - 2024-02-20

### Documentation

- update latest dependency and FAQ - ([6dadead](https://github.com/liuyinz/git-cliff.el/commit/6dadeadf45c476aa4fa61d42d11a229b21577fc9))

### Features

- add option git-cliff-executable and show info about it in menu - ([400d4c5](https://github.com/liuyinz/git-cliff.el/commit/400d4c524cd9126996c13434af4b8ef1311c2cc8))
- update git-cliff menu to compatible with latest version - ([0783d4f](https://github.com/liuyinz/git-cliff.el/commit/0783d4f08a9ff040c36645021de8c24bf6c25e46))

### Miscellaneous Chores

- bump copyright years - ([1949b53](https://github.com/liuyinz/git-cliff.el/commit/1949b536a259b8da8722b5fd064b89fb163f976b))

## [0.4.5](https://github.com/liuyinz/git-cliff.el/compare/v0.4.4..v0.4.5) - 2023-12-26

### Bug Fixes

- change short argument to -x for --context - ([122c249](https://github.com/liuyinz/git-cliff.el/commit/122c2499caed7f44c023f41a3970fb81a8173fdd))
- use built-in face link-visited instead of deprecated transient-pink - ([fc58980](https://github.com/liuyinz/git-cliff.el/commit/fc589803a88edba9339a8b884818119f52b23e80))

### Miscellaneous Chores

- **(dependency)** require emacs version >= 29.1 - ([c5b107d](https://github.com/liuyinz/git-cliff.el/commit/c5b107d84d6ee7a7447f197426fee50ee123ef61))

## [0.4.4](https://github.com/liuyinz/git-cliff.el/compare/v0.4.3..v0.4.4) - 2023-12-08

### Bug Fixes

- set right value type for defcustoms - ([c24a46c](https://github.com/liuyinz/git-cliff.el/commit/c24a46cdf39a2be00c946184611551d4ca75512c))
- autoload command git-cliff-menu correctly - ([6bd365c](https://github.com/liuyinz/git-cliff.el/commit/6bd365c883f2ec68589818fdac6925185b8b2330))

### Features

- support --bump flag in git-cliff v1.4.0 - ([b6a8c67](https://github.com/liuyinz/git-cliff.el/commit/b6a8c67c8f7468554401f1c6fe0d000d3987c13b))

### Miscellaneous Chores

- **(dependency)** update git-cliff version >=1.4.0 - ([1bc9a48](https://github.com/liuyinz/git-cliff.el/commit/1bc9a48de2fa11be3c7474c606a509e28a9a4dc9))
- **(dependency)** update transient version >=0.5.0 - ([b8244d2](https://github.com/liuyinz/git-cliff.el/commit/b8244d21136d5831e69724294099e298c962d41d))

## [0.4.3](https://github.com/liuyinz/git-cliff.el/compare/v0.4.2..v0.4.3) - 2023-10-16

### Bug Fixes

- close transient panel after choose preset - ([aa5f870](https://github.com/liuyinz/git-cliff.el/commit/aa5f8709e10c5864e3408489a34fafd20874063c))
- provide v0.1.0 as first tag if no tag exist in repository - ([f05c042](https://github.com/liuyinz/git-cliff.el/commit/f05c0426cf4c3554d7c3d391e5dfe5ef01976a99))
- do not open new window when changelog is already in display - ([0bfe02a](https://github.com/liuyinz/git-cliff.el/commit/0bfe02ad661aa2891899f9f264b8de3072defb41))

### Documentation

- **(README)** update todo list - ([551ac47](https://github.com/liuyinz/git-cliff.el/commit/551ac47f16f28e4f66afeb9af9b9198e09533ba8))

## [0.4.2](https://github.com/liuyinz/git-cliff.el/compare/v0.4.1..v0.4.2) - 2023-10-08

### Documentation

- **(README)** update customization and todos - ([0843ecd](https://github.com/liuyinz/git-cliff.el/commit/0843ecdb1f8762cb11764ef440067f08b363319c))
- **(README)** add FAQ section - ([8849fe3](https://github.com/liuyinz/git-cliff.el/commit/8849fe38ed34ebeaf72eef03e21ee94d8ffe19df))
- **(README)** update install info with melpa - ([d7f326f](https://github.com/liuyinz/git-cliff.el/commit/d7f326f081a2806826c7301063a29423b66aec28))

### Miscellaneous Chores

- **(test)** fix minor typos - ([281fe65](https://github.com/liuyinz/git-cliff.el/commit/281fe65c0d0fac2ce6e6261f1f419a2396276908))

## [0.4.1](https://github.com/liuyinz/git-cliff.el/compare/v0.4.0..v0.4.1) - 2023-10-04

### Bug Fixes

- **(git-cliff--edit-config)** couldn't find config error - ([e7fc71a](https://github.com/liuyinz/git-cliff.el/commit/e7fc71a316b170c358b2b17b0936c1ef99218d6e))

### Documentation

- update screenshots - ([55be427](https://github.com/liuyinz/git-cliff.el/commit/55be427986d956d82a8c33e11c0facd99c4a64c4))
- fix wrong url link - ([d5c4c62](https://github.com/liuyinz/git-cliff.el/commit/d5c4c62b47a1d6ed1494b92f3688623e9041f216))

### Features

- **(example)** add default config and template - ([42b44be](https://github.com/liuyinz/git-cliff.el/commit/42b44bec1ea398ce37c01b516f85fac7163b8273))

### Miscellaneous Chores

- **(dependency)** update dependency for emacs >=27.1 - ([93ebbf0](https://github.com/liuyinz/git-cliff.el/commit/93ebbf0ae180063e5808bb794f7c6436373aa058))

## [0.4.0](https://github.com/liuyinz/git-cliff.el/compare/v0.3.2..v0.4.0) - 2023-10-04

### Miscellaneous Chores

- **(changelog)** remove whitespace - ([c30a094](https://github.com/liuyinz/git-cliff.el/commit/c30a094f630424de19763f71f14a17fba9c84065))

### Refactoring

- **(menu)** [**breaking**] remove --repository and git-cliff cache mechanism - ([8347e26](https://github.com/liuyinz/git-cliff.el/commit/8347e26efafeb1387f5fb69bff20f574a4944ce1))

## [0.3.2](https://github.com/liuyinz/git-cliff.el/compare/v0.3.1..v0.3.2) - 2023-10-01

### Bug Fixes

- display changelog after release new version - ([e8cedbb](https://github.com/liuyinz/git-cliff.el/commit/e8cedbb592fc3932f9bfb17fa32d63516be267d1))

## [0.3.1](https://github.com/liuyinz/git-cliff.el/compare/v0.3.0..v0.3.1) - 2023-10-01

### Bug Fixes

- exclude git-cliff--* commands in M-x completion - ([7b380a6](https://github.com/liuyinz/git-cliff.el/commit/7b380a61ed2fb7272f83980c9137aa1980562252))
- get active infix value correctly - ([cbe9388](https://github.com/liuyinz/git-cliff.el/commit/cbe9388189461e652770aa789a97851d26a24b5b))
- error when update git-cliff-templates - ([7702302](https://github.com/liuyinz/git-cliff.el/commit/770230205877cd574548e12af36edeb5aca97be5))
- update git-cliff-menu when reset values - ([250c40d](https://github.com/liuyinz/git-cliff.el/commit/250c40d59a5fd55112db63e83d71d6a8e4397f5b))

## [0.3.0](https://github.com/liuyinz/git-cliff.el/compare/v0.2.0..v0.3.0) - 2023-09-29

### Features

- **(tag)** support bumped tags to select - ([cb41871](https://github.com/liuyinz/git-cliff.el/commit/cb4187198db0f4a30f3355e53c9d78110eb56ecd))
- support cache for git-cliff-menu per project - ([69a82ba](https://github.com/liuyinz/git-cliff.el/commit/69a82ba0fb449c13325df80611acf3e606221166))
- add command git-cliff--release - ([8173c43](https://github.com/liuyinz/git-cliff.el/commit/8173c43e271377039f4dfed727f690fbf106f98a))

## [0.2.0](https://github.com/liuyinz/git-cliff.el/compare/v0.1.0..v0.2.0) - 2023-09-20

### Bug Fixes

- **(status)** show status info correctly - ([d515263](https://github.com/liuyinz/git-cliff.el/commit/d5152634abfa0ca5a7708f4b0121ff4103583bff))

### Features

- add var git-cliff-version - ([3d7e338](https://github.com/liuyinz/git-cliff.el/commit/3d7e338cc5808cd18e4e6f06cccee8114a00bf94))
- support range feature - ([8844fe3](https://github.com/liuyinz/git-cliff.el/commit/8844fe3e1fc869e8f931c9bfaa48334bc95b2236))

## [0.1.0] - 2023-09-20

### Bug Fixes

- **(config)** update config var when add config file in repo - ([99d63b0](https://github.com/liuyinz/git-cliff.el/commit/99d63b09be072497222b2faae9305ed04aabe9a2))
- **(preview)** erase content before preview new output - ([4341899](https://github.com/liuyinz/git-cliff.el/commit/4341899fd9553f9c26ea6db5ff4050a293e86af1))
- get text property wrongly from completing-read return value - ([ed10163](https://github.com/liuyinz/git-cliff.el/commit/ed101632e6e3045c3a08287834762a26ffb487fe))
- git-cliff--choose-template completion sorting error - ([e81564c](https://github.com/liuyinz/git-cliff.el/commit/e81564cc703046f4059b2f97c5074474c8426490))
- git-cliff--configs return wrong value - ([71f0e76](https://github.com/liuyinz/git-cliff.el/commit/71f0e76fc2998def11be2736022a075d46d6ced5))
- do not set prepending as init state - ([1d95a09](https://github.com/liuyinz/git-cliff.el/commit/1d95a095c43bfa3ea55348e3257d2be80693aa6a))
- use repo dir instead of workdir as default-directory - ([b2d40d9](https://github.com/liuyinz/git-cliff.el/commit/b2d40d9421c1f1ab347ac908ff71834353874471))
- IO error path not found when args string contain --<option>=~ - ([877f3cb](https://github.com/liuyinz/git-cliff.el/commit/877f3cbcef90aa809135f9c12edb13005f2efcea))
- use defconst to define git-cliff-config-regexp - ([f74cebb](https://github.com/liuyinz/git-cliff.el/commit/f74cebb618f710b5686d03228ecc7f0f0462ae77))
- set correct name for shell-command-buffer - ([0371b6d](https://github.com/liuyinz/git-cliff.el/commit/0371b6d8e396813d6a4919bbbb63adee59c23010))
- switch to target buffer after call git-cliff--run - ([817b640](https://github.com/liuyinz/git-cliff.el/commit/817b640d017fa8286f599f0f4c539e3fab445586))

### Documentation

- **(README)** update todo list - ([4a6775c](https://github.com/liuyinz/git-cliff.el/commit/4a6775c0644a89bfa0d75c06a5744d0caef8e7b1))
- add README.md - ([9b84b29](https://github.com/liuyinz/git-cliff.el/commit/9b84b29d1aec5db299157cfc17c21babeb5fa915))

### Features

- **(menu)** provide repo infomations in transient menu - ([d445f2d](https://github.com/liuyinz/git-cliff.el/commit/d445f2dd728a6247c3e7aa4cd1126ffb74c62721))
- **(preset)** add configs from git-cliff/examples - ([2bf85c3](https://github.com/liuyinz/git-cliff.el/commit/2bf85c30f347282bb949f9114f7174c04a585fd8))
- **(transient)** add git-cliff-menu - ([fa6e0c5](https://github.com/liuyinz/git-cliff.el/commit/fa6e0c5b313b4b6deeaa00fef10082eb2927505e))
- add related suffixes - ([1fd8383](https://github.com/liuyinz/git-cliff.el/commit/1fd83835b0911bfb94729fd03aeddd3725e6a614))
- set default value for changelog reader - ([c756131](https://github.com/liuyinz/git-cliff.el/commit/c7561318faa9e96735d1b5eb15fe36c68ccac241))
- add option --repository - ([4fa4b10](https://github.com/liuyinz/git-cliff.el/commit/4fa4b103b883e2d994f40edfb69e80c4d2dfef77))
- extract body templates to directory examples - ([86be222](https://github.com/liuyinz/git-cliff.el/commit/86be22296a5692253389ee2fa7ba6613147ed221))
- support --body option - ([926723c](https://github.com/liuyinz/git-cliff.el/commit/926723cd840deee766bb834e8ddaf62eb936f8d2))

### Miscellaneous Chores

- **(changelog)** update cliff config - ([2b00e13](https://github.com/liuyinz/git-cliff.el/commit/2b00e1355a66d897bbd59563d861de237fa69e60))
- **(ci)** update actions/checkout - ([d30e755](https://github.com/liuyinz/git-cliff.el/commit/d30e755edb9cbdd40048483a2db7b5a2fd9ad581))
- **(gitignore)** ignore autoloads and tmp file - ([c0b7ad5](https://github.com/liuyinz/git-cliff.el/commit/c0b7ad52f9811868e35e39b305416554bc554069))
- **(init)** initial commit - ([a75fcaa](https://github.com/liuyinz/git-cliff.el/commit/a75fcaa17e2983e3abce406da7415f9c4075378f))
- add cliff.toml for generate changelog - ([435b3c1](https://github.com/liuyinz/git-cliff.el/commit/435b3c1c60c961f26b0147824d92b7e8723bacfc))

<!-- generated by git-cliff -->
