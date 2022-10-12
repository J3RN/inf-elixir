# inf-elixir
[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/inf-elixir-badge.svg)](https://melpa.org/#/inf-elixir)
[![MELPA Stable](https://stable.melpa.org/packages/inf-elixir-badge.svg)](https://stable.melpa.org/#/inf-elixir)
[![CircleCI status](https://circleci.com/gh/J3RN/inf-elixir.svg?style=svg)](https://app.circleci.com/pipelines/github/J3RN/inf-elixir)

This is a package to allow you to pop open and interact with Elixir REPL (IEx, presently).

## Features
- [x] Run IEx normally (`inf-elixir`)
- [x] Run IEx in a project setting (`inf-elixir-project`)
- [x] Ability to send commands to the inf buffer from a code file (`inf-elixir-send-line`, `inf-elixir-send-region`, `inf-elixir-send-buffer`)

## TODO
- [ ] Got a feature you want to see? Open an issue! :smile:

## Installation

`inf-elixir` is proudly distributed by [MELPA](https://melpa.org/) and [MELPA Stable](https://stable.melpa.org). I would also strongly encourage folks to use MELPA Stable, as the releases there will be more, well, stable. In order to install the package, follow [the Getting Started instructions on the MELPA website](https://melpa.org/#/getting-started) or [the Getting Started instructions on the MELPA Stable website](https://stable.melpa.org/#/getting-started).

Once your installation is configured to use MELPA, `inf-elixir` can be installed with `M-x package-install RET inf-elixir RET`.

[`use-package`](https://github.com/jwiegley/use-package) can be used to install and/or configure `inf-elixir`:

``` elisp
(use-package inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c i b" . 'inf-elixir-send-buffer)
         ("C-c i R" . 'inf-elixir-reload-module)))
```

## Keybindings

`inf-elixir` intentionally ships with no keybindings by default. This grants the user the freedom to specify whatever keybindings they would like. Functions that users would probably have an interest in binding are:
- `inf-elixir` :: Starts an IEx shell in a new buffer, or switches to an existing IEx shell buffer.
- `inf-elixir-project` :: Starts an IEx shell in the context of the project (by default by passing the `-S mix` argument to `iex`), or switches to an existing IEx shell buffer.
- `inf-elixir-send-line` :: Send the current line to the IEx shell buffer as input.
- `inf-elixir-send-region` :: Send the selected region to the IEx shell buffer as input.
- `inf-elixir-send-buffer` :: Send the entire current buffer to the IEx shell as input.
- `inf-elixir-reload-module` :: Reload modules from current buffer using `IEx.Helpers.r/1`.

An example of keybindings is included in the `use-package` declaration above.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg

## Development

I am not yet using any kind of build tool (like [Eldev](https://github.com/doublep/eldev) or [Cask](https://github.com/cask/cask)) to develop this plugin. Generally speaking, working with the code involves:
1. Clone the git repository
2. Make some changes
3. Load your changes with `M-x load-file RET inf-elixir.el RET`
4. Verify your changes worked
5. Send a PR :pray:

There are some tests written with [ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/index.html) that can be run with this command:

```
$ emacs -batch -l ert -l inf-elixir.el -l tests/inf-elixir-test.el -f ert-run-tests-batch-and-exit
```
