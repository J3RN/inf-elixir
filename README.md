# inf-elixir
[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/inf-elixir-badge.svg)](https://melpa.org/#/inf-elixir)

This is a package to allow you to pop open and interact with Elixir REPL (IEx, presently).

## Features
- [x] Run IEx normally (`inf-elixir`)
- [x] Run IEx in a project setting (`inf-elixir-project`)
- [x] Ability to send commands to the inf buffer from a code file (`inf-elixir-send-line`, `inf-elixir-send-region`, `inf-elixir-send-buffer`)

## TODO
- [ ] Got a feature you want to see? Open an issue! :smile:

## Keybindings

`inf-elixir` intentionally ships with no keybindings by default. This grants the user the freedom to specify whatever keybindings they would like. Functions that users would probably have an interest in binding are:
- `inf-elixir` :: Starts an IEx shell in a new buffer, or switches to an existing IEx shell buffer.
- `inf-elixir-project` :: Starts an IEx shell in the context of the project (by default by passing the `-S mix` argument to `iex`), or switches to an existing IEx shell buffer.
- `inf-elixir-send-line` :: Send the current line to the IEx shell buffer as input.
- `inf-elixir-send-region` :: Send the selected region to the IEx shell buffer as input.
- `inf-elixir-send-buffer` :: Send the entire current buffer to the IEx shell as input.

An example of some keybindings is included below.

## Installation

`inf-elixir` is proudly distributed by [MELPA](https://melpa.org/). In order to use packages from MELPA, follow [the Getting Started instructions on the MELPA website](https://melpa.org/#/getting-started).

Once your installation is configured to use MELPA, `inf-elixir` can be installed with `M-x package-install RET inf-elixir RET`.

[`use-package`](https://github.com/jwiegley/use-package) can be used to install and/or configure `inf-elixir`:

``` elisp
(use-package inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c i b" . 'inf-elixir-send-buffer)))
```

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
