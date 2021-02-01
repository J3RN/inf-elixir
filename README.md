# inf-elixir

This is a package to allow you to pop open and interact with Elixir REPL (IEx, presently).

## Features
- [x] Run IEx normally (`inf-elixir`)
- [x] Run IEx in a project setting (`inf-elixir-project`)
- [x] Ability to send commands to the inf buffer from a code file (`inf-elixir-send-line`, `inf-elixir-send-region`, `inf-elixir-send-buffer`)

## TODO
- [ ] Got a feature you want to see? Open an issue! :smile:

## Keybindings

`inf-elixir` intentionally ships with no keybindings by default. This is grant the user the freedom to specify whatever keybindings they would like. Functions that users would probably have an interest in binding are:
- `inf-elixir` :: Starts an IEx shell in a new buffer, or switches to an existing IEx shell buffer.
- `inf-elixir-project` :: Starts an IEx shell in the context of the project (by default by passing the `-S mix` argument to `iex`).
- `inf-elixir-send-line` :: Send the current line to the IEx shell buffer as input.
- `inf-elixir-send-region` :: Send the selected region to the IEx shell buffer as input.
- `inf-elixir-send-buffer` :: Send the entire current buffer to the IEx shell as input.

An example of some keybindings is included below.

## Example configuration (using `use-package`)

- create a subdir `packages` in your `.emacs.d` directory
- cd into `~/.emacs.d/packages` and run `git submodule add <this repo>`
- put the following into your `init.el` file (customize the bindings do your liking) ...

``` elisp
(use-package inf-elixir
  :load-path "packages/inf-elixir"
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c i b" . 'inf-elixir-send-buffer)))
```
