# inf-elixir

This is a package to allow you to pop open and interact with Elixir REPL (IEx, presently).

## Features
- [x] Run IEx normally (`inf-elixir`)
- [x] Run IEx in a project setting (`inf-elixir-project`)
- [x] Ability to send commands to the inf buffer from a code file (`inf-elixir-send-line`, `inf-elixir-send-region`, `inf-elixir-send-buffer`)

## TODO
- [ ] Got a feature you want to see? Open an issue! :smile:

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
