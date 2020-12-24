# Frames only mode

[![CI](https://github.com/davidshepherd7/frames-only-mode/workflows/CI/badge.svg)](https://github.com/davidshepherd7/frames-only-mode/actions) 
[![MELPA](https://melpa.org/packages/frames-only-mode-badge.svg)](https://melpa.org/#/frames-only-mode)

An emacs global minor mode to use emacs frames (i.e. operating system windows)
instead of emacs' internal windowing system. This combines particularly well
with
[tiling window managers](https://en.wikipedia.org/wiki/Tiling_window_manager)
such as [XMonad](http://xmonad.org/).

There's a fairly rough screencast showing the kind of things you can do with this [here](https://www.youtube.com/watch?v=vi1BwRYJr6k).


## Typical Setup

In combination with `frames-only-mode` I recommend:

* Running emacs as [server-client](http://wikemacs.org/wiki/Emacs_server).
* Binding a hotkey to open new emacs frames (see below).
* Using [helm](https://github.com/emacs-helm/helm),
  [ivy](https://github.com/abo-abo/swiper) or
  [ido](https://www.masteringemacs.org/article/introduction-to-ido-mode) instead
  of the default completion interface.


Bind this shell command to a hotkey to automatically open a useful buffer in
a new frame (usually your most recently viewed buffer which isn't currently open):

    emacsclient -c -n -e '(switch-to-buffer nil)'


## Changelog

### Unstable

* Add support for eshell completion
* Fix some customize group weirdness



## Advanced Configuration

### Integrating with command line git

To make this work nicely with git (e.g. to pop up a new frame when we run git commit from the command line) we need to set the editor to run an emacsclient in a new frame. However, unlike the case for running emacsclient instances from the window manager, we don't want it to detach from the console. We can achieve this by adding the following to the `~/.gitconfig` file:

```
[core]
    editor = emacsclient -c
```

If instead we only used `editor = emacsclient` it would try to open a new buffer in an existing frame. This often results in the commit message buffer showing up in a different workspace(!), or in the terminal.


### Integration with default emacs completion

If you use the default emacs completion (or ido with a popup completion buffer)
then `frames-only-mode-use-windows-for-completion` can be used to control
whether the `*Completions*` buffer is displayed in a frame or an emacs window.
The default is to use an emacs window, which works well with any window manager.
Alternatively the `*Completions*` buffer can be disabled entirely by setting
`completion-auto-help` to `nil`.


## Other links

This mode originated in
[a blog post](http://techtrickery.com/tearing-out-the-emacs-window-manager.html)
which has some additional details.
