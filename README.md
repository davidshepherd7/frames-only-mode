# Frames only mode

[![travis](https://travis-ci.org/davidshepherd7/frames-only-mode.svg?branch=master)](https://travis-ci.org/davidshepherd7/frames-only-mode) [![MELPA](https://melpa.org/packages/frames-only-mode-badge.svg)](https://melpa.org/#/frames-only-mode)

An emacs global minor mode to use emacs frames (i.e. operating system windows)
instead of emacs' internal windowing system. This combines particularly well
with
[tiling window managers](https://en.wikipedia.org/wiki/Tiling_window_manager)
such as [XMonad](http://xmonad.org/).

There's a fairly rough screencast showing the kind of things you can do with this [here](https://www.youtube.com/watch?v=vi1BwRYJr6k).


## Typical Setup

Install by dropping `frames-only-mode.el` into your load path, requiring it, and
running `(frames-only-mode)` to enable the (global) minor mode.

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


## Advanced Configuration

### Integration with virtual desktops under X11

By default when a frame is open on a hidden virtual desktop emacs will not
reopen the frame. `frames-only-mode` can try to detect when this is the case and
open a new frame anyway.

To enable this you need to install `wmctrl` (`sudo apt-get install wmctrl` on
Ubuntu) and set
`frames-only-mode-reopen-frames-from-hidden-x11-virtual-desktops` to `t`. (In the
future this may be enabled by default when running under X11 with `wmctrl`
installed, but it needs more testing first.)

If you use this feature with a window manager other than XMonad please let me
know if it works
[here](https://github.com/davidshepherd7/frames-only-mode/issues/2).

This feature is only applicable to X11 (I'm not sure what happens on Wayland/OSX/Windows).


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
