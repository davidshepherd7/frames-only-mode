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
* Binding a window manager hotkey to open new emacs frames (see below).
* Rebinding any window splitting keys that you use to call `make-frame` instead.


Bind this shell command to a hotkey to automatically open a useful buffer in
a new frame (usually your most recently viewed buffer which isn't currently open):

    emacsclient -c -n -e '(switch-to-buffer nil)'


## Advanced Configuration

### Integrating with command line git

To make this work nicely with git (e.g. to pop up a new frame when we run git commit from the command line) we need to set the editor to run an emacsclient in a new frame. However, unlike the case for running emacsclient instances from the window manager, we don't want it to detach from the console. We can achieve this by adding the following to the `~/.gitconfig` file:

```
[core]
    editor = emacsclient -c
```

If instead we only used `editor = emacsclient` it would try to open a new buffer in an existing frame. This often results in the commit message buffer showing up in a different workspace(!), or in the terminal.


### Automatically killing (closing) certain frames

Sometimes when a buffer is closed/buried you want to kill the containing frame,
for example when pressing `q` in a help buffer. For things in Emacs itself this
is usually configured by default, but for other things you can control this
behaviour using `frames-only-mode-kill-frame-when-buffer-killed-buffer-list`.

For example to automatically kill frames for buffers named `*foo*`:

```
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*foo*")
```

This variable also supports using regular expressions by using a cons cell where
the car (first) element is `'regexp` and the cdr (second) is the regexp. This is a
little tricky because transient buffers are typically named with `*` characters
at the start and end of their names which need escaping. For example to
automatically kill frames for popup buffers from po-mode which are named like
`*foo.po*` do:

```
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list '(regexp . "\\*.*\\.po\\*"))
```

(note the double-backslash escaping of the regexp special character here *
because we want to treat it as a normal literal character).


### Integration with default emacs completion

If you use the default emacs completion (or ido with a popup completion buffer)
then `frames-only-mode-use-windows-for-completion` can be used to control
whether the `*Completions*` buffer is displayed in a frame or an emacs window.
The default is to use an emacs window, which works well with any window manager.
Alternatively the `*Completions*` buffer can be disabled entirely by setting
`completion-auto-help` to `nil`.


## Changelog

### Unstable

* Add support for eshell completion
* Fix some customize group weirdness
* Fix sometimes leaving additional frames open after quitting magit
* Regexp support in kill-frame-when-buffer-killed-buffer-list



## Other links

This mode originated in
[a blog post](http://techtrickery.com/tearing-out-the-emacs-window-manager.html)
which has some additional details.
