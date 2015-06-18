
A collection of settings (and a little code) for making emacs open things in frames (i.e. real windows) instead of internal emacs-windows. This allows it to work properly with an external window manager.

There's a fairly rough screencast showing the kind of things you can do with this [here](https://www.youtube.com/watch?v=vi1BwRYJr6k).

Basic setup
-----

Clone the repository to a directory inside `~/.emacs.d` (or anywhere else in your load path) and add
```emacs
(load-file "~/.emacs.d/frames-only-mode/frames-only-mode.el")
(use-package frames-only-mode)
```
somewhere in your config files.


Setup and integrating with the windows manager
------------

Half the point of a tiling window manager is to be able to launch and kill windows easily and quickly, but what if we accidentally kill the last Emacs frame? If we are running Emacs in the normal way it will save and close down, then next time we launch it all the start up code has to execute again.

One way to handle this is to run Emacs in server (a.k.a. daemon) mode. This means you start an emacs server which stays running in the background and you spawn/kill instances of emacsclient as you work. As an additional benefit it means that all your emacs instances share the same buffers, history, settings, etc.

To run emacs in server mode simply add the following to your config files:
```emacs
;; Start emacs as a server
(server-start)
```


Now we need to create some new commands for launching emacsclients instead of emacs itself. First a bash alias:
```bash
alias emacs='emacsclient -c -n'
```
The `-c` creates a new frame and the `-n` detaches the client from the bash console. Handily if you run this command when no server exists it will create one before launching the client.

Finally we want a command which we can bind to a button in the window manager to launch a new emacsclient. For this I use a slightly modified version of the alias above (I use [xmonad](http://xmonad.org/) so this code actually just creates a haskell string which I later bind a key to execute as shell command):

```haskell
myEditor = "emacsclient -c -n -e '(switch-to-buffer nil)'"
```

You can see I've added an argument `-e '(switch-to-buffer nil)'`, this prevents the new client from opening the file named as an argument. Since we don't name any files for it to open the client would go to the scratch buffer by default, which is fairly useless. With this command it opens whichever buffer was most recently closed instead.


Emacs minibuffer completion
---------

Completion in the minibuffer can be problematic because, with the default
emacs settings, emacs will open a new frame to display completions. I
recommend using an alternative completion method such as [helm](http://tuhdo.github.io/helm-intro.html) or [ido](https://www.masteringemacs.org/article/introduction-to-ido-mode) (along with [smex](https://github.com/nonsequitur/smex) enable ido completion of M-x and [ido-ubiquitous](https://github.com/DarwinAwardWinner/ido-ubiquitous) to enable it in other places).

When using ido you should also set `ido-completion-buffer` to `nil` to
prevent the popup of a completions buffer (the buffer is unecessary because
possible completion are displayed in the minibuffer).

If you do use the default emacs completion (or ido with a popup completion
buffer) then `frames-only-mode-use-windows-for-completion` can be used to
control whether the `*Completions*` buffer is displayed in a frame or an
emacs window. The default is to use an emacs window, which works well
without changing any window manager settings. Alternatively the
`*Completions*` buffer can be disabled entirely by setting
`completion-auto-help` to `nil`.


Integrating with command line git
----------------

To make this work nicely with git (e.g. to pop up a new frame when we run git commit from the command line) we need to set the editor to run an emacsclient in a new frame. However, unlike the case for running emacsclient instances from the window manager, we don't want it to detach from the console. We can achieve this by adding the following to the `~/.gitconfig` file:

```
[core]
	editor = emacsclient -c
```

If instead we only used `editor = emacsclient` it would try to open a new buffer in an existing frame. This often results in the commit message buffer showing up in a different workspace(!), or in the terminal.

Rationale
-------

My main reason for disliking the built in emacs windows management is that it assumes everything you want to work with is inside Emacs. There's no support for tiling a chromium window or a terminal along with a few Emacs windows. This probably worked well when people did everything in a single terminal session, but not so much anymore.

Secondly if your main windows manager is also tiling then the two will conflict with each other in annoying ways. For example opening a new terminal emulator will completely wreck your nicely laid out Emacs windows.

Finally it means you need two separate sets of keys to do almost the same thing depending on whether you are inside or outside of Emacs.

The easiest solution to these problems is to never open multiple Emacs windows inside a single frame, that's the aim of this package.
