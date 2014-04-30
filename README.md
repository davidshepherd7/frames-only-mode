
A collection of settings (and a little code) for making emacs open things in frames (i.e. real windows) instead of internal emacs-windows. This allows it to work properly with an external window manager.

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



Rationale
-------

My main reason for disliking the built in emacs windows management is that it assumes everything you want to work with is inside Emacs. There's no support for tiling a chromium window or a terminal along with a few Emacs windows. This probably worked well when people did everything in a single terminal session, but not so much anymore.

Secondly if your main windows manager is also tiling then the two will conflict with each other in annoying ways. For example opening a new terminal emulator will completely wreck your nicely laid out Emacs windows.

Finally it means you need two separate sets of keys to do almost the same thing depending on whether you are inside or outside of Emacs.

The easiest solution to these problems is to never open multiple Emacs windows inside a single frame, that's the aim of this package.
