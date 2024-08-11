# ZenICB

## What is ZenICB?

ZenICB is an ancient, poorly maintained Emacs mode for chatting on ICB
servers, which are themselves ancient and poorly maintained. Unless
you're one of maybe five people in the world, you are not interested
in using it.

ZenICB was originally created by Ben Mesander and Faried Nawaz, and
has barely seen any changes since the late 1990s. I (Perry Metzger)
happened to need it for something so I fixed a couple of bugs, cleaned
up the code, and put it up on GitHub for the benefit of the handful of
people who might care.

## What is (mostly was) ICB?

ICB was/is a really creaky old chat system, sort of like IRC only without
the peer to peer server component. You probably don't care about it.

Just in case you're morbidly curious, you can read [the Wikipedia entry](https://en.wikipedia.org/wiki/Internet_Citizen%27s_Band)

True masochists can read [a summary of the
protocol](http://www.icb.net/_jrudd/icb/protocol.html), a lightly
cleaned up copy of which is in `doc/protocol.html`.

## How do I use this?

Mostly, unless you know people with an ICB server and desperately want
to communicate that way, you shouldn't.

Documentation is basically non-existent, but in the very unlikely case
you actually want to use this with a live ICB server, you will want to
add the directory with the .el files onto your `load-path` in your
`.emacs` file.

```
  (setq load-path
        (cons (expand-file-name "/full/path/to/zenicb/directory") load-path))
```

You will also need to read `zenicb-example.el`, and you will
want to copy, customize, and load the contents of it into your Emacs
setup.

To try it out, run `emacs`, `M-x load-library zenicb`, and then `M-x zenicb`,
or some reasonable equivalent.

Yes, this should actually all be packaged up for `package.el`. Anyone
who wants to do that can submit a pull request, just as anyone who
wants to write real documentation could do that too. Except, there's
no user community so that's not likely to happen.

## What's the license?

GPLv2

## Miscellaneous

Known bugs may or may not be documented in the BUGS file. (Mostly
not.) To do items (which will never happen) are mostly not described
in the TODO file.
