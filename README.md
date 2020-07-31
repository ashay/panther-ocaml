# Panther #

A utility program that wraps an existing editor to read and write encrypted
files.

## Rationale ##

I like to keep a journal, but I am paranoid about writing my private thoughts,
regardless of the medium.  If it's paper, I worry that the papers could be
read by guests visiting my place.  If I'm storing my notes on a computer, I
dislike that every program I run has the same permissions as me, so programs
that I install for experimental use can siphon those files.  I avoid storing
these notes on some cloud provider for the same reason.

As a workaround, I wrote this program, which lets me temporarily decrypt
files, edit them, and save the encrypted contents.

## How Panther Works ##

At its core, Panther uses the AES CBC implementation from
[Cryptokit](https://github.com/xavierleroy/cryptokit) to temporarily decrypt
files into tmpfs and writing the (re-encrypted) contents back to the original
file.  Panther uses the platform-specific notify mechanism (from
[fswatch](https://github.com/kandu/ocaml-fswatch/)) to update the backing
store whenever the file in tmpfs is updated.

## WARNING ##

Since `inotify` on Linux uses file paths instead of file descriptors, Panther
cannot is forced to retain the decrypted file on disk while it is being
edited.  This implies that any program monitoring `/tmp` can read the
decrypted files.

This attack vector can be mitigated by using file descriptors for monitoring
the file for changes, but I need to implement this myself.  I have a prototype
implementation that opens the decrypted file for reading, then _deletes_ the
file to remove its inode entry, while using the file descriptor to
periodically read and monitor the file for changes.
