PullRequestNagger
=================

It sucks when your coworkers or collaborators don't review your work. Use
this tool to send them a friendly (or not so friendly, it's up to you) reminder
of the pull requests that you have open on a given repository.

Usage is easy: just build and run. The prompts are self-explanatory. You will
need your SMTP server details.

```shell
cabal install
# PullRequestNagger is in your cabal bin directory.
# That might not be in your PATH.
PullRequestNagger
```

The program will attempt to grab all open pull requests from the given
repository belonging to the given user, and then to send an email from the
given SMTP server using TLS and LOGIN authentication. It works with Google
mail, in which case the host is `smtp.googlemail.com` port `587`.

TODO
====

Currently one run of the program can nag only one email address. This is fine
if there is an email list that you can target, but really we ought to take in
multiple recipient addresses and target them all.
