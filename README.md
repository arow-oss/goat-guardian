# Goat Guardian

Goat Guardian is a reverse-proxy authentication server.  You can run it in
front of an existing web application.  It will handle all authentication,
including OAuth and email-based login.

Goat guardian is meant to make it easy to add support for authentication to
your web application.  It works great for demos, prototypes, internal web
applications, rapid development with a small team.  It even works well for
larger applications and teams that want an easy way to implement multiple forms
of authentication!

## Introduction

Goat Guardian is a reverse-proxy that runs in front of your web application.
It proxies requests to your web application.  It is similar to Apache or nginx.

Here is a simple network diagram of what it looks like to use Goat Guardian:

![Goat Guardian Simple Network Diagram](./img/goat-guardian-network-no-db.png)

Goat Guardian received requests from the end-user.  If it is a request to
authenticate, Goat Guardian will directly handle it.  If it is any other
request, it will be forwarded to your web application.

## Language

In this document, *end-user* will be used to refer to to the user of the web
application.  In the diagram above, it is the blue guy all the way to the left.

*Operator* will be used to refer to the person who is setting up and using Goat
Guardian.  In most cases, this will be you, the person reading this document.

*Upstream* or *upstream web app* will be used to refer to the web application
that runs behind Goat Guardian.  In the diagram above, this refers to the web
application all the way to right.  In general, this should be an application that
wants to authenticate users, but doesn't currently have any way to do
authentication.

## Quick Start

First, you must download and build Goat Guardian.  You must have `git` and
[`stack`](https://docs.haskellstack.org/en/latest/README/) installed to be able
to do this:

```sh
$ git clone https://github.com/arow-oss/goat-guardian.git
$ cd goat-guardian
$ stack install
```

This should build and install Goat Guardian as a binary in
`~/.local/bin/goat-guardian`.

You can run it by running the binary, but there are some arguments you must
specify either on the command line or as environment variables.

Here is an example of running `goat-guardian` with all the required arguments:

```sh
$ goat-guardian \
    --twitter-oauth-key Egex7CUsqQqSoEDjMivGtT1r0 \
    --twitter-oauth-secret HCZPPaOmib64GP7QbQaEwrLwKswK8pQDe4UwsAS3EVJBupBj5l  \
    --session-key DORwMBc0sSvGvIDutozVyNnJwU7qTknHkqFWUhpoAElJruHWT0GH7qmTpKajqIxbkuyTN5M5mb9CuM5JECg7SKadylr1QMeZqo1yPexd07KEdMKCbqdxJBmgHoTbBLL8 \
    --redir-after-login-url http://localhost:3000/after-login \
    --sqlite-conn-string example-db.sqlite3
```

An explanation of all the options available is in a following section.

This command will run Goat Guardian on port 3000.  If you open up a web browser
and open http://localhost:3000/twitter/login you will be redirected to twitter
to login.

Other ways of interacting with Goat Guardian will be explained in a following
section.


## Origin

Goat Guardian was originally proposed as a
[question](https://security.stackexchange.com/questions/187191/would-a-reverse-proxy-authentication-server-be-a-secure-setup/187219)
on the Information Security Stack Exchange.

## Maintainers

- [Kadzuya Okamoto](https://github.com/arowM)
- [Dennis Gosnell](https://github.com/cdepillabout)
