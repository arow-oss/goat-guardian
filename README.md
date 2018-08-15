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

## Origin

Goat Guardian was originally proposed as a
[question](https://security.stackexchange.com/questions/187191/would-a-reverse-proxy-authentication-server-be-a-secure-setup/187219)
on the Information Security Stack Exchange.

## Maintainers

- [Kadzuya Okamoto](https://github.com/arowM)
- [Dennis Gosnell](https://github.com/cdepillabout)
