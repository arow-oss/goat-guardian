# example-servant-app

This is an example Servant application showing how to make full use of Goat
Guardian.  It implements the full functionality of Goat Guardian.

The following is an explanation of how to build and use it.  When creating a
web application that runs upstream of Goat Guardian, this example application
can be used as either a starting point, or as a reference.

## Build

In order to build and run this example, you must first install Goat Guardian on
your system.  In order to install Goat Guardian, you must have `git` and
[`stack`](https://docs.haskellstack.org/en/latest/README/) installed:

```sh
$ git clone https://github.com/arow-oss/goat-guardian.git
$ cd goat-guardian
$ stack install goat-guardian
```

Next, you can build `example-servant-app`:

```sh
$ stack build example-servant-app
```

## Run

Now that Goat Guardian has been installed on your system, and the example
application has been built, we can run both of them.

First, you must run the `example-servant-app`:


```sh
$ stack exec -- example-servant-app
```

This runs the example application listening on port 8000 on `localhost`.

Next, you need to `goat-guardian`.  The following is an example of running it
with the specified twitter keys, session key, etc.  You'll need to specify your
own `twitter-oauth-key` and `twitter-oauth-secret`.  Please see the Goat
Guardian [`README.md`](../README.md) for information on how to do this:

```sh
$ goat-guardian \
    --twitter-oauth-key Egex7CUsqQqSoEDjMivGtT1r0 \
    --twitter-oauth-secret HCZPPaOmib64GP7QbQaEwrLwKswK8pQDe4UwsAS3EVJBupBj5l  \
    --session-key DORwMBc0sSvGvIDutozVyNnJwU7qTknHkqFWUhpoAElJruHWT0GH7qmTpKajqIxbkuyTN5M5mb9CuM5JECg7SKadylr1QMeZqo1yPexd07KEdMKCbqdxJBmgHoTbBLL8 \
    --redir-after-login-url http://localhost:3000/after-login \
    --sqlite-conn-string example-db.sqlite3
```

This runs Goat Guardian listening on port 3000 on `localhost`.

## Access

Now, you can try accessing the URL http://localhost:3000/ in your browser.

This opens up the homepage for the example application.  You can try logging in
with Twitter, as well as with an email and password.  After logging in you will
be redirected to http://localhost:3000/after-login.  This page allows you do
some operations that require the user to be logged in.

You can also check out http://localhost:3000/all-posts.  This page will display
differently depending on whether you are logged-in or not.

## Source

All of the code for the example web app is in [src/Lib.hs](./src/Lib.hs), including the
Haskell code and HTML code.  This is a good resource for figuring out how to
adapt Goat Guardian to work with your own web application.
