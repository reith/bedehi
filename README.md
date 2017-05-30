bedehi
=====

Bedehi is a simple IOU software as a Telegram bot.  It helps you to have a
record of debts and notify your contacts whenever you repaid them by few simple
commands or the other way around.

This bot can be added to groups If you have a group that people there charge
each other regularly for something like buying daily foods.

Commands
-----
    /gave AMOUNT to @PERSON [-- some words as description]
    /took AMOUNT from @PERSON [-- some words as description]
    /report
    /contacts


Deployment
-----
You need Erlang for running this on your own server. [Create](https://core.telegram.org/bots#3-how-do-i-create-a-bot) a Telegram bot and
then put bot token in [config/sys.config](config/sys.config). You can change listening port or
host name here, defaults are set by [src/bedehi.app.src](src/bedehi.app.src).

Get [rebar3](https://s3.amazonaws.com/rebar3/rebar3) and make release:

    $ rebar3 release

Then put SSL certificates into `_build/default/rel/bedehi/ssl`:

* ca.crt: certificate chain
* server.crt: issued certificate
* server.key: certificate private key


Start software:

    $ ./_build/default/rel/bedehi/bin/bedehi start

[Set webhook](https://core.telegram.org/bots/api#setwebhook) of your bot according to server host name and listening port.
