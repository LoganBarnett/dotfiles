#! /usr/bin/env bash

hostname "magnesium"

# Are we the right user?

# Is our user a sudoer as expected?

# X-Windows settings.
ln -s $DIR/lightdm.conf /etc/lightdm/lightdm.conf

# The magnesium device reports when I should be in a meeting by activating some
# desk lights. It needs a cron job, a server for blinking the lights, and a
# server for accepting the daily agenda.
