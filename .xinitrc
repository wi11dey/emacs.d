#!/bin/sh

# Disable D-Bus auto-starting by setting DBUS_SESSION_BUS_ADDRESS to a non-existent bus:
export DBUS_SESSION_BUS_ADDRESS="invalid address"

exec emacsclient --create-frame --alternate-editor="" # Currently not loading `default' face correctly.
# exec emacs
