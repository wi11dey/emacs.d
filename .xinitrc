#!/bin/sh

# Disable D-Bus auto-starting by setting DBUS_SESSION_BUS_ADDRESS to a non-existent bus:
export DBUS_SESSION_BUS_ADDRESS="invalid address"

export WEBKIT_FORCE_SANDBOX=0 # Temporary fix for Emacs 27 on Guix needing GApplication for sandbox.

#exec emacsclient --create-frame --alternate-editor="" # Currently not loading `default' face correctly.
exec emacs
