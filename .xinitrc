#!/bin/sh

# Disable D-Bus auto-starting by setting DBUS_SESSION_BUS_ADDRESS to a non-existent bus:
export DBUS_SESSION_BUS_ADDRESS="invalid address"

# Use the version of the TrueType interpreter with proper hinting (see https://freetype.org/freetype2/docs/subpixel-hinting.html for the news that the v40 TrueType interpreter simply ignores all horizontal hinting instructions):
export FREETYPE_PROPERTIES="truetype:interpreter-version=35"

export WEBKIT_FORCE_SANDBOX=0 # Temporary fix for Emacs 27 on Guix needing GApplication for sandbox.

#exec emacsclient --create-frame --alternate-editor="" # Currently not loading `default' face correctly.
exec emacs
