# Expecting something useful here?  Yeah, me to.  I want to track at least where
# I can find some drivers.  I'll have to figure out how to unpack them later.
{
  # This suspiciously looks like a universal driver.  You can go to
  # https://epson.com/Support/Printers/All-In-Ones/ET-Series/Epson-ET-2720/s/SPT_C11CH42201
  # to do discovery on that with other OS versions.  The .dmg file contains a
  # .app, which is an install wizard.  The installation wizard has three
  # installation packages - the drivers, a Google Drive scan+update ...app?, and
  # a link to documentation.  It will query for research participation.  One can
  # see what driver installation entails with:
  # https://github.com/Homebrew/homebrew-cask/pull/161662
  # The wizard will instruct to "charge" the ink (I don't think this means
  # electromagnetically), and ask for confirmation that this is done.  It will
  # ask if you are connecting via wireless or direct USB.  It will ask if you
  # need to set up the printer, or if it is already on the network (if wireless
  # was chosen).  It then uses some bespoke communication app to connect.
  # Afterwards it has you push a button to add the printer.  Adding the printer
  # just brings up the dialog.  The screenshot is incorrect.  The printer must
  # still be connected to via an IP or domain name.  You _must_ select Line
  # Printer Daemon - LPD as the protocol, and for the driver use Select
  # Software.  From there you should be able to select the specific printer
  # version.  From there you should be able to click Add.  This should not sit
  # for more than a few seconds, otherwise check you selected the correct
  # protocol.
  #
  # During installation, it sets up EPSON and EPSON Event Manager login items.
  # It would be nice to number the steps.
  epson-2720-darwin-14-driver = "https://ftp.epson.com/drivers/ET2720_Lite_64_NA.dmg";
}
