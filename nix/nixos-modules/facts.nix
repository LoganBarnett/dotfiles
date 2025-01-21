################################################################################
# I've other people use the nomenclature of "facts", and it kind of makes sense
# because "settings" would be a very ambiguous term.  "Facts" is only de facto
# via Puppet, as I understand.
#
# It means settings, but more like a meta settings.  Settings here should be
# logical instead of pertaining to specific systems whenever possible.
################################################################################
{
  network = {
    users = {
      logan = {
        description = "The reason we suffer.";
        email = "logustus@proton";
        type = "person";
        full-name = "Logan Barnett";
      };
      selenium-octoprint-service = {
        email = "selenium-octoprint-service@proton";
        type = "service";
        description = "Octoprint on Selenium.";
        full-name = "selenium-octoprint-service";
      };
    };
    groups = {
      "3d-printers" = {
        description = "People who can use 3D printers.";
        members = [
          "logan"
        ];
      };
    };
  };
}
