################################################################################
# A agenix-rekey generator for using a template to substitute secrets into a
# file that itself is also considered secret.
#
# This generator takes advantage of the un-merged settings feature in
# agenix-rekey.
#
# The settings structure expected is:
# {
#   template: String;
# }
# or
# {
#   templateFile: String;
# }
# The dependencies pulled in will substitute based on their name.
# The template string is in the form of %name% but one day I may make that
# configurable.
################################################################################
{ lib, ... }: {
  age.generators.template-file = {
    decrypt,
    deps,
    file,
    name,
    pkgs,
    secret,
    ...
  }: let
    template = (
      secret.settings.template
        or (builtins.readFile secret.settings.templateFile)
    );
  in
   lib.traceVal ''
     cat ${lib.escapeShellArg secret.settings.templateFile} \
       ${
         lib.strings.concatStringsSep " " (builtins.map
           (dep: ''
             | ${pkgs.replace}/bin/replace-literal \
               -e \
               -f \
               "%${lib.escapeShellArg dep.name}%" \
               "$(${decrypt} ${lib.escapeShellArg dep.file})" \
           ''
           )
           deps
         )
       }
   '';
}
