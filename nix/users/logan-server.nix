# This user is for my servers, which don't need nearly as rich of a
# configuration as my workstations.
{ ... }: {
  # TODO: Make this configurable such that my client machines require
  # passwords but service machines do not.
  security.sudo.wheelNeedsPassword = false;
  # Left as reference if you want fine-grained settings:
  # security.sudo.extraRules = [
  #   {
  #     users = [ "logan" ];
  #     options = [ "NOPASSWD" ];
  #   }
  # ];
  users.users = {
    logan = {
      # TODO: You can set an initial password for your user.
      # If you do, you can skip setting a root password by passing
      # '--no-root-passwd' to nixos-install.
      # Be sure to change it (using passwd) after rebooting!
      initialPassword = "correcthorsebatterystaple";
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQOx2dxH8oP1406bie6eO3HB6fin4NY01laNiWRqcNsrRl6/M6e80wiTnG9u0Walb3JXegyqrHKIlFgvcrn2Tg/y944akJ/XqrcLPn3vwTcCV6XGI/1hPdcN0V156pbbnTS/T9y9btO+QJvELOjT4dET6HixBeBpGhLM95cirOrJjT2C6VVBYTGdAu3eKwCeDsjQtfKOHp9Huv0c1i57Fb13iTU1u0+L2o+LMYpS8YNbcBOgzx9FyyjvA/KuEVcyt2raVpbJv6nOP9ynz7a1Ja3Y2tgQwC6XCMpgKYHDYxaJhJbWjv9cxwq4zSzBr8yrlDKooqvpp9fTdOBAWF4R2MI2wb01yaaTlqPDcATBl5+Xu+SvxYf9wBt6wFIbv0baf1WtDDE7u9d2K/MJhShK9p45AQPTbmoYw7fzeMQOLdZNdZdXIOHWd17IJi2T+WnnO9hL1x+M5uZUlFlk0jGu0NP/YmHuWjGxxL7AIO1hH2q7ZHq7tzM+8sV6tjfGePwALFXSBBSGn2czgtfKzEVRFHBQajPco0g9zFWvi5ZfmU4QAkWOrQQFLEYK4IE0e1gR9Dsnqdm5tiYkCdVlapbG9jWdIBAgOCMj2bBXn+YObCrbVHW4wNo5OR6nec+b6miCuG23ue/o5j2L64kE16n1+hGx/Bbm0Adif4vw8zXVhAmxvQ== logan@scandium"
      ];
      extraGroups = [
        # Allow this user to sudo.
        "wheel"
      ];
    };
  };
  # This should allow me to do remote deployments without --use-remote-sudo, or
  # at least will make it work on a freshly installed NixOS host.  The initially
  # deployed versions use a slimmed down version of the configuration, whose
  # settings are somewhat hand picked by the image maintainers.  I don't know
  # why my remote builds work without this after the first deployment has been
  # done (which must be done by copying the source over).  I can't find good
  # explanations of this.  But the wiki
  # (https://wiki.nixos.org/wiki/Nixos-rebuild#Deploying_on_other_machines)
  # specifically covers this error (even if the sequence doesn't come up).
  # I haven't tested this yet because new hosts are slow to come by.
  nix.settings.trusted-users = [ "logan" ];
}
