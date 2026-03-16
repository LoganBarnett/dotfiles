{ lib, ...  }: let
  # I'm the Riddler!
  org-name = lib.concatStrings [
    "n"
    "w"
    "e"
    "a"
  ];
  org-colo-domain = "${org-name}colo.pvt";
  org-pvt-domain = "${org-name}.pvt";
in {
  programs.ssh = {

    # Global canonical domains and hostname settings.
    extraConfig = ''
      CanonicalDomains map2.${org-colo-domain} plo.${org-colo-domain} ${org-colo-domain} ${org-pvt-domain}
      CanonicalizeHostname yes
    '';

    matchBlocks = {

      # This host is required for managing my work repositories on BitBucket
      # Cloud.  Without this, the SSH agent will offer my personal key and that
      # key does not have adequate permission.  I cannot have the same key bound
      # to multiple accounts.  With this setup, I must forever use the name
      # given to Host below, instead of the actual hostname.  This will force
      # the SSH agent to use the key I have indicated.
      #
      # The usage now is the remote URL will appear as
      # "$work:project/repo.git".  Since all repositories are currently
      # under "$work" this should be easy at least.  Bonus: I don't have to add
      # the "git@" part anymore.
      #
      # For the original see below.  I have not have deviated at time of
      # writing.
      # https://gist.github.com/shakeeb91/cd3d3c387f339fbd93ac7388b3c885e0
      # Host $work
      #   HostName bitbucket.org
      #   User git
      #   IdentityFile ~/.ssh/id_rsa_$work
      #   IdentitiesOnly yes

      # See $work above for explanation.  This must also be present so the $work
      # key doesn't take precedence.
      "bitbucket.org" = {
        hostname = "bitbucket.org";
        user = "git";
        identityFile = "~/.ssh/id_rsa";
        identitiesOnly = true;
      };

      # Work domain configuration with Kerberos authentication.
      "*.${org-colo-domain}" = {
        extraOptions = {
          AddKeysToAgent = "yes";
          # We need this to prevent the fingerprint issue that can come up when
          # new hosts are created or their IPs have shifted.
          CheckHostIP = "no";
        };
        forwardAgent = true;
        extraOptions = {
          # Enable Kerberos ticket handling automatically.  This should forward
          # to other clients.
          GSSAPIAuthentication = "yes";
          GSSAPIDelegateCredentials = "yes";
          # Old/unsupported.
          # KerberosAuthentication yes
          # These keys are no longer supported and will generate an error, but
          # many old examples use it.
          # GSSAPIKeyExchange yes
          # GSSAPITrustDNS yes
          # Ensure that we are not using our public keys here.
          # Note the "Pub" instead of "Public".
          # PubKeyAuthentication no
          PreferredAuthentications = "gssapi-with-mic,gssapi-keyex";
          # ForwardX11 yes
          # ForwardX11Trusted yes
          UpdateHostKeys = "yes";
          ServerAliveInterval = "5";
          # There are so many hosts, and the values change, so the known_hosts
          # file just gets cluttered with meaningless entries.  Turn off the
          # public key check.
          StrictHostKeyChecking = "no";
          # Even with StrictHostKeyChecking=no, ssh will still write the public
          # key to known_hosts.  This is handy because it allows an auto-confirm
          # for new hosts, but still enforces the check for hosts visited in the
          # past.  If we set the known_hosts file to be /dev/null, then every
          # host will be treated as a new host, and therefore the frequent
          # rebuilds we do will not have an impact on any
          # automation.  Furthermore, it keeps the known_hosts file free of
          # ephemeral clutter.
          UserKnownHostsFile = "/dev/null";
        };
        user = "logan.barnett";
      };
    };
  };
}
