{ facts, ... }:
{
  programs.ssh = {

    matchBlocks = {

      "*.${facts.network.domain}" = {
        # This way we needn't manage identities on the various servers I work
        # on.
        forwardAgent = true;
        extraOptions = {
          # Prevent SSH from appending work canonical domains (e.g.
          # dc1.corp.pvt) to .proton hostnames, which causes stalls when the
          # canonical lookup times out instead of returning NXDOMAIN.
          CanonicalizeHostname = "no";
          # Unset the LC_CTYPE value to avoid this error, which can confuse
          # Tramp and is just noise:
          #
          # -bash: warning: setlocale: LC_ALL: cannot change locale (en_US.UTF-8)
          #
          # Unfortunately this doesn't work.  The man page for ssh_config says
          # the prefixed `-` can be used to remove previously declared SendEnv
          # variables.  However, the global ssh_config is what declares these
          # values on macOS.  One can see from running ssh with `-vvv` that
          # /etc/ssh/ssh_config gets loaded _after_ the user config
          # (~/.ssh/config).  So this solution can never work.  I cannot find
          # anything that will allow me to set precedence of these
          # files.  Instead, go to the host in question and run this: sudo
          # locale-gen en_US.UTF-8
          #
          # SendEnv -LC_CTYPE -LC_ALL
        };
      };

      # This is our USB thumb drive host for bootstrapping installation.  Its
      # fingerprint is essentially dynamic, so ignore it.
      "nucleus.${facts.network.domain}" = {
        extraOptions = {
          StrictHostKeyChecking = "no";
        };
      };

    };
  };

}
