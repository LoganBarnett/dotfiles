################################################################################
# Enable realtime game streams (interactive).
#
# Sunshine requires a PIN to complete handshaking, which is done via the desktop
# typically.  However a "local" connection to the web application it hosts is
# also adequate for supplying this PIN.
# 1. Setup an SSH tunnel (e.g. ssh -L 47990:127.0.0.1:47990 titanium.proton),
#    47990 is the default port.
# 2. Navigate to https://localhost:47990.  You may have to accept a self signed
#    certificate.  I would like to find out how to fix that, but it is left
#    another day.
# 3. Create a user account if necessary, and save the credentials somewhere
#    secure.
# 4. Wait or refresh to log in with the newly created credentials.  The UI will
#    instruct you to do this too.
# 5. Once logged in, click on the PIN tab at the top.
# 6. Attempt to connect via Moonlight on the target host (titanium.proton in my
#    case).
# 7. Take the PIN it demands, and input it into the web UI.  Submit.
# 8. Return to Moonlight.  You should see that the connection icon to your host
#    no longer has a lock icon on it.  Double click it to connect.
# 9. Enjoy streaming.
################################################################################
{ ... }: {
  services.sunshine = {
    enable = true;
    openFirewall = true;
  };
}
