{ pkgs, ... }: {
  environment.systemPackages = [
    # A program to allow communicating over serial. Perhaps I can script it?
    pkgs.minicom
    # Connect to MongoDB for poking around the document store.
    # Currently broken due to gperftools.
    # mongodb
    # Like nmap, a tool for testing network ports. The executable is "nc".
    # This installs the GNU version, although the OpenBSD version allows
    # connecting to Unix sockets. For Unix sockets just use socat.
    pkgs.netcat
    # Recursively walks up the file hiearchy to show permissions. Quite helpful!
    # Currently not available as a nix package. Research on this has led to
    # attempting unixtools and nettools. The unixtools package doesn't contain
    # namei, and I could not confirm nettools due to an issue with openssl being
    # out of date in that package.
    #
    # Currently broken on aarch64. No tickets found on it. Looks like xnu is
    # trying to use x86_64 and breaking.
    #pkgs.nettools
    # A tool for mapping network ports.
    pkgs.nmap
  ];
}
