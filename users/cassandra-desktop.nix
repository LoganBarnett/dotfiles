{ ... }: {
  # Nothing to do here yet.
  # home-manager.users.cassandra = {};
  users.users.cassandra = {
    home = "/home/cassandra";
    isNormalUser = true;
    initialPassword = "yiddishcrispingappearfantasizestoplightwalmart";
  };
}
