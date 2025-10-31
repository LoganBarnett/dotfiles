{
  age.secrets.home-assistant-client-secret = {
    generator.script = "long-passphrase";
    rekeyFile = ../secrets/home-assistant-client-secret.age;
  };
}
