################################################################################
# Store shared Mosquitto secrets here.
################################################################################
{
  age.secrets = {
    mosquitto-home-assistant-password = {
      generator.script = "long-passphrase";
      rekeyFile = ../secrets/mosquitto-home-assistant-password.age;
      settings = {
        # For Mosquitto's password generator.
        username = "home-assistant";
        # For Home Assistant's password generator.
        fieldName = "mosquitto-password";
      };
    };
    mosquitto-tasmota-master-bedroom-led-strip-password = {
      generator.script = "long-passphrase";
      rekeyFile =
        ../secrets/mosquitto-tasmota-master-bedroom-led-strip-password.age;
      settings = {
        # Just follow the name of the device.  Found here:
        # http://tasmota-8cfdb2-7602.proton/in?
        username = "tasmota-8CFDB2-7602";
      };
    };
  };
}
