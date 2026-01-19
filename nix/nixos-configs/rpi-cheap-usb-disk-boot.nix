################################################################################
# Using an enclosure for your boot disk on your Raspberry Pi, and it's a cheap
# JMicron driver?  You'll need this.
################################################################################
{ ... }: {
  boot.kernelParams = [
    # give USB time to settle.
    "rootdelay=5"
  ];
}
