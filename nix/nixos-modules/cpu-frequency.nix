################################################################################
# CPU frequency management.
#
# This isn't about overclocking but _under clocking_.  I suppose it could
# include overclocking, but this is of particular interest for under clocking
# because both Intel and AMD CPUs offer different profiles to clock the machine
# lower on demand.  This is generally referred to as "pstate" and there's some
# kernel.org docs on the topic that are of interest.  Speaking strictly for AMD,
# the perspective seems to be that of the firmware and not the host OS.
#
# AMD specific documentation:
# https://docs.kernel.org/admin-guide/pm/amd-pstate.html
#
# Key takeaways:
# If the pstate is set to "active", that means the firmware will actively manage
# the CPU frequency.  This means the firmware is making choices about what the
# host OS uses.  "passive" means ... I don't know what it means.  I think maybe
# it means there's policies given via the kernel?  "guided" means the OS decides
# what to use.
#
# We want "guided".
#
# Other tools are included to help diagnose potential CPU issues.
################################################################################
{ config, pkgs, ... } : {
  boot.kernelParams = [
    "initcall_blacklist=acpi_cpufreq_init"
    # Not sure what this does, but I found some advice suggesting to set this.
    # It had no noticeable effect.
    # "amd_pstate.shared_mem=1"
    "amd_pstate=guided"
  ];
  environment.systemPackages = [
    pkgs.cpufrequtils
    # This doesn't exist under pkgs.
    config.boot.kernelPackages.cpupower
  ];
  powerManagement.enable = false;
  # powerManagement.cpuFreqGovernor = "performance";
  powerManagement.cpuFreqGovernor = null;
  powerManagement.cpufreq.max = null;
  powerManagement.cpufreq.min = null;
  hardware.nvidia.powerManagement.enable = false;
  hardware.nvidia.powerManagement.finegrained = false;
  # services.cpufreq.enable = false;
  # services.auto-cpufreq.enable = false;
  # services.tlp.enable = false;
}
