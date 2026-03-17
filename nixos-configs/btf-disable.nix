{
  # This is a deviation from the exact README version, following directions here
  # to customize the kernel: https://nixos.wiki/wiki/Linux_kernel This is done
  # to address the vmlinux/BPF build issues as stated here:
  # https://discourse.nixos.org/t/cannot-build-arm-linux-kernel-on-an-actual-arm-device/54218/3
  boot.kernelPatches = [
    {
      name = "disable-bpf";
      patch = null;
      # https://discourse.nixos.org/t/cannot-build-arm-linux-kernel-on-an-actual-arm-device/54218/3
      # Says to disable CONFIG_DEBUG_INFO_BTF (with no hints as to how), and
      # https://github.com/NixOS/nixpkgs/blob/ae725bafb39b7e96c39e9769f32600d0081e1361/pkgs/os-specific/linux/kernel/common-config.nix#L56
      # has DEBUG_INFO_BTF.  Just set both to "n" which should be false.
      extraConfig = ''
        DEBUG_INFO_BTF n
        CONFIG_DEBUG_INFO_BTF n
      '';
    }
  ];
}
