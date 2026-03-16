{ gnused, writeShellApplication }:
writeShellApplication {
  name = "sed";
  # Store path used directly so the wrapper doesn't recurse through its own
  # name on PATH.
  text = ''
    prev=""
    for arg in "$@"; do
      if [[ "$prev" == "-i" ]] && [[ -z "$arg" ]]; then
        echo "sed: This is GNU sed; -i does not accept a suffix argument." >&2
        echo "sed: Use -i alone for in-place editing without a backup file." >&2
        exit 1
      fi
      prev="$arg"
    done
    exec ${gnused}/bin/sed "$@"
  '';
}
