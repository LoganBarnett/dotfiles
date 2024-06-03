{ extra-gem-sources ? [], lib }: ''
---
:backtrace: false
:bulk_threshold: 1000
:sources:
- https://rubygems.org/
${lib.concatStrings
  (builtins.map (s: "- ${s}\n") extra-gem-sources)
 }
:update_sources: true
:verbose: true
:concurrent_downloads: 8
gem: "--no-document --env-shebang"
install: "--user-install"
''
