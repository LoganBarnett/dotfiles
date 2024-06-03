{ extra-npm-registries ? [], lib }: ''
save-exact=true
//registry.npmjs.org/:=true
save-prefix=
${lib.concatStrings
  (builtins.map (s: "${s}\n") extra-npm-registries)
 }
''
