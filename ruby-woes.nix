# This is a nix file that outlines the impossibility of running with Ruby in
# nix. I don't know how other people do it. Their examples do not work.  This
# file outlines the various ways in which I have tried to get nix working with
# nix-env + withPackages (the recommended way to run a system-wide nix
# configuration). I repeat, this does not work. This is a record for the things
# I have attempted.
#
# Here is an example hammer run:
# hammer host ssh -s 'environment = foo and hostgroup = bar' -c 'hostname'
#
# This produces one of two errors, depending on which avenue is taken:
# The first results from leaning on bundlerApp:
#
# Traceback (most recent call last):
# 	9: from /nix/store/adhvc47v7wx856vlw0fnpsffnxj5x5pk-ruby-gems/bin/hammer:18:in `<main>'
# 	8: from /nix/store/2abzp6kzd7wni7z6qqrksmg0k83g9y6c-bundler-2.1.4/lib/ruby/gems/2.6.0/gems/bundler-2.1.4/lib/bundler.rb:149:in `setup'
# 	7: from /nix/store/2abzp6kzd7wni7z6qqrksmg0k83g9y6c-bundler-2.1.4/lib/ruby/gems/2.6.0/gems/bundler-2.1.4/lib/bundler/runtime.rb:20:in `setup'
# 	6: from /nix/store/2abzp6kzd7wni7z6qqrksmg0k83g9y6c-bundler-2.1.4/lib/ruby/gems/2.6.0/gems/bundler-2.1.4/lib/bundler/runtime.rb:101:in `block in definition_method'
# 	5: from /nix/store/2abzp6kzd7wni7z6qqrksmg0k83g9y6c-bundler-2.1.4/lib/ruby/gems/2.6.0/gems/bundler-2.1.4/lib/bundler/definition.rb:226:in `requested_specs'
# 	4: from /nix/store/2abzp6kzd7wni7z6qqrksmg0k83g9y6c-bundler-2.1.4/lib/ruby/gems/2.6.0/gems/bundler-2.1.4/lib/bundler/definition.rb:237:in `specs_for'
# 	3: from /nix/store/2abzp6kzd7wni7z6qqrksmg0k83g9y6c-bundler-2.1.4/lib/ruby/gems/2.6.0/gems/bundler-2.1.4/lib/bundler/definition.rb:170:in `specs'
# 	2: from /nix/store/2abzp6kzd7wni7z6qqrksmg0k83g9y6c-bundler-2.1.4/lib/ruby/gems/2.6.0/gems/bundler-2.1.4/lib/bundler/spec_set.rb:80:in `materialize'
# 	1: from /nix/store/2abzp6kzd7wni7z6qqrksmg0k83g9y6c-bundler-2.1.4/lib/ruby/gems/2.6.0/gems/bundler-2.1.4/lib/bundler/spec_set.rb:80:in `map!'
# /nix/store/2abzp6kzd7wni7z6qqrksmg0k83g9y6c-bundler-2.1.4/lib/ruby/gems/2.6.0/gems/bundler-2.1.4/lib/bundler/spec_set.rb:86:in `block in materialize': Could not find amazing_print-1.3.0 in any of the sources (Bundler::GemNotFound)
#
# The second results from leaning on bundlerEnv:
#
# Traceback (most recent call last):
# 	2: from /nix/store/p0hvm4x0ccccm17xvf12zlci11sbymxb-ruby-gems/lib/ruby/gems/2.6.0/bin/hammer:23:in `<main>'
# 	1: from /nix/store/nbf3gwfs1cp1lks5lgn0rambhn6czb0r-ruby-2.6.6/lib/ruby/2.6.0/rubygems.rb:296:in `activate_bin_path'
# /nix/store/nbf3gwfs1cp1lks5lgn0rambhn6czb0r-ruby-2.6.6/lib/ruby/2.6.0/rubygems.rb:277:in `find_spec_for_exe': can't find gem hammer_cli (>= 0.a) with executable hammer (Gem::GemNotFoundException)
#
# Despair.
#
# I have yet to put a bug report together. I have inadvertently wasted an entire
# day fiddling with this, finding various links to examples that are broken, no
# longer work, or experimental. It could be related to withPackages, but given
# how other things worked in the past I am doubtful.
#
# If you are a nix maintainer of some kind who has come across this, I apologize
# for any harsh tone here. I am exhausted.
#
# Link dump:
# https://github.com/NixOS/nixpkgs/tree/master/pkgs/development
# https://github.com/NixOS/nixpkgs/issues/83442
# https://github.com/shepting/ruby-nix-sample
# https://www.reddit.com/r/NixOS/comments/i1echp/ruby_nix_gem_loading_error/
# https://github.com/NixOS/nixpkgs/pull/61114/files
# https://discourse.nixos.org/t/ruby-exposing-new-gems-to-a-bundlerapp-installed-tool/2989/12
# https://discourse.nixos.org/t/installing-simple-ruby-gem-via-nix/2081
# https://github.com/nix-community/bundix
#
# Among those links I have found at least a couple of independent comments
# stating something along the lines of "just look at the code", which I find to
# be a major disappointment. Perhaps this is just the part of nix that's working
# on Ruby?
#

#######
# Everything below this is attempted outside of withPackages, in a let.
#######

  # functions =
  #   import <nixpkgs/pkgs/development/ruby-modules/bundled-common/functions.nix> {
  #     inherit lib;
  #     gemConfig = defaultGemConfig;
  #   };

  # additional-gems = lib.mapAttrs (name: initialAttrs:
  #   let
  #     attrs = functions.applyGemConfigs (
  #       { inherit ruby; gemName = name; } // initialAttrs
  #     );
  #   in
  #     buildRubyGem (functions.composeGemAttrs ruby additional-gems name attrs)
  # ) (import ./gemset.nix);

# This creates a gem based environment for us for the purposes of running
# applications built in Ruby. I don't think that even with Nix we can have Ruby
# applications who disagree on what versions of their dependencies should be
# installed - so be mindful of potential conflicts and breaks from adding new
# tools or upgrading existing ones.
#
# The approach here is to declare a Gemfile and let bundix do the hard work via
# `bundix -l`. This generates a gemset.nix file, which Nix can then consume. In
# a sense, bundix is generating a hollow gem whose sole purpose is to provide
# the gems actually desired, but as dependencies. From there, bundlerApp in Nix
# proper will take over, and place the gems in the right places. This also does
# the hard work of path management (via things like PATH and GEM_PATH), as well
# as providing symlinks to the executables provided by some of these gems.
#
# Be mindful that in order to actually have an executable it must be declared in
# the exes below.

  # gem-apps = pkgs.bundlerApp {
  #   # copyGemFiles = true;
  #   exes = [
  #     "eyaml"
  #     "hammer"
  #     "hammer-complete"
  #   ];
  #   gemdir = ./.;
  #   gemfile = ./Gemfile;
  #   gemset = ./gemset.nix;
  #   pname = "gem-apps";
  # };
    # gems = pkgs.bundlerEnv {
    #   name = "bundler-gems";
    #   gemdir = ./.;
    # };

    # ruby = pkgs.ruby.overrideAttrs {
    #   gems = pkgs.bundlerApp {
    #     name = "gems";
    #     copyGemFiles = true;
    #     exes = [
    #       "eyaml"
    #       "hammer"
    #       "hammer-complete"
    #     ];
    #     gemdir = ./.;
    #     gemfile = ./Gemfile;
    #     gemset = ./gemset.nix;
    #     pname = "gems";
    #   };
    # };

    # ruby = pkgs.ruby.overrideAttrs {
      # This attempt taken from
      # https://github.com/shepting/ruby-nix-sample/blob/master/shell.nix
      # and it doesn't work.
      # Interestingly, see
      # https://github.com/shepting/ruby-nix-sample/blob/master/gemset.nix for
      # how one might include a set of gems. I would hate to maintain that.
      # gems = pkgs.gems;
    #   gems = with pkgs; [
    #     aws-sdk         # To use eyaml encryption at work.
    #     hiera-eyaml     # To use eyaml encryption at work.
    #     hiera-eyaml-kms # To use eyaml encryption at work.
    #   ];
    # };
      # gem-apps = pkgs.bundlerApp {
      #   # copyGemFiles = true;
      #   exes = [
      #     "eyaml"
      #     "hammer"
      #     "hammer"
      #     "hammer-complete"
      #   ];
      #   gemdir = ./.;
      #   gemfile = ./Gemfile;
      #   gemset = ./gemset.nix;
      #   pname = "gem-apps";
      # };

      ##
      # This is my documented exmaple, but any of the *Gem variables here will
      # apply.
      #
      # 1. The pname _must_ match the name of a real gem, as that gem is
      # presented during install. Without it, the Ruby nix machinery doesn't see
      # it. It will claim there is no attribute for your pname.
      #
      # 2. Right now bundlerApp it doesn't automatically discover exes, you have
      # you figure those out on your own, and then list them in exes.
      # Alternatively you determine the Ruby location in the nix store and then
      # derive the gem's bin path from there, as is done in this configuration.
      # If you're here because of issues with GEM_PATH, consider launching a new
      # shell to determine the new version, or see the shell's rc file for how
      # this is determined.
      #
      # 3. bundix and bundlerApp/bundlerEnv alike assume a root directory with a
      # gem. This is because generally the nix environment (and particularly
      # these tools) assume you are working out of a directory that has your
      # Ruby project in it, instead of using these gems as system tools. While
      # Ruby might be ill suited for managing any kind of global tools, we
      # sometimes still need them. As such, there is ./nix-gems which holds
      # directories for suites of gems. These directories don't need to be exact
      # gem names but it helps to have them close.
      #
      # 4. To create a new gem bundle, you will need to create
      # ./nix-gems/<bundle-name>/Gemfile, and populate it with your desired
      # gems. Then create a bundlerApp entry like below, and assign that to a
      # meaningful variable name. Use that variable inside of the ruby packages,
      # down in the ruby entry below.
      #
      # 5. bundlerEnv is favored over bundlerApp. bundlerApp ensures the execs
      # are added to the PATH but the gems themselves aren't installed. We could
      # duplicate each bundlerApp declaration with a following bundlerEnv
      # declaration.
      ##

      amazingPrint = pkgs.bundlerEnv {
        inherit ruby;
        copyGemFiles = true;
        # exes = ["hammer" "hammer-complete"];
        # scripts = ["hammer" "hammer-complete"];
        gemdir = ./nix-gems/hammer/.;
        pname = "amazing_print";
      };
      hammerCliApp = pkgs.bundlerApp {
        inherit ruby;
        exes = ["hammer" "hammer-complete"];
        # scripts = ["hammer" "hammer-complete"];
        gemdir = ./nix-gems/hammer/.;
        pname = "hammer_cli";
      };
      # hammerCliApp = pkgs.bundlerApp {
      #   exes = ["hammer" "hammer-complete"];
      #   scripts = ["hammer" "hammer-complete"];
      #   gemdir = ./nix-gems/hammer/.;
      #   pname = "hammer_cli";
      # };
      hammerCliEnv = pkgs.bundlerEnv {
        inherit ruby;
        copyGemFiles = true;
        pname = "hammer_cli_foreman_ssh";
        name = "hammer_cli_foreman_ssh";
        gemdir = ./nix-gems/hammer/.;
      };
      # hieraEyamlApp = pkgs.bundlerApp {
      #   copyGemFiles = true;
      #   pname = "hiera-eyaml-kms";
      #   exes = ["eyaml"];
      #   gemdir = ./nix-gems/hiera-eyaml/.;
      # };
      hieraEyaml = pkgs.bundlerEnv {
        copyGemFiles = true;
        pname = "hiera-eyaml-kms";
        exes = ["eyaml"];
        gemdir = ./nix-gems/hiera-eyaml/.;
      };

     # hammerCli = pkgs.bundlerApp {
     #    copyGemFiles = true;
     #    pname = "hammer_cli_foreman_ssh";
     #    exes = ["hammer" "hammer-complete"];
     #    gemdir = ./nix-gems/hammer/.;
     #  };
     #  hieraEyaml = pkgs.bundlerApp {
     #    copyGemFiles = true;
     #    pname = "hiera-eyaml-kms";
     #    exes = ["eyaml"];
     #    gemdir = ./nix-gems/hiera-eyaml/.;
     #  };

#######
# Everything above this is attempted outside of withPackages, in a let.
#######
#######
# Everything below this is attempted inside of withPackages.
#######

(ruby.withPackages (ps: with ps; [
  bundler
  amazingPrint
  hammerCliApp
  hammerCliEnv
  hieraEyaml
  # This technique only works for packages like nokogiri below, which
  # tend to have native extensions and are otherwise supported
  # explicitly by the ruby nix package. This is not where you put
  # arbitrary packages. Instead use bundlerApp. See
  # https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/ruby.section.md#packaging-applications
  # Even then this documentation is lacking on how to integrate it into
  # nix-env.
  #
  # nokogiri # Works, but not what I want.
  # typhoeus # Works, but not what I want.
  # aws-sdk  # Doesn't work, but might be transitive.
  #
  # These do not work because GEM_PATH and GEM_HOME don't function, I
  # think. Best to just install global Ruby applications-via-gem with
  # gem install <pkg>. Ensure GEM_HOME points to the
  # ~/.gem/ruby/2.6.0 directory. GEM_PATH should be $GEM_HOME/bin. It's
  # possible the default values work here, and during my debugging I
  # crushed them.
  #
  # hiera-eyaml     # Alas, this is not supported. Install manually.
  # hiera-eyaml-kms # Alas, this is not supported. Install manually.
]))
#######
# Everything above this is attempted inside of withPackages.
#######
