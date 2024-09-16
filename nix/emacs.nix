# The ultimate editor.
{ lib, pkgs, emacs-overlay, ... }: {
  nixpkgs.overlays = [ emacs-overlay.overlays.default ];
  environment.systemPackages = [
    (pkgs.emacsWithPackagesFromUsePackage {
      alwaysTangle = false;
      config = ../lisp/init.el;
      # Use `config` above as the default init file.
      defaultInitFile = true;
      extraEmacsPackages = (epkgs: let
        utility-packages = [
          # Automatically compile .el files when loading.
          epkgs.auto-compile
          epkgs.browse-at-remote
          # A collection of ivy integrations.  Ivy is a general choice selector.
          epkgs.counsel
          epkgs.counsel-projectile
          # Set configuration easily at runtime/interactively.  Helpful for
          # testing out values or toggling various verbose / debug modes.
          # epkgs.custom
          # A library of helpful, abstract functions.
          epkgs.dash
          # Make use of direnv + .envrc to give us project specific tools
          # managed by Nix.
          epkgs.direnv
          # Doom has some good themes.  Let's use one!
          epkgs.doom-themes
          # Add some cool evil-mode bindings to org-mode.  See
          # https://github.com/Somelauw/evil-org-mode for some of the bindings
          # and changes it brings about.  Of note:
          # 1. < and > apply to headings and child headings (if collapsed for
          #    child headings).
          # 2. Many other things I take for granted.
          epkgs.evil-org
          # Enhanced help/documentation for Emacs.
          epkgs.helpful
          # Manage keybindings sanely.
          epkgs.general
          # Load shared SSH/GPG agents from keychain.
          epkgs.keychain-environment
          # Some common lisp vs emacs lisp thing.
          epkgs.noflet
          # Use the pass utility (https://www.passwordstore.org/).
          epkgs.password-store
          # Enter passwords for secrets.
          epkgs.pinentry
          (pkgs.emacs.pkgs.trivialBuild {
            pname = "piper";
            ename = "piper";
            version = "0.0.0-2022-10-04-unstable";
            src = pkgs.fetchFromGitLab {
              owner = "howardabrams";
              repo = "emacs-piper";
              rev = "ddaf7d70cc8fbdd01ce6b7970e81a546aaeeb585";
              hash = "sha256-nNQ0F8/N2ZfdeNQXkkXbZMHuPqGejiqAnIkBq4qKwf4=";
            };
            packageRequires = [
              epkgs.cl-lib
              epkgs.dash
              # Built into Emacs?
              # epkgs.em-glob
              epkgs.f
              epkgs.hydra
              epkgs.s
            ];
            meta = {
              homepage = "https://gitlab.com/howardabrams/emacs-piper";
              license = lib.licenses.gpl3;
            };
          })
          # Project management.
          epkgs.projectile
          # Use ripgrep in search places (like projectile).
          epkgs.ripgrep
          # Make HTTP requests.
          epkgs.request
          # Make HTTP requests... but later.
          epkgs.request-deferred
        ];
        languages = [
          epkgs.dockerfile-mode
          epkgs.elm-mode
          # This must come from something else.  Magit perhaps?
          # epkgs.gitignore-mode
          # Editing of graphics shaders.
          epkgs.glsl-mode
          epkgs.gnuplot
          # A subset of plantuml - text declared diagramming.
          epkgs.graphviz-dot-mode
          # Cucumber .feature files.
          epkgs.feature-mode
          epkgs.json-mode
          epkgs.lsp-mode
          epkgs.lsp-ui
          # Org-babel for OpenSCAD.
          # epkgs.ob-scad
          # The readme mentions `openscad-lsp` which could be used to enhance
          # things further.  Worth a look!
          # There is also scad-dbus but likely wouldn't work on MacOS.  Unless
          # there's a dbus MacOS plugin...
          # The ultimate text document.  Find via "org-mode" typically, but it's
          # just "org" I guess.
          epkgs.org
          # Manage contacts via org-mode.  Can be consumed by mu4e.
          epkgs.org-contacts
          # A smattering of org-mode plugins.
          epkgs.org-contrib
          # Export org-mode documents to an HTML email dialect.
          epkgs.org-mime
          # Turn an org tree into a live, editable presentation!
          epkgs.org-tree-slide
          # Export org-mode documents to D&D LaTeX template.
          # Get from: https://github.com/xeals/emacs-org-dnd.git
          # epkgs.ox-dnd
          # Export org-mode documents to GitHub Flavored Markdown.
          epkgs.ox-gfm
          # Export org-mode documents to Reveal.js slide decks.
          epkgs.ox-reveal
          # Check lists for org-mode?
          # epkgs.org-checklist
          # Text based diagramming.
          epkgs.plantuml-mode
          epkgs.puppet-mode
          epkgs.enh-ruby-mode
          epkgs.js2-mode
          epkgs.lua-mode
          epkgs.markdown-mode
          epkgs.typescript-mode
          epkgs.nix-mode
          # For JSX editing.
          epkgs.rjsx-mode
          # epkgs.css-mode
          epkgs.web-mode
          # epkgs.html-mode
          epkgs.groovy-mode
          epkgs.rust-mode
          epkgs.rustic
          # I have a reference to ob-scad here:
          # https://github.com/wose/ob-scad.git But scad-mode contains this.
          # Perhaps it was merged?  Check the difference.
          epkgs.scad-mode
          epkgs.terraform-mode
          epkgs.yaml-mode
          epkgs.vimrc-mode
        ];
        programs = [
          # Gamified task management.
          epkgs.habitica
          (pkgs.emacs.pkgs.trivialBuild {
            pname = "ejira";
            ename = "ejira";
            version = "0.0.0-2022-06-21-unstable";
            src = pkgs.fetchFromGitHub {
              owner = "nyyManni";
              repo = "ejira";
              rev = "49cb61239b19bf13775528231d7d49656ec7a8bb";
              hash = "sha256-obDU3hSe+4aa/kwS8sSGmwEmpyXWhOFc5VY/Xxf6cjA=";
            };
            packageRequires = [
              epkgs.cl-lib
              epkgs.dash
              epkgs.f
              epkgs.helm
              epkgs.language-detection
              epkgs.org
              # epkgs.org-agenda
              # epkgs.org-capture
              # epkgs.org-id
              epkgs.ox-jira
              epkgs.s
              (pkgs.emacs.pkgs.trivialBuild {
                pname = "jiralib2";
                ename = "jiralib2";
                version = "0.0.0-2020-11-22-unstable";
# My fork contains some error handling that was never opened as a pull request.
                src = pkgs.fetchFromGitHub {
                  owner = "LoganBarnett";
                  repo = "jiralib2";
                  rev = "5c25c9033c0f755a1a2163ab99e48460ba0f8f64";
                  hash = "sha256-tdwwYaqWYH8+Yz3RpX1WalWnj7A0J2uBEwMiyUFJg9o=";
                };
                packageRequires = [
                  epkgs.dash
                  # Emacs built-in.
                  # epkgs.json
                  epkgs.request
                  # Emacs built-in.
                  # epkgs.url-parse
                ];
                meta = {
                  homepage = "https://github.com/LoganBarnett/jiralib2";
                  license = lib.licenses.gpl3;
                };
              })
            ];
            meta = {
              homepage = "https://github.com/nyyManni/ejira";
              license = lib.licenses.gpl3;
            };
          })
          # Emacs interface to mu, an email client.
          epkgs.mu4e
          # I forgot exactly what this is for.
          epkgs.multi-term
        ];
        editing = [
          # Edit Chrome browser text areas with Emacs.
          epkgs.atomic-chrome
          # Allow converting between various cases (PascalCase, camelCase,
          # snake_case, kebob-case, etc).  How does this vary from
          # string-inflection?
          epkgs.caseformat
          # A generalized auto-complete library.
          epkgs.company
          # Emacs is the best vim implementation I've found.
          epkgs.evil
          # Add evil mode to just about everything.
          epkgs.evil-collection
          # I forgot what iedit is for.
          epkgs.evil-iedit-state
          # Comment with g c.
          epkgs.evil-nerd-commenter
          # Jump to a character using two characters, because one frequently
          # isn't enough.
          epkgs.evil-snipe
          # Remote co-editing.  Are they still in business?
          epkgs.floobits
          # Generalized error reporting.
          epkgs.flycheck
          # Spell checking with flycheck.
          # epkgs.flyspell
          # epkgs.flyspell-correct
          epkgs.highlight-parentheses
          # Show colored bars in the indentation to indicate levels of
          # indentation.
          # epkgs.indent-bars
          (pkgs.emacs.pkgs.trivialBuild {
            pname = "indent-bars";
            ename = "indent-bars";
            version = "0.0.0-2024-08-30-unstable";
            src = pkgs.fetchFromGitHub {
              owner = "jdtsmith";
              repo = "indent-bars";
              rev = "c8376cf4373a6444ca88e88736db7576dedb51d6";
              hash = "sha256-MEegS7ArIQuE0z5e4h7EAnn+V0Q5MtIgthldyMpKrcA=";
            };
            packageRequires = [
              epkgs.cl-lib
              # This is implicitly included in Emacs.
              # epkgs.color
              # This is implicitly included in Emacs.
              # epkgs.cus-edit
              epkgs.compat
              # This is implicitly included in Emacs.
              # epkgs.font-lock
              # This is implicitly included in Emacs.
              # epkgs.font-remap
              epkgs.map
              epkgs.seq
              # This is implicitly included in Emacs.
              # epkgs.timer
            ];
            meta = {
              homepage = "https://github.com/jdtsmith/indent-bars";
              license = lib.licenses.gpl3;
            };
          })
          # The best git UI.
          epkgs.magit
          # Helper to break expressions into multiple lines, or multiple lines
          # combined into a single line.
          epkgs.multi-line
          # Prettify a buffer.  Only for JavaScript?  Other languages use it
          # too, but unknown for this Emacs library.
          epkgs.prettier-js
          # Coloring keywords is okay but coloring identifiers is pretty
          # helpful.  Doesn't always seem to work though.
          epkgs.rainbow-identifiers
          # Rainbow... something.
          epkgs.rainbow-mode
          # Matching colors for braces, parenthensies, etc. all with alternating
          # colors.
          epkgs.rainbow-delimiters
          # Squish or expand spacing to emulate other spacing (like 4 spaces to
          # 2).  I have this locally, so no ELPA package.
          # pkgs.redshift-indent
          # Emacs has trouble with extremely long lines.  This detects that and
          # disables a bunch of minor modes so you can at least look upon the
          # file.
# File is directly included in the dotfiles repo.
          # pkgs.so-long
          # Allow converting between various cases (PascalCase, camelCase,
          # snake_case, kebob-case, etc).  How does this vary from
          # caseformat?
          epkgs.string-inflection
          # Watch for file changes, I think.
          epkgs.tree-sitter
          epkgs.tree-sitter-langs
          # Keep whitespace in my buffers clean, but only if I modified the
          # lines involved.
          epkgs.ws-butler
          # Navigate chorded keybindings in a self-documenting way.
          epkgs.which-key
          # Use editorconfig settings, I think. 
          epkgs."with-editor"
          # Fix up HTML documents.
          epkgs.web-beautify
          epkgs.yasnippet
          # Get template files going better with yasnippet.
          # Need to pin to 0a5616216b6d8b15e50c2384f9b3fa2ff1616c80.
          (epkgs.yatemplate.overrideAttrs {
            src = pkgs.fetchFromGitHub {
              owner = "piknik";
              repo = "yatemplate";
              rev = "0a5616216b6d8b15e50c2384f9b3fa2ff1616c80";
              hash = "sha256-34zv0k5DPT6kMc38gVleAr8jbTkYMZMxN3AM7EOp5ww=";
            };
          })
        ];
      in utility-packages
      ++ editing
      ++ languages
      ++ programs
      );
    })
    # Other forms left for reference.
    # aarch64 (arm) is lacking on the mainline build of emacs/emacsMacPort. This
    # branch builds. See https://github.com/NixOS/nixpkgs/pull/138424 for
    # progress on it getting merged.
    # pkgs.emacsMacport
    # ((pkgs.emacsPackagesFor pkgs.emacs-unstable).emacsWithPackages (
    # # package = ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (
    #   epkgs: [
    #     epkgs.mu4e
    #   ]
    # ))
    # pkgs.emacs
  ];
}
