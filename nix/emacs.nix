# The ultimate editor.
{ flake-inputs, lib, pkgs, ... }: {
  nixpkgs.overlays = [ flake-inputs.emacs-overlay.overlays.default ];
  environment.systemPackages = let
    ob-duckdb = (pkgs.emacs.pkgs.trivialBuild {
      pname = "ob-duckdb";
      ename = "ob-duckdb";
      version = "0.0.0-2024-11-04-unstable";
      src = pkgs.fetchFromGitHub {
        owner = "smurp";
        repo = "ob-duckdb";
        rev = "3fd1123e7552a97d676be8aebd22dfbe8c6cfd0e";
        hash = "sha256-dZWHFNIPeU1vcbIuZLRdEv6uQi6U/OmWYRmps75Ol5k=";
      };
      packageRequires = [];
      meta = {
        homepage = "https://github.com/smurp/ob-duckdb";
        license = lib.licenses.gpl3;
      };
    });
  in [
    # For ob-duckdb support.
    # pkgs.duckdb
    # For ob-dsq support.
    pkgs.dsq
    (pkgs.emacsWithPackagesFromUsePackage {
      alwaysTangle = false;
      config = ../lisp/init.el;
      # Use `config` above as the default init file.
      defaultInitFile = true;
      extraEmacsPackages = (epkgs: let
        completion-packages = [
          # In-buffer completion.  Helpful for suggesting symbols in programming
          # languages, and words that have been used before in the same buffer
          # or project.  I have elected to consolidate everything into vertico
          # since, without at least major lifting, corfu and vertico seem to be
          # mutually exclusive to each other.  Or at least I have a hard time
          # pulling the two apart.  Doom probably does this properly with a
          # mountain of hacks, but I don't care enough to investigate.
          # epkgs.corfu
          # Consult is a completing read system.  It basically comes up with
          # contextual suggestions (like files, spellings, snippets, and more).
          epkgs.consult
          epkgs.consult-dir
          epkgs.consult-flycheck
          epkgs.consult-yasnippet
          # Kind of a smart DWIM system in a way, and is recommended to go with
          # Vertico and friends.
          epkgs.embark
          epkgs.embark-consult
          # Give us colorful icons in auto-complete, which could help us
          # visually identify certain kinds of completion information (such as
          # structs vs. enums in Rust).
          # epkgs.nerd-icons-corfu
          # Give us colorful icons in auto-complete, which could help us
          # visually identify certain kinds of completion information (such as
          # structs vs. enums in Rust).
          epkgs.nerd-icons-completion
          # Provides context to Vertico completion results via a "margin".
          # These are incredibly useful.  Included are things such as file
          # permissions/size/mdate, key bindings (interactive Lisp functions),
          # and documentation (more Lisp).
          epkgs.marginalia
          # Orderless is part of the sorting/filtering mechanism that is
          # recommended to go with Vertico.  I admittedly don't know much about
          # it more than that.
          epkgs.orderless
          # This is the minibuffer completion system.  It integrates with the
          # Emacs built-in functions (contrast to company-mode).
          epkgs.vertico
          # Doom included this, but I haven't seen for myself if this is used,
          # let alone something I want.
          epkgs.vertico-posframe
        ];
        utility-packages = [
          # Automatically compile .el files when loading.
          epkgs.auto-compile
          epkgs.browse-at-remote
          epkgs.wgrep
          # epkgs.vertico-multiform
          # Set configuration easily at runtime/interactively.  Helpful for
          # testing out values or toggling various verbose / debug modes.
          # epkgs.custom
          # A library of helpful, abstract functions.
          epkgs.dash
          # Make use of direnv + .envrc to give us project specific tools
          # managed by Nix.
          epkgs.direnv
          # The best of modelines.
          epkgs.doom-modeline
          # Doom has some good themes.  Let's use one!
          epkgs.doom-themes
          # I want the branch that works with "require" per:
          # https://github.com/restaurant-ide/emacs-eruby-mode/tree/patch-1
          # This package should let us edit ERB templates with good color coding
          # and such.
          (pkgs.emacs.pkgs.trivialBuild {
            pname = "eruby-mode";
            ename = "eruby-mode";
            version = "1.2015111-2016-08-12-unstable";
            src = pkgs.fetchFromGitHub {
              owner = "restaurant-ide";
              repo = "emacs-eruby-mode";
              rev = "902465d4490415ff2241204e7e3facaf7f341073";
              hash = "sha256-SNxnLdAhY67hM3rJYEFU/rXgbpRKrtEsbAwg5cbF+T0=";
            };
            packageRequires = [];
            meta = {
              homepage = "https://github.com/petere/emacs-eruby-mode";
              # "free" is the unspecified license.
              license = lib.licenses.free;
            };
          })
          # Add some cool evil-mode bindings to org-mode.  See
          # https://github.com/Somelauw/evil-org-mode for some of the bindings
          # and changes it brings about.  Of note:
          # 1. < and > apply to headings and child headings (if collapsed for
          #    child headings).
          # 2. Many other things I take for granted.
          epkgs.evil-org
          # Flyspell is built in, I suppose.
          # epkgs.flyspell
          epkgs.flyspell-correct
          # Put git +/- symbols in the gutter and work with line numbers all at
          # the same time (as opposed to git-gutter).
          epkgs.git-gutter-fringe
          # Enhanced help/documentation for Emacs.
          epkgs.helpful
          # Manage keybindings sanely.
          epkgs.general
          # Load shared SSH/GPG agents from keychain.
          epkgs.keychain-environment
          # An optional dependency for doom-modeline to get pretty icons for
          # various things.
          epkgs.nerd-icons
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
          (epkgs.scad-mode.overrideAttrs (old: (let
            version = "94.0";
          in {
            inherit version;
            src = pkgs.fetchFromGitHub {
              owner = "LoganBarnett";
              repo = "emacs-scad-mode";
              rev = "38e715440b2a2b05db3dda94a7eb56c169c45760";
              hash = "sha256-TIyh5OfzIpcgBhbxq/1TxjXFWpH/twbLc29rkwL1IaI=";
            };
          })))
          epkgs.terraform-mode
          epkgs.yaml-mode
          epkgs.vimrc-mode
        ];
        org-mode-packages = [
          # Allow more advanced table querying / building than tblfm.  This
          # provides a SQL dialect that can be used.  It's kind of retired, but
          # I don't see alternatives at the moment that use something like SQL.
          # Alternatively there is org-aggregate, which seems to try to be a
          # better tblfm (https://github.com/tbanel/orgaggregate).  DSQ itself
          # points to DuckDB, which is in more active development.  There is
          # https://github.com/smurp/ob-duckdb so maybe that will be better, but
          # it still looks very new.
          # There is direct SQLite support in org-mode via:
          # https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-sqlite.html
          # However, it requires an actual SQLite database on disk, which I find
          # undesirable.
          # ob-duckdb
          epkgs.ob-dsq
          # Org-babel for OpenSCAD.  This is tucked into scad-mode.
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
          # Export org-mode documents to Jekyll, a static site generator.
          # It's included locally, actually.  We should see about publishing it.
          # epkgs.org-jekyll
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
          epkgs.org
          # epkgs.org-agenda
          # epkgs.org-capture
          # epkgs.org-id
          epkgs.ox-jira
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
          # Swap stuff?
          epkgs.evil-exchange
          # I forgot what iedit is for.
          epkgs.evil-iedit-state
          # Comment with g c.
          epkgs.evil-nerd-commenter
          # Jump to a character using two characters, because one frequently
          # isn't enough.
          epkgs.evil-snipe
          # Add a surround motion for handling matched braces, quotes, etc.
          epkgs.evil-surround
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
      in
        completion-packages
        ++ editing
        ++ languages
        ++ programs
        ++ utility-packages
        ++ org-mode-packages
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
