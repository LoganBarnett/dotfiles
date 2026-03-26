################################################################################
# ivatar — self-hosted Libravatar / Gravatar-compatible avatar service.
#
# Python/Django application.  Several avatar-generator packages and a couple
# of utility libraries are not in nixpkgs; they are packaged inline below.
#
# Configuration model
# ───────────────────
# ivatar loads ivatar/settings.py, which ends with `from config import *`.
# config.py sits at the project root (not inside the ivatar Python package) and
# is not installed by setuptools.  We copy it to site-packages in postInstall so
# it is importable at runtime.
#
# config.py reads many settings from environment variables.  We patch it to:
#   • Fix a hardcoded Docker hostname in the PostgreSQL host.
#   • Override CACHES to use the filesystem backend (no memcached required).
#   • Allow ALLOWED_HOSTS and STATIC_ROOT to be injected via env vars.
#
# SECRET_KEY is appended to settings.py so it is read from the systemd
# credential file (CREDENTIALS_DIRECTORY/secret-key) rather than the
# hard-wired development key.
#
# Entry points
# ────────────
# Two entry points are added to setup.cfg in postPatch:
#   ivatar-manage  — Django management commands (migrate, collectstatic, …)
#   ivatar-server  — gunicorn WSGI server wrapping ivatar.wsgi:application
#
# Both are wrapped by buildPythonApplication with the full Python closure, so
# all propagatedBuildInputs are available at runtime without extra PATH or
# PYTHONPATH wrangling in the NixOS module.
################################################################################
{
  lib,
  fetchFromGitHub,
  fetchurl,
  python3,
}:
let
  inherit (python3.pkgs)
    buildPythonApplication
    buildPythonPackage
    fetchPypi
    ;

  # ── Avatar generators not in nixpkgs ───────────────────────────────────────
  # All four are maintained by ofalk (ivatar's author) or a direct contributor;
  # they follow the same minimal setup.py / Pillow-only dependency pattern.

  identicon = buildPythonPackage {
    pname = "identicon";
    # No release tags; pin to the commit current at packaging time.
    version = "unstable-2025-01-01";
    format = "pyproject";
    src = fetchFromGitHub {
      owner = "ofalk";
      repo = "identicon";
      rev = "ec61896b87c55326690eecb928df3d9ed9257795";
      hash = "sha256-/0jpcGk9/2kej1jgSBbTrA9syluBxf9aE4LE14Dp33o=";
    };
    build-system = with python3.pkgs; [ setuptools ];
    propagatedBuildInputs = with python3.pkgs; [ pillow ];
  };

  monsterid = buildPythonPackage {
    pname = "monsterid";
    version = "unstable-2025-01-01";
    format = "setuptools";
    src = fetchFromGitHub {
      owner = "ofalk";
      repo = "monsterid";
      rev = "630e35ad36eb12fad582a38fe48b64ef1c8bc7d4";
      hash = "sha256-t3p65ob0Ea6/84r5LfKxt5xhYAquNfny70XDPy2qUY8=";
    };
    propagatedBuildInputs = with python3.pkgs; [ pillow ];
  };

  pagan = buildPythonPackage {
    pname = "pagan";
    version = "unstable-2025-01-01";
    format = "pyproject";
    src = fetchFromGitHub {
      owner = "daboth";
      repo = "pagan";
      rev = "48b2ae053aac6a539be2bf316eb43ede599f951f";
      hash = "sha256-/ZMLe+vo+OZaxJLKqUO+WzBzrVPTZoisTfIC3dCub1k=";
    };
    build-system = with python3.pkgs; [ setuptools ];
    propagatedBuildInputs = with python3.pkgs; [ pillow ];
  };

  robohash = buildPythonPackage {
    pname = "robohash";
    version = "unstable-2025-01-01";
    format = "setuptools";
    src = fetchFromGitHub {
      owner = "ofalk";
      repo = "Robohash";
      # Upstream development happens on the devel branch; there is no stable tag.
      rev = "8effe253b447442ffd220ca8c5eba6201660e8e4";
      hash = "sha256-2nbfMyExIZrR3MhLXZXQ6DWTgwrmCxoA4bSRwvN+Ge8=";
    };
    propagatedBuildInputs = with python3.pkgs; [
      pillow
      natsort
    ];
  };

  # pydenticon5 is a Python 3 port of pydenticon.  nixpkgs carries `pydenticon`
  # (azaghal/pydenticon), which is a different project with an incompatible API.
  pydenticon5 = buildPythonPackage {
    pname = "pydenticon5";
    version = "unstable-2025-01-01";
    format = "setuptools";
    src = fetchFromGitHub {
      owner = "ercpe";
      repo = "pydenticon5";
      rev = "7252c422d05f2d815fcbb74b710c7c8d549248d7";
      hash = "sha256-4KQgc5e5ycvX+rvzaRNx2M/xcZAiwD5iLbLlkozkd6g=";
    };
    propagatedBuildInputs = with python3.pkgs; [ pillow ];
  };

  # social-auth-app-django 5.4.3 is the last release that supports Django 4.x;
  # 5.5.0 bumped the floor to Django 5.1.  nixpkgs carries 5.5.1+, so we
  # package 5.4.3 inline until nixpkgs Django is upgraded.
  social-auth-app-django = buildPythonPackage rec {
    pname = "social-auth-app-django";
    version = "5.4.3";
    format = "pyproject";
    src = fetchPypi {
      # PyPI sdist filename uses underscores; pname uses hyphens.
      pname = "social_auth_app_django";
      inherit version;
      hash = "sha256-0fQobVyh5RLJsvaG5+yyoBKBSPGjPYU7adwHtYUINi4=";
    };
    build-system = with python3.pkgs; [ setuptools ];
    propagatedBuildInputs = with python3.pkgs; [
      django
      social-auth-core
    ];
    doCheck = false;
  };

  # ── Auth / account packages not in nixpkgs ─────────────────────────────────

  # ofalk's fork adds Python 3 compatibility fixes on top of the original.
  django-openid-auth = buildPythonPackage {
    pname = "django-openid-auth";
    version = "unstable-2025-01-01";
    format = "setuptools";
    src = fetchFromGitHub {
      owner = "ofalk";
      repo = "django-openid-auth";
      rev = "2bb6a517bbe58ed4d7266f4c8f2e2f7ab45bbe9f";
      hash = "sha256-jwx84bJlBoBdS8m4knig/GnKqlDINMBCnxTDH/Gp8vA=";
    };
    propagatedBuildInputs = with python3.pkgs; [
      django
      python3-openid
      six
    ];
  };

  django-user-accounts = buildPythonPackage rec {
    pname = "django-user-accounts";
    version = "3.3.2";
    format = "pyproject";
    src = fetchPypi {
      inherit pname version;
      hash = "sha256-ruq5O1cV+VD5we28A7QUONCSe2gjLEC6HxlTjW6z+1k=";
    };
    build-system = with python3.pkgs; [ setuptools ];
    propagatedBuildInputs = with python3.pkgs; [
      django
      django-appconf
      pytz
    ];
    # Tests require a database; skip them.
    doCheck = false;
  };

  pyLibravatar = buildPythonPackage rec {
    pname = "pylibravatar";
    version = "2.0.2";
    format = "pyproject";
    src = fetchPypi {
      inherit pname version;
      hash = "sha256-YtzUMUj7TQtNKWBlBsxCzcYG7myqSROnKH8v8De5GoA=";
    };
    build-system = with python3.pkgs; [ setuptools ];
    propagatedBuildInputs = with python3.pkgs; [ dnspython ];
  };

in
buildPythonApplication {
  pname = "ivatar";
  version = "1.7.0";
  format = "pyproject";

  src = fetchurl {
    url = "https://git.linux-kernel.at/oliver/ivatar/-/archive/1.7.0/ivatar-1.7.0.tar.gz";
    hash = "sha256-pU3vTgrf11D+D41e4z/9ss23dPbNgB6jcNNhpivtSAk=";
  };

  build-system = with python3.pkgs; [
    setuptools
    wheel
  ];

  propagatedBuildInputs = with python3.pkgs; [
    # ── Core Django stack ─────────────────────────────────────────────────────
    argon2-cffi
    bcrypt
    defusedxml
    django
    django-anymail
    django-auth-ldap
    django-bootstrap4
    django-extensions
    django-ipware
    dnspython
    email-validator
    gunicorn
    pillow
    prettytable
    psycopg2
    py3dns
    python-magic
    # ── Avatar generators ─────────────────────────────────────────────────────
    identicon
    monsterid
    pagan
    pydenticon5
    robohash
    # ── Authentication ────────────────────────────────────────────────────────
    django-openid-auth
    python3-openid
    social-auth-app-django
    # ── Account management / federation ──────────────────────────────────────
    django-user-accounts
    pyLibravatar
    # ── Observability ─────────────────────────────────────────────────────────
    opentelemetry-api
    opentelemetry-sdk
  ];

  postPatch = ''
        # ── config.py: fix hardcoded Docker hostname ──────────────────────────────
        # The upstream config assumes a Docker Compose setup where the PostgreSQL
        # container is named "postgresql".  For a local Unix-socket connection the
        # host must be empty (or a socket directory).
        substituteInPlace config.py \
          --replace-fail '"HOST": "postgresql",' \
                         '"HOST": os.environ.get("POSTGRESQL_HOST", ""),'

        # ── config.py: append NixOS-specific overrides ───────────────────────────
        # These run after all upstream logic in config.py so they take precedence.
        # Using a quoted heredoc (no variable expansion) so the Python code is
        # written verbatim.
        cat >> config.py << 'NIXOS_OVERRIDES'

    # ── NixOS deployment overrides ────────────────────────────────────────────────
    # These lines are appended by the Nix derivation.  They override the defaults
    # above using environment variables so the NixOS module can configure the
    # service declaratively without patching source.

    ALLOWED_HOSTS = os.environ.get("IVATAR_ALLOWED_HOSTS", "localhost").split(",")

    # nginx forwards the original Host header correctly; the upstream
    # USE_X_FORWARDED_HOST = True causes Django to read X-Forwarded-Host
    # instead, which ends up as the server hostname.  Disable it so Django
    # uses the Host header that nginx sets to the virtual-host FQDN.
    USE_X_FORWARDED_HOST = False

    if "IVATAR_STATIC_ROOT" in os.environ:
        STATIC_ROOT = os.environ["IVATAR_STATIC_ROOT"]

    CACHES = {
        "default": {
            "BACKEND": "django.core.cache.backends.filebased.FileBasedCache",
            "LOCATION": os.environ.get("IVATAR_CACHE_DIR", "/var/tmp/ivatar_cache"),
            "TIMEOUT": 900,
        },
    }

    # SMTP settings for the django.core.mail.backends.smtp.EmailBackend.
    # Django does not read these from env vars natively; bridge them here so the
    # NixOS module can inject them without a separate settings file.
    if "DJANGO_EMAIL_HOST" in os.environ:
        EMAIL_HOST = os.environ["DJANGO_EMAIL_HOST"]
        EMAIL_PORT = int(os.environ.get("DJANGO_EMAIL_PORT", "587"))
        EMAIL_HOST_USER = os.environ.get("DJANGO_EMAIL_HOST_USER", "")
        EMAIL_USE_TLS = os.environ.get("DJANGO_EMAIL_USE_TLS", "1") == "1"
    # ── ssl: clear VERIFY_X509_STRICT ────────────────────────────────────────
    # urllib3 2.5+ explicitly ORs ssl.VERIFY_X509_STRICT into every context it
    # creates for Python 3.13+.  The internal proton CA lacks a Key Usage
    # extension and is rejected under strict mode.  Patch urllib3's context
    # factory to clear the flag after construction; REQUESTS_CA_BUNDLE still
    # pins the exact CA bundle.
    import ssl as _ssl
    import urllib3.util.ssl_ as _urllib3_ssl
    _orig_urllib3_ctx = _urllib3_ssl.create_urllib3_context
    def _patched_urllib3_ctx(*args, **kwargs):
        ctx = _orig_urllib3_ctx(*args, **kwargs)
        if hasattr(_ssl, "VERIFY_X509_STRICT"):
            ctx.verify_flags &= ~_ssl.VERIFY_X509_STRICT
        return ctx
    _urllib3_ssl.create_urllib3_context = _patched_urllib3_ctx
    del _orig_urllib3_ctx, _patched_urllib3_ctx, _urllib3_ssl, _ssl
    # ── social-auth OIDC ──────────────────────────────────────────────────────
    # Wire social-auth-app-django for OIDC login.  All settings are gated on
    # the relevant env var being set so the service starts cleanly even if OIDC
    # is not yet configured on a host.
    INSTALLED_APPS.append("social_django")

    AUTHENTICATION_BACKENDS = (
        ["social_core.backends.open_id_connect.OpenIdConnectAuth"]
        + list(AUTHENTICATION_BACKENDS)
    )

    if os.environ.get("IVATAR_OIDC_ENDPOINT"):
        SOCIAL_AUTH_OIDC_OIDC_ENDPOINT = os.environ["IVATAR_OIDC_ENDPOINT"]
    if os.environ.get("IVATAR_OIDC_CLIENT_ID"):
        SOCIAL_AUTH_OIDC_KEY = os.environ["IVATAR_OIDC_CLIENT_ID"]
    _oidc_cred = os.path.join(
        os.environ.get("CREDENTIALS_DIRECTORY", ""), "oidc-client-secret"
    )
    if os.path.isfile(_oidc_cred):
        with open(_oidc_cred) as _f:
            SOCIAL_AUTH_OIDC_SECRET = _f.read().strip()
    del _oidc_cred

    NIXOS_OVERRIDES

        # ── ivatar/settings.py: read SECRET_KEY from credential file ─────────────
        # The base settings.py ships with a hard-wired development key.  Append
        # code that replaces it with the contents of the systemd credential file
        # when the service runs under LoadCredential.  CREDENTIALS_DIRECTORY is set
        # automatically by systemd when LoadCredential is used.
        cat >> ivatar/settings.py << 'NIXOS_SECRET'

    # ── NixOS: load SECRET_KEY from systemd credential ───────────────────────────
    import os as _nix_os
    _creds_dir = _nix_os.environ.get("CREDENTIALS_DIRECTORY", "")
    if _creds_dir:
        _key_file = _nix_os.path.join(_creds_dir, "secret-key")
        if _nix_os.path.isfile(_key_file):
            with open(_key_file) as _f:
                SECRET_KEY = _f.read().strip()
    del _nix_os, _creds_dir
    NIXOS_SECRET

        # ── login.html: add SSO button ───────────────────────────────────────────
        # Write the SSO button to a fragment file and splice it in with sed's r
        # command.  This keeps the Nix-string minimum indentation at ≥4 spaces
        # so the heredoc terminators above remain at column 0 after stripping.
        cat > sso-button.html << 'SSO_EOF'
      &nbsp;
      <a href="/social/login/oidc/" class="button">{% trans 'Login with SSO' %}</a>
    SSO_EOF
        sed -i "/openid-login/r sso-button.html" \
          ivatar/ivataraccount/templates/login.html

        # ── ivatar/urls.py: add social-auth URL routes ───────────────────────────
        # social_django serves /social/login/<backend>/ and
        # /social/complete/<backend>/ — the OIDC callback lands at
        # /social/complete/oidc/ which must match the redirect_uri registered
        # with Authelia.
        sed -i \
          '/path("openid\/", include("django_openid_auth.urls")),/ a\    path("social\/", include("social_django.urls", namespace="social")),' \
          ivatar/urls.py

        # ── pyproject.toml: build configuration ──────────────────────────────────
        # The upstream source ships as a Docker/develop deployment with no build
        # configuration.  Create a minimal pyproject.toml so setuptools can
        # discover and package the ivatar Python code.
        # buildPythonApplication wraps console_scripts with the full Python closure,
        # so both binaries will have all propagatedBuildInputs on their path.
        cat > pyproject.toml << 'PYPROJECT'
    [build-system]
    requires = ["setuptools", "wheel"]
    build-backend = "setuptools.build_meta"

    [project]
    name = "ivatar"
    version = "1.7.0"

    [project.scripts]
    ivatar-manage = "ivatar._manage:main"
    ivatar-server = "gunicorn.app.wsgiapp:run"

    [tool.setuptools.packages.find]
    include = ["ivatar*"]

    [tool.setuptools.package-data]
    "*" = ["*.html", "*.css", "*.js", "*.png", "*.gif", "*.ico", "*.po", "*.mo", "*.svg", "*.txt"]
    PYPROJECT

        # Create the management-command entry point module.
        cat > ivatar/_manage.py << 'MANAGE_PY'
    import os
    import sys


    def main():
        os.environ.setdefault("DJANGO_SETTINGS_MODULE", "ivatar.settings")
        from django.core.management import execute_from_command_line

        execute_from_command_line(sys.argv)


    if __name__ == "__main__":
        main()
    MANAGE_PY
  '';

  postInstall = ''
    # config.py is loaded via `from config import *` at the end of
    # ivatar/settings.py, but setuptools only installs named packages
    # (directories with __init__.py).  Copy it explicitly so it is importable
    # once the application is installed outside the source tree.
    cp config.py "$out/${python3.sitePackages}/config.py"
    # The root templates/ directory is not inside the ivatar Python package
    # so setuptools skips it.  config.py adds BASE_DIR/templates to DIRS
    # where BASE_DIR is the parent of ivatar/ = site-packages.  Copy the
    # directory there so Django can find the top-level templates.
    cp -r templates "$out/${python3.sitePackages}/templates"
  '';

  meta = {
    description = "Self-hosted Libravatar / Gravatar-compatible federated avatar service";
    homepage = "https://git.linux-kernel.at/oliver/ivatar";
    license = lib.licenses.agpl3Plus;
    mainProgram = "ivatar-manage";
    platforms = lib.platforms.linux;
  };
}
