<!DOCTYPE html>

<html lang="en">

  <head>
    <title>Not found</title>

    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <meta http-equiv="X-UA-Compatible" content="IE=Edge" />

    <script type="text/javascript" src="js/jquery.min.js"></script>
    <script type="text/javascript" src="js/jquery-ui.min.js"></script>

    <meta name="viewport" content="width=device-width, initial-scale=1.0" />

    <script type="text/javascript" src="bootstrap/js/bootstrap.min.js"></script>
    <link rel="stylesheet" href="bootstrap/css/bootstrap.min.css" />

    <link rel="stylesheet" href="bootstrap/css/bootstrap-responsive.min.css" />

    <link rel="stylesheet" href="css/nixos-site.css" type="text/css" />

    <link rel="stylesheet" href="//netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css" />

    <link rel="shortcut icon" type="image/png" href="/favicon.png" />
    
    <meta name="google-site-verification" content="ir-07nYvo3u3x_VmkTO1wCfYJ8uC-SrVBGR7hZgqPSE" />

  </head>

  <body>

    <div class="navbar navbar-fixed-top">
      <div class="navbar-inner">
        <div class="container">
          <button type="button" class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </button>
          <li class="dropdown brand">
            <a class="dropdown-toggle" href="#" data-toggle="dropdown">
              <img src="logo/nix-wiki.png" alt="NixOS" class="logo" />                NixOS              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu">
                            <li><a href="/">Site Home</a></li>
              <li><a href="nixos">NixOS</a></li>
              <li><a href="nix">Nix</a></li>
              <li><a href="nixpkgs">Nixpkgs</a></li>
            </ul>
          </li>

          <div class="nav-collapse collapse">              <ul class="nav pull-left">
                <li><a href="nixos/about.html">About</a></li>
                <li><a href="nixos/download.html">Download</a></li>
                <li><a href="nixos/learn.html">Learn</a></li>
                <li><a href="nixos/packages.html">Packages</a></li>
                <li><a href="nixos/options.html">Options</a></li>
                <li><a href="nixos/community.html">Community</a></li>
                <li><a href="nixos/security.html">Security</a></li>
              </ul>
              <ul class="nav pull-right">
                <!--
                <li><a href="https://github.com/NixOS/nixpkgs"><i class="fa fa-github"></i></a></li>
                <li><a href="https://twitter.com/nixos_org"><i class="fa fa-twitter"></i></a></li>
                -->
            </ul>
          </div>
        </div>
      </div>
    </div>

    <div class="container main">        <div class="page-header">
          <h1>Not found</h1>
        </div>
<p>
  The requested content <span class="at_url"></span> could not be found.
</p>

<script>
// Enhance the error message with the URL when possible.
$(function() {
  var url = window.location.pathname;
  $(".at_url").html(" at <tt class='url'></tt> ")
  	.children(".url").text(url)
})
</script>

      <div class="footer">
        <hr />
        <center>
        <small class="muted">        </small>
        </center>
      </div>

    </div>

    <script>
      $(document).ready(function() {
        $(".nixos-popover").popover({});
      });
    </script>

  </body>

</html>
