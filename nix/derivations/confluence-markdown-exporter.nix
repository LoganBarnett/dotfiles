{
  fetchFromGitHub,
  fetchPypi,
  python3Packages,
  lib,
  ...
}: let
  pname = "confluence-markdown-exporter";
  version = "3.0.5";
  pypkgs = python3Packages;
in pypkgs.buildPythonApplication {
  inherit pname version;

  format = "pyproject";

  src = fetchFromGitHub {
    owner = "Spenhouet";
    repo = "confluence-markdown-exporter";
    rev = version;
    hash = "sha256-t6/xgN7gGxWW+in8YKnc+DEgxFtFSBfo2x6mEDnsnmM=";
  };

  propagatedBuildInputs = [
    pypkgs.atlassian-python-api
    pypkgs.beautifulsoup4
    pypkgs.click
    pypkgs.hatchling
    pypkgs.markdownify
    pypkgs.pyyaml
    pypkgs.pydantic-settings
    pypkgs.python-dateutil
    pypkgs.questionary
    pypkgs.requests
    pypkgs.tabulate
    pypkgs.tqdm
    pypkgs.typer
  ];

  meta = {
    description = "Export Confluence pages to Markdown";
    homepage = "https://github.com/Spenhouet/confluence-markdown-exporter";
    license = lib.licenses.mit;
    # maintainers = with lib.maintainers; [ your-name-here ];
  };
}
