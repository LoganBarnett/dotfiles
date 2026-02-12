{ bash, jira-cli-go, writeShellApplication, ... }: let
  name = "jira-se-task";
  script = name;
in writeShellApplication {
  inherit name;
  runtimeInputs = [ bash jira-cli-go ];
  text = builtins.readFile ../scripts/${script};
}
