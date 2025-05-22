#!/usr/bin/env bash

################################################################################
# Submit a test alert to the alerts matrix room.
################################################################################
# Other settings might be better matched to reality, so we can test things like
# linking.
# This might be better parameterized.
prometheus_host='prometheus.proton'
secret="$(cat /run/agenix/matrix-alertmanager-secret)"
curl \
  --verbose \
  --request POST \
  --header "Content-Type: application/json" \
  "http://localhost:3001/alerts?secret=$secret" \
  --data '
{
  "receiver": "team-admins",
  "status": "firing",
  "alerts": [
    {
      "status": "firing",
      "labels": {
        "alertname": "FakeTestAlert",
        "severity": "page",
        "instance": "'"$prometheus_host"'"
      },
      "annotations": {
        "summary": "This is a test alert",
        "description": "This alert was sent manually to test Matrix integration"
      },
      "startsAt": "2025-05-22T00:00:00Z",
      "endsAt": "2025-05-22T01:00:00Z",
      "generatorURL": "https://'"$prometheus_host"'/graph"
    }
  ],
  "groupLabels": {
    "alertname": "FakeTestAlert"
  },
  "commonLabels": {
    "alertname": "FakeTestAlert",
    "severity": "page",
    "instance": "fakehost.local"
  },
  "commonAnnotations": {
    "summary": "This is a test alert",
    "description": "This alert was sent manually to test Matrix integration"
  },
  "externalURL": "http://alertmanager.proton"
}
'
