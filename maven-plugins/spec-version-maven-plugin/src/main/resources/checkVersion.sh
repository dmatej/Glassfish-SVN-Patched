#!/bin/sh

# TODO parse args.
args="-Dusage"

mvn ${project.groupId}:${project.artifactId}:${project.version}:check-version $args
