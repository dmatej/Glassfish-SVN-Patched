#!/bin/sh

# Shell for configuring environment.
# Some servers/tests may require different JVM or configuration.
# set ANT_HOME=/scratch/ant; export ANT_HOME

# set JAVA_HOME=/scratch/jdk1.5.0_06; export JAVA_HOME
# set JAVA_HOME=/scratch/jdk1.6.0; export JAVA_HOME

# Weblogic config
# WL_HOME=/scratch/bea/wlserver_10.0; export WL_HOME
# JAVA_HOME=/scratch/bea/jrockit_150_11; export JAVA_HOME
WL_HOME=/scratch/wls/103.4/wlserver_10.3; export WL_HOME
JAVA_HOME=/scratch/wls/103.4/jrockit_160_20_R28.0.2-1452; export JAVA_HOME
