#!/bin/bash
#
# DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
#
# Copyright (c) 2010-2012 Oracle and/or its affiliates. All rights reserved.
#
# The contents of this file are subject to the terms of either the GNU
# General Public License Version 2 only ("GPL") or the Common Development
# and Distribution License("CDDL") (collectively, the "License").  You
# may not use this file except in compliance with the License.  You can
# obtain a copy of the License at
# https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
# or packager/legal/LICENSE.txt.  See the License for the specific
# language governing permissions and limitations under the License.
#
# When distributing the software, include this License Header Notice in each
# file and include the License file at packager/legal/LICENSE.txt.
#
# GPL Classpath Exception:
# Oracle designates this particular file as subject to the "Classpath"
# exception as provided by Oracle in the GPL Version 2 section of the License
# file that accompanied this code.
#
# Modifications:
# If applicable, add the following below the License Header, with the fields
# enclosed by brackets [] replaced by your own identifying information:
# "Portions Copyright [year] [name of copyright owner]"
#
# Contributor(s):
# If you wish your version of this file to be governed by only the CDDL or
# only the GPL Version 2, indicate your decision by adding "[Contributor]
# elects to include this software in this distribution under the [CDDL or GPL
# Version 2] license."  If you don't indicate a single choice of license, a
# recipient has the option to distribute your version of this file under
# either the CDDL, the GPL Version 2 or to extend the choice of license to
# its licensees as provided above.  However, if you add GPL Version 2 code
# and therefore, elected the GPL Version 2 license, then the option applies
# only if the new code is made subject to such option by the copyright
# holder.
#


The scripts contained here are used to collect statistics on paramaeters like number of OSGI modules
and their states, startup time and the memory consumed when any distribution of glassfish is run.

Below is a brief explanation on how to run the scripts and the expected parameters to be provided.

GF_HOME = Glassfish home to the depth ~/../../glassfish3/glassfish
TARGET_DIRECTORY = Directory path where results need to be populated
STATS_XML_FILE_NAME = XML file name for collecting OSGI module statistics
TIME_XML_FILE_NAME = XML file name for collecting startup time statistics
MEMORY_XML_FILE_NAME = XML file name for collecting memory statistics
LOG_FILE_NAME = log file name
APP_PATH = path to the archive that needs to be deployed
ID = Any unique identifier for the run
FILE_NAME_MATCH = match the file name for which aggregate report is needed
TYPE = type for which report is needed; allowed types are "MODULE/TIME/MEMORY/STATE"
OUTPUT_DIR = output directory for aggregated reports
OUTPUT_XML_FILE_NAME = file name for aggregated report
OUTPUT_HTML_FILE_NAME = file name for HTML graph visulaization

1. startup_stats.sh

The script collects parameters on OSGI modules, startup time and memory when Glassfish starts up.

Usage: ./startup_stats.sh GF_HOME ID(REVISION) TARGET_DIRECTORY STATS_XML_FILE_NAME TIME_XML_FILE_NAME MEMORY_XML_FILE_NAME LOG_FILE_NAME

2. deploy_stats.sh

The script collects parameters on OSGI modules, deployment time and memory when an application is deployed.

Usage: ./deploy_stats.sh GF_HOME ID(REVISION) TARGET_DIRECTORY STATS_XML_FILE_NAME TIME_XML_FILE_NAME MEMORY_XML_FILE_NAME LOG_FILE_NAME APP_PATH

3. generate_xml_html.sh

The script collects parameters from all previous runs and prepares a summary HTML report of all parameters and also graphic visualization of the same.

Usage: ./generate_xml_html.sh TARGET_DIRECTORY FILE_NAME_MATCH TYPE OUTPUT_DIR OUTPUT_XML_FILE_NAME OUTPUT_HTML_FILE_NAME

Sample Applications are also bundled here for usage with script deploy_stats.sh. The bundled applications includes a simple webapp, an EJB app and also 
a full fledged enterprise application.
