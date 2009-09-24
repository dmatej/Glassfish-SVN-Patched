#!/bin/sh

cvs checkout -r SJSAS91_FCS_BRANCH glassfish/appserv-native glassfish/bootstrap/make/defines.mk
cp glassfish/bootstrap/make/defines.mk glassfish/appserv-native/make/defines-bootstrap.mk
