------------------------------------------------------------------------
Describes how to build xml-commons distributions and documentation.
------------------------------------------------------------------------

* The xml-commons distribution contains various utilities. Each utility
is also available as a separate distribution (currently only resolver).

* The top-level build.xml controls everything. It calls the separate
build files for each sub-project. The sub-project builds can also be
called direectly

* All documentation is built by Apache Forrest (so you need that locally
installed). See http://xml.apache.org/commons/howto-forrest.html
The website update cronjob happens via user dims at 01:00 and 13:00 GMT.

------------------------------------------------------------------------
Step 1
------
cd xml-commons
ant -projecthelp
... No point repeating the options here, but one hint:
site, validate, war, and webapp targets are part of Forrest.

ant all
- Builds the documentation, builds each sub-project, and packs the
distribution.

------------------------------------------------------------------------
Step 2
------
cd xml-commons/java
ant -buildfile resolver.xml dist
... Builds separate resolver distribution and copies over some relevant
Forrest-built docs.

------------------------------------------------------------------------
