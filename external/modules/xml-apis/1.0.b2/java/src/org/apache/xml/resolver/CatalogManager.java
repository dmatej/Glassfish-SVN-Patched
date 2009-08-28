// CatalogManager.java - Access CatalogManager.properties

/*
 * The Apache Software License, Version 1.1
 *
 *
 * Copyright (c) 2001 The Apache Software Foundation.  All rights 
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:  
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Xalan" and "Apache Software Foundation" must
 *    not be used to endorse or promote products derived from this
 *    software without prior written permission. For written 
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache",
 *    nor may "Apache" appear in their name, without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation and was
 * originally based on software copyright (c) 2001, International
 * Business Machines Corporation., http://www.ibm.com.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

package org.apache.xml.resolver;

import java.lang.ClassNotFoundException;
import java.lang.IllegalAccessException;
import java.lang.InstantiationException;
import java.lang.SecurityException;
import java.lang.ClassCastException;
import java.net.URL;
import java.net.MalformedURLException;

import java.io.*;
import java.util.*;

/**
 * <p>CatalogManager provides an interface to the catalog properties.</p>
 *
 * <p>Properties can come from two places: from system properties or
 * from a <i>CatalogManager.properties</i> file. This class provides a transparent
 * interface to both, with system properties preferred over property file values.</p>
 *
 * <p>The following table summarizes the properties:</p>
 *
 * <table border="1">
 * <thead>
 * <tr>
 * <td>System Property</td>
 * <td>CatalogManager.properties<br/>Property</td>
 * <td>Description</td>
 * </tr>
 * </thead>
 * <tbody>
 * <tr>
 * <td>xml.catalog.ignoreMissing</td>
 * <td>&#160;</td>
 * <td>If true, a missing <i>CatalogManager.properties</i> file or missing properties
 * within that file will not generate warning messages. See also the
 * <i>ignoreMissingProperties</i> method.</td>
 * </tr>
 *
 * <tr>
 * <td>xml.catalog.files</td>
 * <td>catalogs</td>
 * <td>The <emph>semicolon-delimited</emph> list of catalog files.</td>
 * </tr>
 *
 * <tr>
 * <td>&#160;</td>
 * <td>relative-catalogs</td>
 * <td>If false, relative catalog URIs are made absolute with respect to the base URI of
 * the <i>CatalogManager.properties</i> file. This setting only applies to catalog
 * URIs obtained from the <i>catalogs</i> property <emph>in the</emph>
 * <i>CatalogManager.properties</i> file</td>
 * </tr>
 *
 * <tr>
 * <td>xml.catalog.verbosity</td>
 * <td>verbosity</td>
 * <td>If non-zero, the Catalog classes will print informative and debugging messages.
 * The higher the number, the more messages.</td>
 * </tr>
 *
 * <tr>
 * <td>xml.catalog.prefer</td>
 * <td>prefer</td>
 * <td>Which identifier is preferred, "public" or "system"?</td>
 * </tr>
 *
 * <tr>
 * <td>xml.catalog.staticCatalog</td>
 * <td>static-catalog</td>
 * <td>Should a single catalog be constructed for all parsing, or should a different
 * catalog be created for each parser?</td>
 * </tr>
 *
 * <tr>
 * <td>xml.catalog.allowPI</td>
 * <td>allow-oasis-xml-catalog-pi</td>
 * <td>If the source document contains "oasis-xml-catalog" processing instructions,
 * should they be used?</td>
 * </tr>
 *
 * <tr>
 * <td>xml.catalog.className</td>
 * <td>catalog-class-name</td>
 * <td>If you're using the convenience classes
 * <tt>org.apache.xml.resolver.tools.*</tt>), this setting
 * allows you to specify an alternate class name to use for the underlying
 * catalog.</td>
 * </tr>
 * </tbody>
 * </table>
 *
 * @see Catalog
 *
 * @author Norman Walsh
 * <a href="mailto:Norman.Walsh@Sun.COM">Norman.Walsh@Sun.COM</a>
 *
 * @version 1.0
 */

public class CatalogManager {
  private static String pFiles         = "xml.catalog.files";
  private static String pVerbosity     = "xml.catalog.verbosity";
  private static String pPrefer        = "xml.catalog.prefer";
  private static String pStatic        = "xml.catalog.staticCatalog";
  private static String pAllowPI       = "xml.catalog.allowPI";
  private static String pClassname     = "xml.catalog.className";
  private static String pIgnoreMissing = "xml.catalog.ignoreMissing";

  /** Flag to ignore missing property files and/or properties */
  private static boolean ignoreMissingProperties
    = (System.getProperty(pIgnoreMissing) != null
       || System.getProperty(pFiles) != null);

  /** Holds the resources after they are loaded from the file. */
  private static ResourceBundle resources;

  /** The name of the CatalogManager properties file. */
  private static String propertyFile = "CatalogManager.properties";

  /** The location of the propertyFile */
  private static URL propertyFileURI = null;

  /** Default catalog files list. */
  private static String defaultCatalogFiles = "./xcatalog";

  /** Default verbosity level if there is no property setting for it. */
  private static int defaultVerbosity = 1;

  /** Default preference setting. */
  private static boolean defaultPreferPublic = true;

  /** Default setting of the static catalog flag. */
  private static boolean defaultStaticCatalog = true;

  /** Default setting of the oasisXMLCatalogPI flag. */
  private static boolean defaultOasisXMLCatalogPI = true;

  /** Default setting of the relativeCatalogs flag. */
  private static boolean defaultRelativeCatalogs = true;

  /**
   * <p>Load the properties from the propertyFile and build the
   * resources from it.</p>
   */
  private synchronized static void readProperties() {
    try {
      propertyFileURI = CatalogManager.class.getResource("/"+propertyFile);
      InputStream in =
	CatalogManager.class.getResourceAsStream("/"+propertyFile);
      if (in==null) {
	if (!ignoreMissingProperties) {
	  System.err.println("Cannot find "+propertyFile);
	}
	return;
      }
      resources = new PropertyResourceBundle(in);
    } catch (MissingResourceException mre) {
      if (!ignoreMissingProperties) {
	System.err.println("Cannot read "+propertyFile);
      }
    } catch (java.io.IOException e) {
      if (!ignoreMissingProperties) {
	System.err.println("Failure trying to read "+propertyFile);
      }
    }
  }

  /**
   * <p>Tell the CatalogManager how to handle missing properties</p>
   *
   * <p>If ignore is true, missing or unreadable property files will
   * not be reported. Otherwise, a message will be sent to System.err.
   * </p>
   */
  public static void ignoreMissingProperties(boolean ignore) {
    ignoreMissingProperties = ignore;
  }

  /**
   * <p>Obtain the verbosity setting from the properties.</p>
   *
   * @returns The verbosity level from the propertyFile or the
   * defaultVerbosity.
   */
  public static int verbosity () {
    String verbStr = System.getProperty(pVerbosity);

    if (verbStr == null) {
      if (resources==null) readProperties();
      if (resources==null) return defaultVerbosity;
      try {
	verbStr = resources.getString("verbosity");
      } catch (MissingResourceException e) {
	return defaultVerbosity;
      }
    }

    try {
      int verb = Integer.parseInt(verbStr.trim());
      return verb;
    } catch (Exception e) {
      System.err.println("Cannot parse verbosity: \"" + verbStr + "\"");
      return defaultVerbosity;
    }
  }

  /**
   * <p>Obtain the relativeCatalogs setting from the properties.</p>
   *
   * <p>This property is used when the catalogFiles property is
   * interrogated. If true, then relative catalog entry file names
   * are returned. If false, relative catalog entry file names are
   * made absolute with respect to the properties file before returning
   * them.</p>
   *
   * <p>This property <emph>only applies</emph> when the catalog files
   * come from a properties file. If they come from a system property or
   * the default list, they are never considered relative. (What would
   * they be relative to?)</p>
   *
   * <p>In the properties, a value of 'yes', 'true', or '1' is considered
   * true, anything else is false.</p>
   *
   * @returns The relativeCatalogs setting from the propertyFile or the
   * defaultRelativeCatalogs.
   */
  public static boolean relativeCatalogs () {
    if (resources==null) readProperties();

    if (resources==null) return defaultRelativeCatalogs;

    try {
      String allow = resources.getString("relative-catalogs");
      return (allow.equalsIgnoreCase("true")
	      || allow.equalsIgnoreCase("yes")
	      || allow.equalsIgnoreCase("1"));
    } catch (MissingResourceException e) {
      return defaultRelativeCatalogs;
    }
  }

  /**
   * <p>Obtain the list of catalog files from the properties.</p>
   *
   * <p>Note that the list of catalog files is always a semicolon
   * delimited list, even on Unix systems where a colon delimited list
   * might be considered more natural.</p>
   *
   * @returns A vector of the catalog file names or null if no catalogs
   * are available in the properties.
   */
  public static Vector catalogFiles () {
    String catalogList = System.getProperty(pFiles);
    boolean fromPropertiesFile = false;

    if (catalogList == null) {
      if (resources == null) readProperties();
      if (resources != null) {
	try {
	  catalogList = resources.getString("catalogs");
	  fromPropertiesFile = true;
	} catch (MissingResourceException e) {
	  System.err.println(propertyFile + ": catalogs not found.");
	  catalogList = null;
	}
      }
    }

    if (catalogList == null) {
      catalogList = defaultCatalogFiles;
    }

    StringTokenizer files = new StringTokenizer(catalogList, ";");
    Vector catalogs = new Vector();
    while (files.hasMoreTokens()) {
      String catalogFile = files.nextToken();
      URL absURI = null;

      if (fromPropertiesFile && !relativeCatalogs()) {
	try {
	  absURI = new URL(propertyFileURI, catalogFile);
	  catalogFile = absURI.toString();
	} catch (MalformedURLException mue) {
	  absURI = null;
	}
      }

      catalogs.add(catalogFile);
    }
    return catalogs;
  }

  /**
     * <p>Obtain the preferPublic setting from the properties.</p>
     *
     * <p>In the properties, a value of 'public' is true,
     * anything else is false.</p>
     *
     * @returns True if prefer is public or the
     * defaultPreferSetting.
     */
  public static boolean preferPublic () {
    String prefer = System.getProperty(pPrefer);

    if (prefer == null) {
      if (resources==null) readProperties();
      if (resources==null) return defaultPreferPublic;
      try {
	prefer = resources.getString("prefer");
      } catch (MissingResourceException e) {
	return defaultPreferPublic;
      }
    }

    if (prefer == null) {
      return defaultPreferPublic;
    }

    return (prefer.equalsIgnoreCase("public"));
  }

  /**
   * <p>Obtain the static-catalog setting from the properties.</p>
   *
   * <p>In the properties, a value of 'yes', 'true', or '1' is considered
   * true, anything else is false.</p>
   *
   * @returns The static-catalog setting from the propertyFile or the
   * defaultStaticCatalog.
   */
  public static boolean staticCatalog () {
    String staticCatalog = System.getProperty(pStatic);

    if (staticCatalog == null) {
      if (resources==null) readProperties();
      if (resources==null) return defaultStaticCatalog;
      try {
	staticCatalog = resources.getString("static-catalog");
      } catch (MissingResourceException e) {
	return defaultStaticCatalog;
      }
    }

    if (staticCatalog == null) {
      return defaultStaticCatalog;
    }

    return (staticCatalog.equalsIgnoreCase("true")
	    || staticCatalog.equalsIgnoreCase("yes")
	    || staticCatalog.equalsIgnoreCase("1"));
  }

  /**
   * <p>Obtain the oasisXMLCatalogPI setting from the properties.</p>
   *
   * <p>In the properties, a value of 'yes', 'true', or '1' is considered
   * true, anything else is false.</p>
   *
   * @returns The oasisXMLCatalogPI setting from the propertyFile or the
   * defaultOasisXMLCatalogPI.
   */
  public static boolean allowOasisXMLCatalogPI () {
    String allow = System.getProperty(pAllowPI);

    if (allow == null) {
      if (resources==null) readProperties();
      if (resources==null) return defaultOasisXMLCatalogPI;
      try {
	allow = resources.getString("allow-oasis-xml-catalog-pi");
      } catch (MissingResourceException e) {
	return defaultOasisXMLCatalogPI;
      }
    }

    if (allow == null) {
      return defaultOasisXMLCatalogPI;
    }

    return (allow.equalsIgnoreCase("true")
	    || allow.equalsIgnoreCase("yes")
	    || allow.equalsIgnoreCase("1"));
  }

  /**
   * <p>Obtain the Catalog class name setting from the properties.</p>
   *
   */
  public static String catalogClassName () {
    String className = System.getProperty(pClassname);

    if (className == null) {
      if (resources==null) readProperties();
      if (resources==null) return null;
      try {
	return resources.getString("catalog-class-name");
      } catch (MissingResourceException e) {
	return null;
      }
    }

    return className;
  }
}
