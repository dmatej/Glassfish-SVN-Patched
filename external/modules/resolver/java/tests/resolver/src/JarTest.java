// ManagerTest.java - A test of multiple Catalog Managers

/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2001-2003 The Apache Software Foundation.  All rights
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
 * 4. The names "Apache" and "Apache Software Foundation" must
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
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Vector;
import java.util.Date;
import java.util.GregorianCalendar;
import java.net.MalformedURLException;

import org.xml.sax.*;
import org.xml.sax.helpers.*;

import javax.xml.parsers.*;

import org.apache.xml.resolver.tools.ResolvingXMLReader;
import org.apache.xml.resolver.Catalog;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.helpers.Debug;
import org.apache.xml.resolver.apps.XParseError;

/**
 * <p>A simple command-line XML parsing application.</p>
 *
 * <p>This class tests the ability of the XML Catalog code to support
 * multiple catalog manager instances.
 * </p>
 *
 * <p>Usage: ManagerTest document.xml</p>
 *
 * <p>The process ends with error-level 1, if there errors.</p>
 *
 * @see org.apache.xml.resolver.tools.ResolvingXMLReader
 *
 * @author Norman Walsh
 * <a href="mailto:Norman.Walsh@Sun.COM">Norman.Walsh@Sun.COM</a>
 *
 * @version 1.0
 */
public class JarTest extends TestParser
{
  /** The main entry point */
  public static void main (String[] args)
    throws FileNotFoundException, IOException {

    for (int i=0; i<args.length; i++) {
      if (args[i].equals("-c")) {
	++i;
	catalogFiles.add(args[i]);
	continue;
      }

      xmlfile = args[i];
    }

    if (xmlfile == null) {
      System.out.println("Usage: JarTest [ -c catalog ] xmlfile");
      System.exit(1);
    }

    System.out.println("Creating my CatalogManager (jar catalog)");
    CatalogManager myManager = new CatalogManager();
    myManager.setUseStaticCatalog(false);
    myManager.setVerbosity(5);
    System.out.println("Parsing with my CatalogManager");
    ResolvingXMLReader reader = new ResolvingXMLReader(myManager);
    parse(reader);
  }
}
