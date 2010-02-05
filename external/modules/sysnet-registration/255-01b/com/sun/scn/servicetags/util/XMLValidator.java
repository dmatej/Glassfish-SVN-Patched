/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */

package com.sun.scn.servicetags.util;

import org.w3c.dom.Document;

import org.xml.sax.SAXException;

import java.io.*;

import javax.xml.XMLConstants;
import javax.xml.parsers.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.validation.*;


/**
 */
public class XMLValidator {
    public boolean validate(Document doc, String schemaFile) {
        boolean isValid = false;

        try {
            SchemaFactory factory = SchemaFactory.newInstance(
                    XMLConstants.W3C_XML_SCHEMA_NS_URI);
                    //"http://www.w3.org/2001/XMLSchema");

            File schemaLocation = new File(schemaFile);
            Schema schema = factory.newSchema(schemaLocation);

            Validator validator = schema.newValidator();

            DOMSource source = new DOMSource(doc);
            validator.validate(source);
            //System.err.println("document is valid.");
            isValid = true;
        } catch (SAXException se) {
            isValid = false;
            //System.err.println("document is not valid because ");
            //System.err.println(se.getMessage());
            //se.printStackTrace();
        } catch (IOException ioe) {
            isValid = false;
            //System.err.println(ioe.getMessage());
            //ioe.printStackTrace();
        }

        return isValid;
    }

    public static void main(String[] args) {
        try {
            DocumentBuilderFactory domFactory = DocumentBuilderFactory.newInstance();
            domFactory.setNamespaceAware(true); // never forget this

            DocumentBuilder builder = domFactory.newDocumentBuilder();
            Document doc = builder.parse(new File(args[0]));
            XMLValidator validator = new XMLValidator();
            validator.validate(doc, args[1]);
            System.exit(0);
        } catch (Exception ex) {
            //System.err.println("document is not valid because ");
            System.err.println(ex.getMessage());
            ex.printStackTrace();
        }
    }
}
