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
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.UnsupportedEncodingException;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLDecoder;

import java.util.ArrayList;
import java.util.List;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;


/**
 */
public class XMLUtil {
    /**
     */
    public static String getRequiredTextValue(Element e, String tagName) {
        NodeList nodeList = e.getElementsByTagName(tagName);
        assert (nodeList.getLength() == 1);

        Node node = nodeList.item(0);

        if (node == null) {
            throw new RuntimeException("missing required element: " + tagName);
        }

        String s = nodeList.item(0).getTextContent();
        if (s != null) {
            try {
                s = URLDecoder.decode(nodeList.item(0).getTextContent(), "UTF-8");
            } catch (UnsupportedEncodingException uee) {
                s = nodeList.item(0).getTextContent();
            }
        }
        if (s == null) {
            return "";
        }
        return s;
    }

    /**
     */
    public static List<String> getOptionalTextValues(Element e, String tagName) {
        NodeList nodeList = e.getElementsByTagName(tagName);
        assert (nodeList.getLength() < 2);

        List<String> list = new ArrayList<String>();
        for (int i=0; i<nodeList.getLength(); i++) {
            Node node = nodeList.item(i);

            //return (node != null) ? URLDecoder.decode(node.getTextContent(),"UTF-8") : "";
            if (node == null) {
                continue;
            }
            String s = node.getTextContent();
            if (s != null) {
                try {
                    s = URLDecoder.decode(node.getTextContent(), "UTF-8");
                } catch (UnsupportedEncodingException uee) {
                    s = node.getTextContent();
                }
            }
            if (s == null || s.equalsIgnoreCase("null")) {
                continue;
            }
            list.add(s);
        }
        return list;
    }

    /**
     */
    public static String getOptionalTextValue(Element e, String tagName) {
        NodeList nodeList = e.getElementsByTagName(tagName);
        assert (nodeList.getLength() < 2);

        Node node = nodeList.item(0);

        //return (node != null) ? URLDecoder.decode(node.getTextContent(),"UTF-8") : "";
        if (node == null) {
            //return null;
            return "";
        }
        String s = node.getTextContent();
        if (s != null) {
            try {
                s = URLDecoder.decode(node.getTextContent(), "UTF-8");
            } catch (UnsupportedEncodingException uee) {
                s = node.getTextContent();
            }
        }
        if (s == null || s.equalsIgnoreCase("null")) {
            return "";
        }
        return s;
    }

    public static void writeDocument(Document doc, OutputStream out)
        throws IOException {
        TransformerFactory tfactory = TransformerFactory.newInstance();
        Transformer serializer;

        try {
            serializer = tfactory.newTransformer();
            serializer.setOutputProperty(OutputKeys.INDENT, "yes");
            serializer.transform(new DOMSource(doc), new StreamResult(out));
        } catch (TransformerException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

    public static void writeElement(Element e, OutputStream out)
        throws IOException {
        TransformerFactory tfactory = TransformerFactory.newInstance();
        Transformer serializer;

        try {
            serializer = tfactory.newTransformer();
            serializer.setOutputProperty(OutputKeys.INDENT, "yes");
            serializer.transform(new DOMSource(e), new StreamResult(out));
        } catch (TransformerException ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
    }
}
