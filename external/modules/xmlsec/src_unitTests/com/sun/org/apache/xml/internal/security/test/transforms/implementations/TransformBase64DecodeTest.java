/*
 * Copyright  1999-2004 The Apache Software Foundation.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */
package com.sun.org.apache.xml.internal.security.test.transforms.implementations;



import java.io.ByteArrayInputStream;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import com.sun.org.apache.xml.internal.security.c14n.CanonicalizationException;
import com.sun.org.apache.xml.internal.security.c14n.InvalidCanonicalizerException;
import com.sun.org.apache.xml.internal.security.exceptions.XMLSecurityException;
import com.sun.org.apache.xml.internal.security.signature.XMLSignatureException;
import com.sun.org.apache.xml.internal.security.signature.XMLSignatureInput;
import com.sun.org.apache.xml.internal.security.test.TestUtils;
import com.sun.org.apache.xml.internal.security.transforms.InvalidTransformException;
import com.sun.org.apache.xml.internal.security.transforms.TransformationException;
import com.sun.org.apache.xml.internal.security.transforms.Transforms;
import com.sun.org.apache.xml.internal.security.transforms.implementations.TransformBase64Decode;
import com.sun.org.apache.xml.internal.security.utils.Constants;
import com.sun.org.apache.xml.internal.security.utils.XMLUtils;
import com.sun.org.apache.xpath.internal.XPathAPI;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;


/**
 * Unit test for {@link com.sun.org.apache.xml.internal.security.transforms.implementations.TransformBase64Decode}
 *
 * @author Christian Geuer-Pollmann
 */
public class TransformBase64DecodeTest extends TestCase {

   /** {@link java.util.logging} logging facility */
    static java.util.logging.Logger log = 
        java.util.logging.Logger.getLogger(
                    TransformBase64DecodeTest.class.getName());

   /**
    * Method suite
    *
    *
    */
   public static Test suite() {
      return new TestSuite(TransformBase64DecodeTest.class);
   }

   /**
    * Constructor TransformBase64DecodeTest
    *
    * @param Name_
    */
   public TransformBase64DecodeTest(String Name_) {
      super(Name_);
   }

   /**
    * Method main
    *
    * @param args
    */
   public static void main(String[] args) {

      String[] testCaseName = { "-noloading",
                                TransformBase64DecodeTest.class.getName() };

      junit.textui.TestRunner.main(testCaseName);
   }

   private static Document createDocument() throws ParserConfigurationException {

      DocumentBuilderFactory dfactory = DocumentBuilderFactory.newInstance();

      dfactory.setNamespaceAware(true);

      DocumentBuilder db = dfactory.newDocumentBuilder();
      Document doc = db.newDocument();

      if (doc == null) {
          throw new RuntimeException("Could not create a Document");
      } else {
         log.log(java.util.logging.Level.FINE, "I could create the Document");
      }
      return doc;
   }

   /**
    * Method createElement
    *
    *
    * @throws ParserConfigurationException
    */
   private static Element createElement() throws ParserConfigurationException {

      Document doc = TransformBase64DecodeTest.createDocument();

      Element element = XMLUtils.createElementInSignatureSpace(doc, Constants._TAG_TRANSFORMS);

      return element;
   }

   /**
    * Method test1
    *
    * @throws CanonicalizationException
    * @throws IOException
    * @throws InvalidCanonicalizerException
    * @throws InvalidTransformException
    * @throws NotYetImplementedException
    * @throws ParserConfigurationException
    * @throws TransformationException
    * @throws XMLSecurityException
    * @throws XMLSignatureException
    */
   public static void test1()
           throws IOException, InvalidTransformException,
                  CanonicalizationException, InvalidCanonicalizerException,
                  TransformationException,
                  XMLSignatureException, XMLSecurityException,
                  ParserConfigurationException {

      // base64 encoded
      String s1 =
         "VGhlIFVSSSBvZiB0aGUgdHJhbnNmb3JtIGlzIGh0dHA6Ly93d3cudzMub3JnLzIwMDAvMDkveG1s\n"
         + "ZHNpZyNiYXNlNjQ=";

      // base64 encoded twice
      String s2 =
         "VkdobElGVlNTU0J2WmlCMGFHVWdkSEpoYm5ObWIzSnRJR2x6SUdoMGRIQTZMeTkzZDNjdWR6TXVi\n"
         + "M0puTHpJd01EQXZNRGt2ZUcxcwpaSE5wWnlOaVlYTmxOalE9";
      Document doc = TransformBase64DecodeTest.createDocument();
      Transforms t = new Transforms(doc);
      doc.appendChild(t.getElement());
      t.addTransform(TransformBase64Decode.implementedTransformURI);

      XMLSignatureInput in =
         new XMLSignatureInput(new ByteArrayInputStream(s1.getBytes()));
      XMLSignatureInput out = t.performTransforms(in);
      String result = new String(out.getBytes());

      assertTrue(
         result.equals(
            "The URI of the transform is http://www.w3.org/2000/09/xmldsig#base64"));
   }

   /**
    * Method testTwice
    *
    * @throws CanonicalizationException
    * @throws IOException
    * @throws InvalidCanonicalizerException
    * @throws InvalidTransformException
    * @throws NotYetImplementedException
    * @throws ParserConfigurationException
    * @throws TransformationException
    * @throws XMLSecurityException
    * @throws XMLSignatureException
    */
   public static void test2()
           throws IOException, InvalidTransformException,
                  TransformationException, CanonicalizationException,
                  InvalidCanonicalizerException,
                  XMLSignatureException, XMLSecurityException,
                  ParserConfigurationException {

      // base64 encoded twice
      String s2 =
         "VkdobElGVlNTU0J2WmlCMGFHVWdkSEpoYm5ObWIzSnRJR2x6SUdoMGRIQTZMeTkzZDNjdWR6TXVi\n"
         + "M0puTHpJd01EQXZNRGt2ZUcxcwpaSE5wWnlOaVlYTmxOalE9";
      Document doc = TransformBase64DecodeTest.createDocument();
      Transforms t = new Transforms(doc);
      doc.appendChild(t.getElement());

      t.addTransform(TransformBase64Decode.implementedTransformURI);

      XMLSignatureInput in =
         new XMLSignatureInput(new ByteArrayInputStream(s2.getBytes()));
      XMLSignatureInput out = t.performTransforms(t.performTransforms(in));
      String result = new String(out.getBytes());

      assertTrue(
         result.equals(
            "The URI of the transform is http://www.w3.org/2000/09/xmldsig#base64"));
   }

   /**
    * Method test3
    *
    * @throws Exception
    */
   public static void test3() throws Exception {
      //J-
      String input = ""
         + "<Object xmlns:signature='http://www.w3.org/2000/09/xmldsig#'>\n"
         + "<signature:Base64>\n"
         + "VGhlIFVSSSBvZiB0aGU   gdHJhbn<RealText>Nmb  3JtIGlzIG<test/>h0dHA6</RealText>Ly93d3cudzMub3JnLzIwMDAvMDkveG1s\n"
         + "ZHNpZyNiYXNlNjQ=\n"
         + "</signature:Base64>\n"
         + "</Object>\n"
         ;
      //J+
      DocumentBuilderFactory dfactory = DocumentBuilderFactory.newInstance();

      dfactory.setNamespaceAware(true);

      DocumentBuilder db = dfactory.newDocumentBuilder();

      db.setErrorHandler(new com.sun.org.apache.xml.internal.security.utils
         .IgnoreAllErrorHandler());

      Document doc = db.parse(new ByteArrayInputStream(input.getBytes()));
      //XMLUtils.circumventBug2650(doc);
      Element nscontext = TestUtils.createDSctx(doc, "ds", Constants.SignatureSpecNS);

      Node base64Node = XPathAPI.selectSingleNode(doc, "//ds:Base64", nscontext);
      XMLSignatureInput xmlinput = new XMLSignatureInput(base64Node);

      Document doc2 = TransformBase64DecodeTest.createDocument();
      Transforms t = new Transforms(doc2);
      doc2.appendChild(t.getElement());
      t.addTransform(Transforms.TRANSFORM_BASE64_DECODE);

      XMLSignatureInput out = t.performTransforms(xmlinput);
      String result = new String(out.getBytes());

      assertTrue("\"" + result + "\"", result.equals(
            "The URI of the transform is http://www.w3.org/2000/09/xmldsig#base64"));
   }

   static {
      com.sun.org.apache.xml.internal.security.Init.init();
   }
}
