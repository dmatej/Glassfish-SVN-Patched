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
package com.sun.org.apache.xml.internal.security.samples;



import java.io.File;
import java.io.FileInputStream;

import com.sun.org.apache.xml.internal.security.signature.XMLSignature;
import com.sun.org.apache.xml.internal.security.utils.Constants;
import com.sun.org.apache.xpath.internal.CachedXPathAPI;
import org.w3c.dom.Element;


/**
 *
 * @author $Author: mullan $
 */
public class AxisVerifier {

   /**
    * Method main
    *
    * @param unused
    * @throws Exception
    */
   public static void main(String unused[]) throws Exception {

      com.sun.org.apache.xml.internal.security.Init.init();

      File signatureFile = new File(AxisSigner.AXIS_SIGNATURE_FILENAME);
      javax.xml.parsers.DocumentBuilderFactory dbf =
         javax.xml.parsers.DocumentBuilderFactory.newInstance();

      dbf.setNamespaceAware(true);

      javax.xml.parsers.DocumentBuilder db = dbf.newDocumentBuilder();
      org.w3c.dom.Document doc = db.parse(new FileInputStream(signatureFile));
      String BaseURI = signatureFile.toURL().toString();
      CachedXPathAPI xpathAPI = new CachedXPathAPI();
      Element nsctx = doc.createElementNS(null, "nsctx");

      nsctx.setAttributeNS(Constants.NamespaceSpecNS, "xmlns:ds",
                           Constants.SignatureSpecNS);

      Element signatureElem = (Element) xpathAPI.selectSingleNode(doc,
                                 "//ds:Signature", nsctx);
      XMLSignature sig = new XMLSignature(signatureElem, BaseURI);
      boolean verify = sig.checkSignatureValue(sig.getKeyInfo().getPublicKey());

      System.out.println("The signature is" + (verify
                                               ? " "
                                               : " not ") + "valid");

      for (int i = 0; i < sig.getSignedInfo().getSignedContentLength(); i++) {
         boolean thisOneWasSigned =
            sig.getSignedInfo().getVerificationResult(i);

         if (thisOneWasSigned) {
            System.out.println("--- Signed Content follows ---");
            System.out
               .println(new String(sig.getSignedInfo()
                  .getSignedContentItem(i)));
         }
      }

      System.out.println("");
      System.out.println("Prior transforms");
      System.out
         .println(new String(sig.getSignedInfo()
            .getReferencedContentBeforeTransformsItem(0).getBytes()));
   }
}
