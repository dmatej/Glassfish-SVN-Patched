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



import java.io.ByteArrayInputStream;

import com.sun.org.apache.xml.internal.security.signature.XMLSignatureInput;
import com.sun.org.apache.xml.internal.security.utils.resolver.ResourceResolverSpi;
import org.w3c.dom.Attr;


/**
 *
 *
 * @author $Author: mullan $
 */
public class MyResolver extends ResourceResolverSpi {

   /**
    * Method engineResolve
    * @inheritDoc
    * @param uri
    * @param BaseURI
    *
    */
   public XMLSignatureInput engineResolve(Attr uri, String BaseURI) {


         ByteArrayInputStream is =
            new ByteArrayInputStream("string".getBytes());

         return new XMLSignatureInput(is);
      
   }

   /**
    * Method engineCanResolve
    * @inheritDoc
    * @param uri
    * @param BaseURI
    *
    */
   public boolean engineCanResolve(Attr uri, String BaseURI) {
      return true;
   }
}
