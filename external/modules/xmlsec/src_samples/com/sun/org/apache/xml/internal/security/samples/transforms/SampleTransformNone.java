
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
package com.sun.org.apache.xml.internal.security.samples.transforms;



import com.sun.org.apache.xml.internal.security.signature.XMLSignatureInput;
import com.sun.org.apache.xml.internal.security.transforms.Transform;
import com.sun.org.apache.xml.internal.security.transforms.TransformSpi;


/**
 * Implements a null transform which leaved the input unmodified.
 *
 * @author Christian Geuer-Pollmann
 */
public class SampleTransformNone extends TransformSpi {

   /** {@link java.util.logging} logging facility */
    static java.util.logging.Logger log = 
        java.util.logging.Logger.getLogger(
                    SampleTransformNone.class.getName());

   /** Field implementedTransformURI */
   public static final String implementedTransformURI =
      "http://www.xmlsecurity.org/NS/Transforms#none";

   /**
    * Method engineGetURI
    *
    *
    */
   protected String engineGetURI() {
      return SampleTransformNone.implementedTransformURI;
   }

   //J-
   public boolean wantsOctetStream ()   { return true; }
   public boolean wantsNodeSet ()       { return true; }
   public boolean returnsOctetStream () { return true; }
   public boolean returnsNodeSet ()     { return true; }
   //J+

   /**
    * Method enginePerformTransform
    *
    * @param input
    *
    */
   protected XMLSignatureInput enginePerformTransform(XMLSignatureInput input, Transform _transformObject) {
      return input;
   }

   static {
      com.sun.org.apache.xml.internal.security.Init.init();
   }
}
