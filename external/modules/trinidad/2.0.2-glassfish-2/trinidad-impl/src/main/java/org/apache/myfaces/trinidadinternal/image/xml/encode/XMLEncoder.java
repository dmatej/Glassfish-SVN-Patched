/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.image.xml.encode;

import java.io.PrintWriter;
import java.util.Map;

import org.apache.myfaces.trinidadinternal.image.ImageContext;

/**
 * Interface for XML encoders.  XML encoders transform property
 * dictionaries into XML output.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/encode/XMLEncoder.java#0 $) $Date: 10-nov-2005.19:04:47 $
 */
public interface XMLEncoder
{
  /**
   * Encode the specified properties into an XML output stream.
   */
  public void encodeXML(
    ImageContext context,
    String     namespaceURI,
    String     localName,
    Map<Object, Object> properties,
    Map<Object, Object> responseProperties,
    PrintWriter out
    );
}
