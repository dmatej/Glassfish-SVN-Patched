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
package org.apache.myfaces.trinidadinternal.image.encode;

import java.awt.Image;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Interface for classes which encode images.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/ImageEncoder.java#0 $) $Date: 10-nov-2005.19:05:17 $
 */
public interface ImageEncoder
{
  /**
   * Encodes the image to the specified output stream.
   * <p>
   * =-=ags Should probably throw something in addition to IOException
   *   to handle other problems
   *
   * @param image The image to encode
   * @param out The output stream to which the image should be encoded
   *
   */
  public void encodeImage(Image image, OutputStream out)
    throws IOException;
}
