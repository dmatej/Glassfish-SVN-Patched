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
package org.apache.myfaces.trinidadinternal.image.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.io.UnsupportedEncodingException;



/**
 * File utility methods
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/util/FileUtils.java#0 $) $Date: 10-nov-2005.19:04:48 $
 */
public class FileUtils
{
  private FileUtils() {}

  /**
   * Returns a Reader for reading the UTF-8 encoded file at the specified
   * path.
   */
  static public Reader getUTF8Reader(String path)
    throws FileNotFoundException
  {
    FileInputStream in = new FileInputStream(path);
    InputStreamReader reader = null;

    try
    {
      reader = new InputStreamReader(in, _UTF8_ENCODING);
    }
    catch (UnsupportedEncodingException e)
    {
      // UTF-8 should always be supported!
      assert false;
      return null;
    }

    return new BufferedReader(reader);
  }

  /**
   * Returns a Writer for writing UTF-8 encoded data to the file at the
   * specified path.
   */
  static public Writer getUTF8Writer(String path)
    throws IOException
  {
    FileOutputStream out = new FileOutputStream(path);
    OutputStreamWriter writer = null;

    try
    {
      writer = new OutputStreamWriter(out, _UTF8_ENCODING);
    }
    catch (UnsupportedEncodingException e)
    {
      // UTF-8 should always be supported!
      assert false;
      return null;
    }

    return new BufferedWriter(writer);
  }

  static private String _UTF8_ENCODING = "UTF8";
}
