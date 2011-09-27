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
package org.apache.myfaces.trinidadinternal.share.io;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;

import org.apache.myfaces.trinidad.share.io.InputStreamProvider;


/**
 * An InputStreamProvider for loading files.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/io/FileInputStreamProvider.java#0 $) $Date: 10-nov-2005.19:00:07 $
 */
public class FileInputStreamProvider implements InputStreamProvider
{
  /**
   * Creates a FileInputStreamProvider.
   * @param file a file object, which must be non-null
   */
  public FileInputStreamProvider(File file)
  {
    if (file == null)
      throw new NullPointerException();

    _file = file;
  }

  public InputStream openInputStream() throws IOException
  {
    // Yes, there's a race condition here.  No, I don't believe
    // it can be fixed, or that it's important.
    _lastModified = _file.lastModified();
    return new BufferedInputStream(new FileInputStream(_file));
  }

  public String getDisplayName()
  {
    return _file.getName();
  }

  public Object getIdentifier()
  {
    return _file;
  }


  /**
   * Returns true if the underlying target has changed
   * since the last call to openInputStream()
   */
  public boolean hasSourceChanged()
  {
    return (_file.lastModified() != _lastModified);
  }


  public Object getCachedResult()
  {
    return _cached;
  }

  public void setCachedResult(Object value)
  {
    _cached = value;
  }

  private final File _file;
  private Object     _cached;
  private long       _lastModified = -1;
}
