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
package org.apache.myfaces.trinidadinternal.util;

import java.io.File;
import java.io.IOException;

import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;

@Deprecated
public final class URLUtils
{
  private URLUtils()
  {
  }

  public static long getLastModified(URL url) throws IOException
  {
    if ("file".equals(url.getProtocol()))
    {
      String externalForm = url.toExternalForm();
      // Remove the "file:"
      File file = new File(externalForm.substring(5));

      return file.lastModified();
    }
    else
    {
      return getLastModified(url.openConnection());
    }
  }

  public static long getLastModified(URLConnection connection) throws IOException
  {
    long modified;
    if (connection instanceof JarURLConnection)
    {
      // The following hack is required to work-around a JDK bug.
      // getLastModified() on a JAR entry URL delegates to the actual JAR file
      // rather than the JAR entry.
      // This opens internally, and does not close, an input stream to the JAR
      // file.
      // In turn, you cannot close it by yourself, because it's internal.
      // The work-around is to get the modification date of the JAR file
      // manually,
      // and then close that connection again.

      URL jarFileUrl = ((JarURLConnection) connection).getJarFileURL();
      URLConnection jarFileConnection = jarFileUrl.openConnection();

      try
      {
        modified = jarFileConnection.getLastModified();
      }
      finally
      {
        try
        {
          jarFileConnection.getInputStream().close();
        }
        catch (Exception exception)
        {
          // Ignored
        }
      }
    }
    else
    {
      modified = connection.getLastModified();
    }

    return modified;
  }
}