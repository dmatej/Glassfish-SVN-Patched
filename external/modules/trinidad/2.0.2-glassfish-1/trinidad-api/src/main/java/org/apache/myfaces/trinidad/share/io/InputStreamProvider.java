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
package org.apache.myfaces.trinidad.share.io;

import java.io.InputStream;
import java.io.IOException;

/**
 * InputStreamProviders encapsulate a single target file. An InputStreamProvider is used to 
 * get an inputStream, cache results and see if the file has been modified. 
 * (There's no real requirement that there be a physical file
 * at the target location).
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/io/InputStreamProvider.java#0 $) $Date: 10-nov-2005.19:00:08 $
 */
public interface InputStreamProvider
{
  /**
   * Return an InputStream for the target.  This function
   * should never return null - if a stream cannot be opened,
   * throw an IOException.
   */
  public InputStream       openInputStream() throws IOException;

  /**
   * Returns the name of the target location, suitable
   * for user display.
   */
  public String            getDisplayName();

  /**
   * Returns an identifier object that uniquely
   * identifies the target location. If two providers
   * return equal identifiers, that is, given:
   * <pre>
   *   Object identifierA = providerA.getIdentifier();
   *   Object identifierB = providerB.getIdentifier();
   * </pre>
   * ... then:
   * <pre>
   *   if (identifierA.equals(identifierB)) ...
   * </pre>
   * then the two providers must point to the same location.
   */
  public Object            getIdentifier();

  /**
   * Returns true if the underlying target has changed
   * since the last call to openInputStream()
   */
  public boolean           hasSourceChanged();

  /**
   * Returns the cached result from reading and parsing this
   * provider.
   * @see CachingNameResolver
   */
  public Object            getCachedResult();

  /**
   * Stores the cached result of reading and parsing this
   * provider.
   * @see CachingNameResolver
   */
  public void              setCachedResult(Object value);
}
