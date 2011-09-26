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

import java.util.Iterator;

import org.apache.myfaces.trinidad.share.io.InputStreamProvider;


/**
 * A subinterface of InputStreamProvider that adds support
 * for tracking dependencies.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/io/CachingInputStreamProvider.java#0 $) $Date: 10-nov-2005.19:00:05 $
 */
public interface CachingInputStreamProvider extends InputStreamProvider
{
  // =-=AEW If I could do it over again, I'd move getCachedResult()
  // and setCachedResult() down to this interface.
  /**
   * Adds a dependency to this provider.  The result of {@link #hasSourceChanged}
   * must then take into account this dependency.
   */
  public void  addCacheDependency(InputStreamProvider dependency);

  /**
   * Returns an enumeration of all the InputStreamProviders that
   * were calculated as dependencies of this file.  This is, of course,
   * only valid after the target has been processed.  If there
   * are no dependencies, returns null.
   */
  public Iterator<InputStreamProvider> getCacheDependencies();
}
