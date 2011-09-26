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

import java.io.InputStream;
import java.io.IOException;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;

/**
 * Proxying class for InputStreamProviders.  Subclassers
 * must override getProvider().
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/io/InputStreamProviderProxy.java#0 $) $Date: 10-nov-2005.19:00:09 $
 */
abstract public class InputStreamProviderProxy implements InputStreamProvider
{
  /**
   * Create an InputStreamProviderProxy.
   */
  public InputStreamProviderProxy()
  {
  }


  /**
   * Return the provider being proxied.
   */
  abstract protected InputStreamProvider getProvider();

  public InputStream openInputStream() throws IOException
  {
    return getProvider().openInputStream();
  }

  public String getDisplayName()
  {
    return getProvider().getDisplayName();
  }

  public Object getIdentifier()
  {
    return getProvider().getIdentifier();
  }
    
  public boolean hasSourceChanged()
  {
    return getProvider().hasSourceChanged();
  }

  @Override
  public boolean equals(Object o)
  {
    if (o instanceof InputStreamProviderProxy)
      o = ((InputStreamProviderProxy) o).getProvider();

    return getProvider().equals(o);
  }

  @Override
  public int hashCode()
  {
    return getProvider().hashCode();
  }

  public Object getCachedResult()
  {
    return getProvider().getCachedResult();
  }

  public void setCachedResult(Object value)
  {
    getProvider().setCachedResult(value);
  }
}
