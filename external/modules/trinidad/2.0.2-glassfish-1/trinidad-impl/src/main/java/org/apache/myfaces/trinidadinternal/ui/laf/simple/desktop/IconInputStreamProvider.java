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
package org.apache.myfaces.trinidadinternal.ui.laf.simple.desktop;

import java.io.InputStream;
import java.io.IOException;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.skin.Icon;

/**
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
class IconInputStreamProvider implements InputStreamProvider
{
  IconInputStreamProvider(Icon icon)
  {
    _icon = icon;
  }

  /**
   * Return an InputStream for the target.  This function
   * should never return null - if a stream cannot be opened,
   * throw an IOException.
   */
  public InputStream  openInputStream() throws IOException
  {
    return _icon.openStream(FacesContext.getCurrentInstance(),
                            RenderingContext.getCurrentInstance());
  }

  /**
   * Returns the name of the target location, suitable
   * for user display.
   */
  public String  getDisplayName()
  {
    return _icon.toString();
  }

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
  public Object getIdentifier()
  {
    return _icon;
  }

  /**
   * Returns true if the underlying target has changed
   * since the last call to openInputStream()
   */
  public boolean           hasSourceChanged()
  {
    return false;
  }

  /**
   * Returns the cached result from reading and parsing this
   * provider.
   * @see CachingNameResolver
   */
  public Object            getCachedResult()
  {
    return _cachedResult;
  }

  /**
   * Stores the cached result of reading and parsing this
   * provider.
   * @see CachingNameResolver
   */
  public void              setCachedResult(Object value)
  {
    _cachedResult = value;
  }

  private Icon   _icon;
  private Object _cachedResult;
}
