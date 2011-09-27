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
package org.apache.myfaces.trinidadinternal.ui.laf;

import java.util.Collections;
import java.util.Iterator;

import org.apache.myfaces.trinidadinternal.ui.RendererManager;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;


/**
 * Defines the components which are used to implement a particular
 * look and feel (for example, the Browser Look And Feel).  The
 * LookAndFeel can vary on a per-request basis, to allow rendering
 * to vary based on the target Agent or a user preference.
 *
 * @see LookAndFeelManager
 * @see org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext#getLookAndFeel
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/LookAndFeel.java#0 $) $Date: 10-nov-2005.18:50:30 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract public class LookAndFeel
{
  /**
   * Returns an string identifier which uniquely identies
   * this LookAndFeel implementation.  LookAndFeel implementations
   * can be retrieved by id via LookAndFeelManager.getLookAndFeelById().
   * @see org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelManager#getLookAndFeelById
   */
  public String getId()
  {
    return null;
  }

  /**
   * Returns the name of the look and feel "family" for this
   * look and feel.
   * The family name is used when specifying a preferred look
   * and feel.  This provides a way to refer to a group of
   * related look and feel implementations while allowing the
   * particular look and feel instance to be selected based on the
   * current agent.
   */
  public String getFamily()
  {
    return null;
  }

  /**
   * Returns the default RendererManager for this LookAndFeel.
   * @see RendererManager
   */
  abstract public RendererManager getRendererManager();

  /**
   * Returns the RendererManager for a particular facet of this
   * LookAndFeel.  By default, all LookAndFeels support only
   * UIConstants.FACET_DEFAULT;  if the requested facet is not
   * found, this default facet will be used instead.
   * @see RendererManager
   * @see #supportsFacet
   */
  public RendererManager getRendererManager(String facet)
  {
    return getRendererManager();
  }

  /**
   * Returns an enumeration of all supported facets.  This
   * must be a non-empty enumeration, and must always include
   * at least UIConstants.FACET_DEFAULT.
   * @see #supportsFacet
   */
  public Iterator<String> getSupportedFacets()
  {
    return Collections.singletonList(UIConstants.FACET_DEFAULT).iterator();
  }

  /**
   * Returns true if a given facet is supported.
   * @see #getSupportedFacets
   */
  public boolean supportsFacet(String facet)
  {
    Iterator<String> en = getSupportedFacets();
    while (en.hasNext())
    {
      if (facet.equals(en.next()))
        return true;
    }

    return false;
  }
}
