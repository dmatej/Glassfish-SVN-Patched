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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.util.Hashtable;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;

/**
 * Generates a unique ID, or returns a unique ID already built on this node.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/UniqueCompositeId.java#0 $) $Date: 10-nov-2005.18:54:17 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class UniqueCompositeId
{
  private String  _id = null;
  private String  _firstId = null;

  static public String getId(String baseid, String suffix)
  {
    return XhtmlUtils.getCompositeId(baseid, suffix);
  }

  public UniqueCompositeId(UIXRenderingContext context,
                           String baseid)
  {
    this(context, 0, baseid);
  }

  @SuppressWarnings("unchecked")
  public UniqueCompositeId(UIXRenderingContext context,
                           int    depth,
                           String baseid)
  {
    // Nothing to base the id on, leave both ids null
    if (baseid == null)
      return;

    StringBuffer compID
      = new StringBuffer(baseid.length()
                         + XhtmlLafConstants.COMPOSITE_ID_EXTENSION.length()
                         + 2);
    compID.append(baseid);
    compID.append(XhtmlLafConstants.COMPOSITE_ID_EXTENSION);
    _firstId = compID.toString();

    String idExt =  null;

    // Bug 3657413: if a previous check at this depth failed, the update
    // was done on this node, not a parent, so check from this node up to
    // the requested depth.
    for (int i = 0; ((idExt == null) && (i <= depth)); i++)
      idExt = (String) context.getLocalProperty(i, _EXT_KEY, null);

    if (idExt == null)
    {
      // nothing set yet AT THIS LEVEL. Check globally
      // RenderingContext should have a count of IDs of this base already set
      // we just increment the count, save it on RenderingContext, and
      // build a new ID by the usual method, and tacking on the count
      // then we save it locally so it gets returned next time we ask for it
      // at this level.
      Hashtable<String, Integer> countHash =
        (Hashtable<String, Integer>) context.getProperty(UIConstants.MARLIN_NAMESPACE,
                                                         _EXT_KEY);
      if (countHash == null)
      {
        countHash = new Hashtable<String, Integer>();
      }

      Integer globalCount = countHash.get(baseid);
      int newCount;

      if (globalCount == null)
      {
        newCount = 0;
      }
      else
      {
        newCount = globalCount.intValue();
        compID.append(newCount++);
      }
      countHash.put(baseid, Integer.valueOf(newCount));
      context.setProperty(UIConstants.MARLIN_NAMESPACE, _EXT_KEY, countHash);
      idExt = compID.toString();
      context.setLocalProperty(_EXT_KEY, idExt);
    }
    _id = idExt;
  }

  public String getId()
  {
    return _id;
  }

  public String firstId()
  {
    return _firstId;
  }

  /**
   * returns true if this ID was a duplicate of another at this level.
   */
  public boolean isDuplicate()
  {
    return !_id.equals(_firstId);
  }

  private static final Object _EXT_KEY = new Object();
}
