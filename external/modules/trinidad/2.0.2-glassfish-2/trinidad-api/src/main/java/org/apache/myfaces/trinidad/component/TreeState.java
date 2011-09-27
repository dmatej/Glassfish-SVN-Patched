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
package org.apache.myfaces.trinidad.component;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectOutput;
import java.io.ObjectInput;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Strongly-typed object for storing UIX Tree State.
 * <p>
 * @todo All warnings in here map to missing features in our state management
 *   system.
 * @todo Add a decent toString() implementation to improve debugging.
 */
class TreeState implements Externalizable
{
  public TreeState()
  {
    _empty = true;
  }

  public boolean isEmpty()
  {
    return _empty;
  }

  @SuppressWarnings("unchecked")
  public void restoreState(FacesContext context, UIXComponentBase component)
  {
    component.restoreState(context, _state);
    int childCount = component.getChildCount();
    int arrayCount = (_children == null) ? 0 : _children.length;
    int transientCount = 0;

    // In Facelets land, we might have transient components *already in* the
    // tree - but they won't have had state saved.  So, we'd better
    // account for those before assuming that our saved state count
    // and actual child count can't be reconciled!
    if (childCount != arrayCount)
    {
      List children = component.getChildren();
      for (int i = 0; i < childCount; i++)
      {
        UIComponent child = (UIComponent) children.get(i);
        if (child.isTransient())
          transientCount++;
      }
    }

    if (arrayCount + transientCount != childCount)
    {
      if (_LOG.isWarning())
        _LOG.warning("SAVED_CHILD_COUNT_NOT_MATCH", new Object[] {arrayCount, childCount});
    }

    else
    {
      List children = component.getChildren();
      int arrayIndex = 0;
      for (int i = 0; i < childCount; i++)
      {
        UIComponent child = (UIComponent) children.get(i);
        if (child.isTransient())
        {
          continue;
        }
        else
        {
          child.processRestoreState(context,  _children[arrayIndex]);
          arrayIndex++;
        }
      }
    }

    // Restore the facets' state
    if (_facets != null)
    {
      assert(_facets.length % 2 == 0);

      int facetCount = _facets.length / 2;
      // If our count is off, log a warning
      if (facetCount < component.getFacetCount())
      {
        if (_LOG.isWarning())
          _LOG.warning("FACETS_STATE_MISSING_WILLNOT_RESTORE", component);
      }


      for (int i = 0; i < facetCount; i++)
      {
        assert((_facets[i * 2] == null) ||
               (_facets[i * 2] instanceof String));

        String facetName = (String) _facets[i * 2];
        if (facetName == null)
          continue;

        Object facetState = _facets[i * 2 + 1];
        UIComponent facet = component.getFacet(facetName);
        if (facet == null)
        {
          if (_LOG.isWarning())
            _LOG.warning("DISCARDING_SAVED_STATE", facetName);
        }
        else
        {
          if (facet.isTransient())
          {
            if (facetState != null)
            {
              if (_LOG.isWarning())
                _LOG.warning("SAVED_STATE_INCLUDE_TRANSIENT_COMPONENT_STATE", facet);
            }
          }
          else
          {
            facet.processRestoreState(context,  facetState);
          }
        }
      }
    }
  }

  @SuppressWarnings("unchecked")
  public void saveState(FacesContext context, UIXComponentBase component)
  {
    // Save the component's state
    _state = component.saveState(context);
    if (_state != null)
      _empty = false;

    // Save the children's state
    int childCount = component.getChildCount();
    if (childCount == 0)
    {
      _children = null;
    }
    else
    {
      _children = new Object[childCount];
      List children = component.getChildren();
      int j = 0;
      for (int i = 0; i < childCount; i++)
      {
        UIComponent child = (UIComponent) children.get(i);
        if (!child.isTransient())
        {
          Object childState = child.processSaveState(context);
          if (childState != null)
          {
            _empty = false;
            _children[j] = childState;
          }

          j++;
        }
      }

      // OK - there were some transient components, so the array's too big;
      // trim it down
      if (j < childCount)
      {
        Object[] newChildren = new Object[j];
        System.arraycopy(_children, 0, newChildren, 0, j);
        _children = newChildren;
      }
    }

    // Save the facets' state
    int facetCount = component.getFacetCount();
    if (facetCount == 0)
    {
      _facets = null;
    }
    else
    {
      _facets = new Object[facetCount * 2];
      int i = 0;
      Set<Map.Entry<String, UIComponent>> entries = component.getFacets().entrySet();
      for(Map.Entry<String, UIComponent> entry : entries)
      {
        UIComponent facet = entry.getValue();

        // Just skip over transient facets
        if (facet.isTransient())
          continue;

        Object facetState = facet.processSaveState(context);
        if (facetState != null)
        {
          _empty = false;
          _facets[2 * i] = entry.getKey();
          _facets[2 * i + 1] = facetState;
        }

        i++;
      }

      // If there's no non-transient facets, then bail
      if (i == 0)
        _facets = null;
    }
  }

  //
  // Implementation of Externalizable - just a bit easier on output
  // size, for very little work.
  //

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(_state);
    out.writeObject(_facets);
    out.writeObject(_children);
  }

  public void readExternal(ObjectInput in)
     throws IOException, ClassNotFoundException
  {
    _state = in.readObject();
    _facets = (Object[]) in.readObject();
    _children = (Object[]) in.readObject();
  }


  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TreeState.class);

  private Object[] _facets;
  private Object[] _children;
  private Object _state;
  private boolean _empty;

  private static final long serialVersionUID = 1L;
}
