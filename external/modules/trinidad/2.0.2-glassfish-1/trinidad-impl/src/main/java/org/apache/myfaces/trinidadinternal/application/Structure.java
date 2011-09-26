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
package org.apache.myfaces.trinidadinternal.application;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.component.UIXComponentBase;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

/**
 * Utility class for storing the structure of a UIComponent tree.
 * <p>
 */
// -= Simon Lessard =-
//     Why using an ArrayList as a map again? It is not type-safe.
//     Using an object to store the key/value seems like a decent
//     compromise memory wise. Or better, I would suggest a TreeMap 
//     to lower the complexity to O(log(n)) instead of O(n) at 
//     minimal memory cost. If balancing is still considered too 
//     expensive and an additional class is not wanted, then parallel
//     lists would also do the trick
// =-=Adam Winer =-=
//   To answer the question:  the explicit intent of this
//   class is entirely to produce an optimal, minimally-sized
//   structure for state saving.  Everything is iterated through
//   straight away.  Type-safety is nearly irrelevant (it's
//   one, self-encapsulated structure, with no relevant external API);
//   everything is iterated through directly (there is no O(log n) anything)
//   relevant.  Of overriding importance here is size and efficiency.
final class Structure implements Externalizable
{
  /**
   * Zero-arg constructor for Externalizable contract.
   */
  public Structure()
  {
  }


  /**
   * Create the structure of an existing component.
   */
  public Structure(UIComponent component)
  {
    _class = component.getClass().getName();
    _id = component.getId();
    _facets = _getFacets(component);
    _children = _getChildren(component);
  }

  /**
   * Re-create a component from a structure object
   */
  @SuppressWarnings("unchecked")
  public UIComponent createComponent()
    throws ClassNotFoundException, InstantiationException,
           IllegalAccessException
  {
    Class<?> clazz = ClassLoaderUtils.loadClass(_class);
    UIComponent component = (UIComponent) clazz.newInstance();
    if (_id != null)
      component.setId(_id);
    // Create any facets
    if (_facets != null)
    {
      Map<String, UIComponent> facets = component.getFacets();
      for (int i = 0 ; i < _facets.size(); i += 2)
      {
        UIComponent facet = ((Structure) _facets.get(i + 1)).
                                 createComponent();
        facets.put((String)_facets.get(i), facet);
      }
    }

    // Create any children
    if (_children != null)
    {
      List<UIComponent> children = component.getChildren();
      for (int i = 0 ; i < _children.size(); i++)
      {
        UIComponent child = _children.get(i).createComponent();
        children.add(child);
      }
    }

    return component;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(_abbreviateClass(_class));
    out.writeObject(_id);

    // Write the facets
    if (_facets == null)
    {
      out.writeShort(0);
    }
    else
    {
      out.writeShort(_facets.size());
      for (int i = 0; i < _facets.size(); i += 2)
      {
        out.writeObject(_facets.get(i));
        ((Structure) _facets.get(i + 1)).writeExternal(out);
      }
    }

    // Write the children
    if (_children == null)
    {
      out.writeShort(0);
    }
    else
    {
      out.writeShort(_children.size());
      
      // 2006-08-02: -= Simon Lessard =-
      //             get(index) is inefficient if the List ever
      //             become something else than ArrayList
      // =-=Adam Winer=-=:  yep, so we won't change it to be something else.
      for (int i = 0; i < _children.size(); i++)
      {
        _children.get(i).writeExternal(out);
      }
    }
  }

  public void readExternal(ObjectInput in)
   throws IOException, ClassNotFoundException
  {
    _class = _unabbreviateClass((String) in.readObject());
    _id = (String) in.readObject();

    // Read the facets
    short facetCount = in.readShort();
    if (facetCount > 0)
    {
      _facets = new ArrayList<Object>(facetCount);
      for (int i = 0; i < facetCount; i += 2)
      {
        _facets.add(in.readObject());
        Structure newStruct = new Structure();
        newStruct.readExternal(in);
        _facets.add(newStruct);
      }
    }

    // Read the children
    short childCount = in.readShort();
    if (childCount > 0)
    {
      _children = new ArrayList<Structure>(childCount);
      for (int i = 0; i < childCount; i++)
      {
        Structure newStruct = new Structure();
        newStruct.readExternal(in);
        _children.add(newStruct);
      }
    }
  }

  /**
   * Store the structure of all the children.
   */
  @SuppressWarnings("unchecked")
  private List<Structure> _getChildren(UIComponent component)
  {
    if (component.getChildCount() == 0)
      return null;

    List<UIComponent> children = component.getChildren();
    ArrayList<Structure> list = new ArrayList<Structure>(children.size());

    for(UIComponent child : children)
    {
      if ((child != null) && !child.isTransient())
      {
        list.add(new Structure(child));
      }
    }

    if (list.isEmpty())
      return null;

    return list;
  }


  /**
   * Store the structure of all the facets.
   */
  @SuppressWarnings("unchecked")
  private List<Object> _getFacets(UIComponent component)
  {
    Iterator<String> facetNames;
    if (component instanceof UIXComponentBase)
    {
      facetNames = ((UIXComponentBase) component).getFacetNames();
    }
    else
    {
      facetNames = component.getFacets().keySet().iterator();
    }

    if (!facetNames.hasNext())
      return null;

    Map<String, UIComponent> facets = component.getFacets();
    ArrayList<Object> list = new ArrayList<Object>(facets.size() * 2);
    while (facetNames.hasNext())
    {
      String name = facetNames.next();
      UIComponent facet = facets.get(name);
      if ((facet != null) && !facet.isTransient())
      {
        list.add(name);
        list.add(new Structure(facet));
      }
    }

    if (list.isEmpty())
      return null;

    return list;
  }

  // Trim down a class name for any very common package prefixes
  static private String _abbreviateClass(String clazz)
  {
    if (clazz.startsWith(_COMMON_CLASS_PREFIX))
      clazz = clazz.substring(_COMMON_CLASS_PREFIX_LENGTH);
    return clazz;
  }

  // Reverse of _abbreviateClass()
  static private String _unabbreviateClass(String clazz)
  {
    if (clazz.charAt(0) == '.')
      return _COMMON_CLASS_PREFIX + clazz;
    return clazz;
  }

  private String _class;
  private String _id;
  private List<Object> _facets;
  private List<Structure> _children;


  static private final String _COMMON_CLASS_PREFIX =
    "org.apache.myfaces.trinidad.component";
  static private final int _COMMON_CLASS_PREFIX_LENGTH =
    _COMMON_CLASS_PREFIX.length();
  
  static private final long serialVersionUID = 1L; 
}