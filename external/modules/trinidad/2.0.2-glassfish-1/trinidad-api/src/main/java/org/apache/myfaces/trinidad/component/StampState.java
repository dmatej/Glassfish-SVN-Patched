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
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.io.Serializable;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.UIPanel;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * This class saves the state of stamp components.
 */
final class StampState implements Externalizable
{
  public StampState()
  {
    _rows = Collections.emptyMap();
  }

  /**
   * Clears all state except for the state associated with the
   * give currencyObj
   * @param skipCurrencyObj
   */
  public void clear(Object skipCurrencyObj)
  {
    if (!_rows.isEmpty())
    {
      Iterator<DualKey> iter = _rows.keySet().iterator();
      while(iter.hasNext())
      {
        DualKey dk = iter.next();
        if (_eq(dk._key1, skipCurrencyObj))
          continue;
        iter.remove();
      }
    }
  }

  public void put(Object currencyObj, String key, Object value)
  {
    Map<DualKey, Object> comparant = Collections.emptyMap();
    if (_rows == comparant)
    {
      if (value == null)
        return;

      // =-=AEW Better default sizes
      _rows = new HashMap<DualKey, Object>(109);
    }

    DualKey dk = new DualKey(currencyObj, key);
    // Make sure that if we're applying a null value, that we
    // don't hold on to the key and retain the entry - just nuke
    // the entry
    if (value == null)
      _rows.remove(dk);
    else
      _rows.put(dk, value);
  }

  public int size()
  {
    return _rows.size();
  }

  public Object get(Object currencyObj, String key)
  {
    DualKey dk = new DualKey(currencyObj, key);
    return _rows.get(dk);
  }

  /**
   * Save the per-row state of a given stamp.
   */
  public static Object saveStampState(FacesContext context, UIComponent stamp)
  {
    RowState state = _createState(stamp);
    return state;
  }

  /**
   * Restore the per-row state of a given stamp.
   */
  public static void restoreStampState(FacesContext context, UIComponent stamp,
                                       Object stampState)
  {
    if (stampState != null)
    {
      RowState state = (RowState) stampState;
      state.restoreRowState(stamp);
    }
  }

  /**
   * save the stamp state of just the children of the given component
   * in the given table.
   */
  @SuppressWarnings("unchecked")
  public static Object saveChildStampState(
    FacesContext context,
    UIComponent   stamp,
    UIXCollection table)
  {
    int childCount = stamp.getChildCount();
    // If we have any children, iterate through the array,
    // saving state
    if (childCount == 0)
      return null;

    Object[] childStateArray = null;
    List<UIComponent> children = stamp.getChildren();
    boolean childStateIsEmpty = true;
    for(int i=0; i < childCount; i++)
    {
      UIComponent child = children.get(i);
      Object childState = table.saveStampState(context, child);

      // Until we have one non-null entry, don't allocate the array.
      // Unlike facets, we *do* care about stashing Transient.TRUE,
      // because we have to keep track of them relative to any
      // later components, BUT if it's all null and transient, we
      // can discard the array.  This does mean that putting
      // transient components into a stamp is a bit inefficient

      // So: allocate the array if we encounter our first
      // non-null childState (even if it's transient)
      if (childStateArray == null)
      {
        if (childState == null)
          continue;

        childStateArray = new Object[childCount];
      }

      // But remember the moment we've encountered a non-null
      // *and* non-transient component, because that means we'll
      // really need to keep this array
      if ((childState != UIXCollection.Transient.TRUE) && (childState != null))
        childStateIsEmpty = false;

      // Store a value into the array
      assert(childStateArray != null);
      childStateArray[i] = childState;
    }

    // Even if we bothered to allocate an array, if all we
    // had were transient + null, don't bother with the array at all
    if (childStateIsEmpty)
      return null;

    return childStateArray;
  }

  /**
   * Restore the stamp state of just the children of the given component
   * in the given table.
   */
  @SuppressWarnings("unchecked")
  public static void restoreChildStampState(
    FacesContext context,
    UIComponent stamp,
    UIXCollection table,
    Object stampState)
  {
    if (stampState == null)
      return;

    List<UIComponent> kids = stamp.getChildren();
    Object[] state = (Object[]) stampState;

    int childIndex = 0;
    for(int i=0; i<state.length; i++)
    {
      Object childState = state[i];
      // Skip over any saved state that corresponds to transient
      // components
      if (childState != UIXCollection.Transient.TRUE)
      {
        while (childIndex < kids.size())
        {
          UIComponent kid = kids.get(childIndex);
          childIndex++;
          // Skip over any transient components before restoring state
          if (!kid.isTransient())
          {
            table.restoreStampState(context, kid, childState);
            break;
          }
        }
      }
      // The component may or may not still be there;  if it
      // is, then we'd better skip over it
      else
      {
        if (childIndex < kids.size())
        {
          UIComponent child = kids.get(childIndex);
          // If the child isn't transient, then it must be
          // something that we want to look at on the next
          // iteration.
          if (child.isTransient())
            childIndex++;
        }
      }
    }
  }

  /**
   * @todo can do better...
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeInt(_rows.size());

    if (_rows.isEmpty())
      return;

    HashMap<DualKey, Object> map = new HashMap<DualKey, Object>(_rows.size());
    map.putAll(_rows);

    if (_LOG.isFinest())
    {
      for(Map.Entry<DualKey, Object> entry : map.entrySet())
      {
        _LOG.finest("Saving " + entry.getKey() + ", " + entry.getValue());
      }
    }

    out.writeObject(map);
  }

  @SuppressWarnings("unchecked")
  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();

    if (size > 0)
    _rows = (Map<DualKey, Object>) in.readObject();

    if (_LOG.isFinest())
    {
      for(Map.Entry<DualKey, Object> entry : _rows.entrySet())
      {
        _LOG.finest("Restoring " + entry.getKey() + ", " + entry.getValue());
      }
    }
  }

  private static RowState _createState(UIComponent child)
  {
    RowState state;
    if (child instanceof EditableValueHolder)
    {
      state = new EVHState();
      state.saveRowState(child);
    }
    else if (child instanceof UIXCollection)
    {
      state = new TableState();
      state.saveRowState(child);
    }
    else if (child instanceof UIXShowDetail)
    {
      state = SDState.getState((UIXShowDetail) child);
    }
    else
    {
      state = null;
    }

    return state;
  }

  private static boolean _eq(Object k1, Object k2)
  {
    if (k1 == null)
      return k2 == null;
    return k1.equals(k2);
  }

  // State for a single row
  static private abstract class RowState implements Serializable
  {

    public RowState()
    {
    }

    abstract public void saveRowState(UIComponent child);

    abstract public void restoreRowState(UIComponent child);

    abstract public boolean isNull();
    
    private static final long serialVersionUID = 1L;
  }

  static private final class SDState extends RowState
  {
    /**
     * Return cached, shared instances of SDState.
     */
    static public RowState getState(UIXShowDetail child)
    {
      FacesBean bean = child.getFacesBean();
      Boolean disclosed = (Boolean)bean.getLocalProperty(UIXShowDetail.DISCLOSED_KEY);
      if (disclosed == null)
        return _NULL;
      else if (disclosed)
        return _TRUE;
      else
        return _FALSE;
    }

    public SDState()
    {
    }

    private SDState(Boolean disclosed)
    {
      _disclosed = disclosed;
    }

    @Override
    public void saveRowState(UIComponent child)
    {
      FacesBean bean = ((UIXShowDetail)child).getFacesBean();
      _disclosed = (Boolean)bean.getLocalProperty(UIXShowDetail.DISCLOSED_KEY);
    }

    @Override
    public void restoreRowState(UIComponent child)
    {
      FacesBean bean = ((UIXShowDetail)child).getFacesBean();
      bean.setProperty(UIXShowDetail.DISCLOSED_KEY, _disclosed);
    }

    @Override
    public boolean isNull()
    {
      return _disclosed == null;
    }

    @Override
    public String toString()
    {
      return "SDState[disclosed=" + _disclosed + "]";
    }

    // Reusable instances of SDState. TODO: use readResolve/writeReplace
    // so that we only send across and restore instances of these
    static private final SDState _TRUE = new SDState(true);
    static private final SDState _FALSE = new SDState(false);
    static private final SDState _NULL = new SDState(null);

    /**
     *
     */
    private static final long serialVersionUID = -8605916495935317932L;

    private Boolean _disclosed;
  }

  static private final class TableState extends RowState
  {
    public TableState()
    {
    }

    @Override
    public void saveRowState(UIComponent child)
    {
      _state = ((UIXCollection) child).__getMyStampState();
    }

    @Override
    public void restoreRowState(UIComponent child)
    {
      //There is a bug in the RI where, on a PPR request, an UIPanel will sometimes be
      //added to a facet that contains only one child in facelets.  This, of course,
      //changes the structure of the saved data.  If we get a UIPanel here, then this
      //but is the cause.  It means we have a Collection which contains a switcher
      //which in turn contains another Collection, and UIPanel was returned instead of
      //the origional collection.  Therefore, this UIPanel should ALWAYS only have one
      //Item.  If the facet contained more then one item, we would ALWAYS have a UIPanel
      //and therefore we would be using a different stamp state.
      UIXCollection myChild = (child instanceof UIPanel)?(UIXCollection)child.getChildren().get(0):(UIXCollection)child;
      
      myChild.__setMyStampState(_state);
    }

    @Override
    public boolean isNull()
    {
      return _state == null;
    }

    private Object _state = null;

    private static final long serialVersionUID = 1L;
  }

  static private class EVHState extends RowState
  {
    public EVHState()
    {
      _valid = true;
    }

    @Override
    public void saveRowState(UIComponent child)
    {
      assert _assertIsStampCorrect(child);

      EditableValueHolder evh = (EditableValueHolder) child;
      _submitted = evh.getSubmittedValue();
      _localSet = evh.isLocalValueSet();
      _local = evh.getLocalValue();
      _valid = evh.isValid();
    }

    @Override
    public void restoreRowState(UIComponent child)
    {
      assert _assertIsStampCorrect(child);

      EditableValueHolder evh = (EditableValueHolder) child;
      evh.setSubmittedValue(_submitted);
      evh.setValue(_local);
      evh.setLocalValueSet(_localSet);
      evh.setValid(_valid);

      assert _assertStampHonoursState(evh);
    }

    @Override
    public boolean isNull()
    {
      return (_valid && (!_localSet) && (_submitted == null));
    }


    @Override
    public String toString()
    {
      return "EVHState[value=" + _local + ",submitted=" + _submitted + "]";
    }

    private boolean _assertStampHonoursState(EditableValueHolder evh)
    {
      return (evh.getSubmittedValue() == _submitted) &&
        (evh.getLocalValue() == _local) &&
        (evh.isLocalValueSet() == _localSet) &&
        (evh.isValid() == _valid);
    }

    /**
     * Make sure that this stampState is used to save/restore state from the
     * same stamp each time.
     */
    private boolean _assertIsStampCorrect(UIComponent stamp)
    {
      if (_assertStamp != null)
      {
        String stampId = stamp.getId();
        String assertStampId = _assertStamp.getId();
        assert (((assertStampId == null) && (stampId == null)) ||
                ((assertStampId != null) && assertStampId.equals(stampId))) :
          "Current stamp:"+stamp+
          " with id:"+stamp.getId()+
          ". Previously stamp was:"+_assertStamp+
          " with id:"+_assertStamp.getId();
      }
      else
      {
        _assertStamp = stamp;
      }
      return true;
    }

    private Object _submitted;
    private Object _local;
    private boolean _localSet;
    private boolean _valid;
    private transient UIComponent _assertStamp = null;

    private static final long serialVersionUID = 1L;
  }

  private static final class DualKey implements Serializable
  {
    public DualKey(Object key1, Object key2)
    {
      _key1 = key1;
      _key2 = key2;

      _hash = _hash(key1) + _hash(key2);
    }

    @Override
    public boolean equals(Object other)
    {
      if (other == this)
        return true;
      if (other instanceof DualKey)
      {
        DualKey otherKey = (DualKey) other;
        if (hashCode() != otherKey.hashCode())
          return false;

        return _eq(_key1, otherKey._key1) && _eq(_key2, otherKey._key2);
      }
      return false;
    }

    @Override
    public int hashCode()
    {
      return _hash;
    }

    @Override
    public String toString()
    {
      return "<"+_key1+","+_key2+">";
    }

    private static int _hash(Object k)
    {
      return (k==null) ? 0 : k.hashCode();
    }

    private final Object _key1, _key2;
    private final int _hash;

    private static final long serialVersionUID = 1L;
  }

  private static final TrinidadLogger _LOG =
     TrinidadLogger.createTrinidadLogger(StampState.class);

  private Map<DualKey, Object> _rows;
  private static final Object[] _EMPTY_ARRAY = new Object[0];
  private static final long serialVersionUID = 1L;
}
