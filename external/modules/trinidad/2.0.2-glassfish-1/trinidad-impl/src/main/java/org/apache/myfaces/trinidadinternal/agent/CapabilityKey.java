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
package org.apache.myfaces.trinidadinternal.agent;

import java.util.HashMap;

/**
 * Key for capability
 * Pretty much an impl similar to uix of AgentCapabilityKey
 * -- added method getKeyAt (and required storage units)
 * -- remove getKeyCount as ...
 *
 */
public final class CapabilityKey
{

  private CapabilityKey(String capabilityName,
                        int index)
  {
    if (capabilityName == null)
      throw new NullPointerException();

    if (index < 0)
      throw new IllegalArgumentException();

    _capName = capabilityName.intern();
    _capIndex = index;
    _capKeyNames.put(_capName, this);

    if (_keys.length < _capIndex)
    {
      CapabilityKey[] newKeys = new CapabilityKey[_keys.length + DEFAULT_SIZE];
      System.arraycopy(_keys, 0, newKeys, 0, _keys.length);
    }

    _keys[_capIndex] = this;
  }

  /**
   * Create a new CapabilityKey. If an key with name already exists returns the
   * pre-created CapabilityKey
   * @param capabilityName
   */
  public static CapabilityKey getCapabilityKey(String capabilityName, boolean createIfNull)
  {
    if (capabilityName == null)
      return null;

    Object key = _capKeyNames.get(capabilityName);
    if ((createIfNull) && (key == null))
      key = _createKey(capabilityName);

    return ((CapabilityKey) key);
  }

  /**
   *
   * @param capabilityName
   * @return CapabilityKey for the capability name
   */
  public static CapabilityKey getCapabilityKey(String capabilityName)
  {
    return getCapabilityKey (capabilityName, false);
  }

  /**
   * @return capability name
   */
  public String getCapabilityName()
  {
    return _capName;
  }

  /**
   * @return capability key index
   */
  public int getIndex()
  {
    return _capIndex;
  }


  /**
   * @param index
   * @return CapabilityKey with the specified index
   */
  public static CapabilityKey getKeyAt(int index)
  {
    if (index >= 0 && index <= _count)
      return _keys[index];

    //just return null if out of range
    return null;
  }

  @Override
  public boolean equals(Object obj)
  {
    return this == obj;
  }

  @Override
  public int hashCode()
  {
    return _capIndex;
  }

  synchronized private static Object _createKey(String capabilityName)
  {
    Object key = _capKeyNames.get(capabilityName);
    if (key == null)
    {
      key = new CapabilityKey(capabilityName, _count++);
    }
    return key;
  }

  @Override
  public String toString()
  {
    return _capName;
  }

  private String _capName;
  private int _capIndex = 0;

  static final private int DEFAULT_SIZE = 50;

  static private HashMap<String, CapabilityKey> _capKeyNames = 
    new HashMap<String, CapabilityKey>();
  static private CapabilityKey[] _keys = new CapabilityKey[DEFAULT_SIZE];
  private static int _count = 0;
}
