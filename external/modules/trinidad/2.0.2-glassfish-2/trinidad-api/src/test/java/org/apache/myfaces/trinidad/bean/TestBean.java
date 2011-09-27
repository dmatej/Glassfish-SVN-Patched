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
package org.apache.myfaces.trinidad.bean;

import java.util.Iterator;

import org.apache.myfaces.trinidad.bean.FacesBeanImpl;
import org.apache.myfaces.trinidad.bean.PropertyKey;

public class TestBean extends FacesBeanImpl
{
  static public final Type TYPE = new Type();
  static public final PropertyKey FIRST_KEY = 
    TYPE.registerKey("first");
  static public final PropertyKey SECOND_KEY = 
    TYPE.registerKey("second");
  static public final PropertyKey TRANSIENT_KEY = 
    TYPE.registerKey("transient",
                     PropertyKey.CAP_TRANSIENT);
  static public final PropertyKey ITEMS_KEY = 
    TYPE.registerKey("items",
                     PropertyKey.CAP_LIST);
  static public final PropertyKey CANT_BE_BOUND_KEY = 
    TYPE.registerKey("cantBeBound",
                     PropertyKey.CAP_NOT_BOUND);
  static public final PropertyKey SILLY_KEY = 
    TYPE.registerKey("silly",
                     PropertyKey.CAP_STATE_HOLDER);
  static public final PropertyKey FIRST_ALIAS_KEY = 
    TYPE.registerAlias(FIRST_KEY, "firstAlias");

  static
  {
    TYPE.lock();
  }

  public TestBean()
  {
  }

  @Override
  public Type getType()
  {
    return TYPE;
  }


  public String getFirst()
  {
    return (String) getProperty(FIRST_KEY);
  }

  public void setFirst(String first)
  {
    setProperty(FIRST_KEY, first);
  }

  public String getFirstAlias()
  {
    return (String) getProperty(FIRST_ALIAS_KEY);
  }

  public void setFirstAlias(String firstAlias)
  {
    setProperty(FIRST_ALIAS_KEY, firstAlias);
  }

  public String getSecond()
  {
    return (String) getProperty(SECOND_KEY);
  }

  public void setSecond(String second)
  {
    setProperty(SECOND_KEY, second);
  }


  public String getTransient()
  {
    return (String) getProperty(TRANSIENT_KEY);
  }

  public void setTransient(String transientStr)
  {
    setProperty(TRANSIENT_KEY, transientStr);
  }

  public String getCantBeBound()
  {
    return (String) getProperty(CANT_BE_BOUND_KEY);
  }

  public void setCantBeBound(String cantBeBound)
  {
    setProperty(CANT_BE_BOUND_KEY, cantBeBound);
  }

  public void addItem(Integer i)
  {
    addEntry(ITEMS_KEY, i);
  }

  public void removeItem(Integer i)
  {
    removeEntry(ITEMS_KEY, i);
  }

  public Integer[] getItems()
  {
    return (Integer[]) getEntries(ITEMS_KEY, Integer.class);
  }

  public Iterator<Object> items()
  {
    return entries(ITEMS_KEY);
  }
}
