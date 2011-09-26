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
package org.apache.myfaces.trinidadinternal.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectStreamClass;
import java.util.HashMap;

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidadinternal.util.ObjectInputStreamResolveClass;

public class ObjectInputStreamResolveClass extends ObjectInputStream
{
  public ObjectInputStreamResolveClass()  throws IOException, SecurityException
  {
    super();
  }

  public ObjectInputStreamResolveClass(InputStream in) throws IOException 
  {
    super(in);
  }
  
  protected Class<?> resolveClass(ObjectStreamClass desc)
                           throws IOException,
                                  ClassNotFoundException
  {
    Class<?> cls = null;
    String className = desc.getName();

    // if it is primitive class, for example, long.class
    // we resolve them by getting them from the hashMaps
    cls = _PRIMITIVE_CLASSES.get(className);

    if (null == cls)
    {
      // TRINIDAD-1062 It has been noticed that in OC4J and Weblogic that the
      // classes being resolved are having problems by not finding
      // them using the context class loader. Therefore, we are adding
      // this work-around until the problem with these application
      // servers can be better understood
      cls = ClassLoaderUtils.loadClass(desc.getName());
    }
    return cls;
  }

  // HashMap to map primitives to their clazzes
  private static final HashMap<String, Class<?>> _PRIMITIVE_CLASSES = new HashMap<String, Class<?>>();

  static
  {
    _PRIMITIVE_CLASSES.put("byte", byte.class);
    _PRIMITIVE_CLASSES.put("short", short.class);
    _PRIMITIVE_CLASSES.put("int", int.class);
    _PRIMITIVE_CLASSES.put("long", long.class);
    _PRIMITIVE_CLASSES.put("boolean", boolean.class);
    _PRIMITIVE_CLASSES.put("char", char.class);
    _PRIMITIVE_CLASSES.put("float", float.class);
    _PRIMITIVE_CLASSES.put("double", double.class);
    _PRIMITIVE_CLASSES.put("void", void.class);
  }
}
