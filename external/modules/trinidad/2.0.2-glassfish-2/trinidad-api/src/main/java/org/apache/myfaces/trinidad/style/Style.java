package org.apache.myfaces.trinidad.style;

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

import java.util.Map;

/**
 * A Style object defines a set of visual (or aural) style properties.
 * The Style interface exposes one method for retrieving properties:
 * getProperties().  
 * getProperties() returns a Map&lt;String, String> of the propery name as the
 * key and the property value as the value.
 *
 */
public abstract class Style
{

  /**
   * Returns an unmodifiable Map of the properties (name/value) 
   * defined by this style.
   */
  abstract public Map<String, String> getProperties();


  /**
   * Converts the style to a String suitable for use as an inline style
   * attribute value.
   */
  abstract public String toInlineString();

}

