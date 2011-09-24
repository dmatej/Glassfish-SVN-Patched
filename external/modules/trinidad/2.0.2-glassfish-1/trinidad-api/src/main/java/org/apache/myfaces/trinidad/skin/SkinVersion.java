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
package org.apache.myfaces.trinidad.skin;

 /**
  * The skin version works tightly with the skin family.
  * This allows someone to create versions of their skin, like purple (no version), 
  * purple version v2, purple version v3. 
  * Then the user can say which skin version they want, like:
  * <skin-family>purple</skin-family><skin-version>v3</skin-version> when they 
  * pick a skin in trinidad-config.xml.
  * When creating a skin, you give it a version if you care about versioning.
  */
final public class SkinVersion
{
  /**
   * Constructor that takes a version name.
   * @param name the name of the version, like "v1". If name is null, it is converted to "".
   * same skin family
   */
  public SkinVersion(String name)
  {
    this(name, false);
  }
  
  /**
   * Constructor that takes a name and a defaultVersion.
   * @param name the name of the version, like "v1". If name is null, it is converted to "".
   * @param defaultVersion true if this skin is the default version for all skins with the
   * same skin family
   */
  public SkinVersion(
    String  name,
    boolean defaultVersion)
  {
    if(name == null) 
      name = "";
    
    _default = defaultVersion;
    _name = name;
  }
  
  public boolean isDefault()
  {
    return _default;
  }
  
  public String getName()
  {
    return _name;
  }
  
  @Override
  final public boolean equals(Object o) 
  {
    if (o == this)
      return true;
    if (!(o instanceof SkinVersion))
    {
      return false;
    }
    SkinVersion test = (SkinVersion)o;
    return (test.isDefault() == this.isDefault()) &&
      (test.getName().equals(this.getName()));
  }
  
  @Override
  final public int hashCode()
  {
    int hash = 17;
    hash = 37*hash + this.getName().hashCode();
    hash = 37*hash + ((this.isDefault()) ? 1231 : 1237 );

    return hash; 
  } 

  @Override
  public String toString()
  {
    StringBuffer buffer = new StringBuffer("Version[");
    buffer.append(getName());

    boolean isDefault = isDefault();

    if (isDefault)
    {
      buffer.append(',');
      buffer.append("default");
    }
    return buffer.toString();
  }
  
  // If the skin doesn't explicitly have a version, then it will return EMPTY_SKIN_VERSION
  // when skin.getVersion is callled. This makes our skin picking code cleaner.
  public final static SkinVersion EMPTY_SKIN_VERSION = new SkinVersion("");

  private final boolean _default;
  private final String _name;
}