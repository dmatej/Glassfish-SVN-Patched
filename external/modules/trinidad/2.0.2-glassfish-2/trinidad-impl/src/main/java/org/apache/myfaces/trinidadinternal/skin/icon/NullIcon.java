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
package org.apache.myfaces.trinidadinternal.skin.icon;

import java.util.Map;
import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidad.skin.Icon;

import org.apache.myfaces.trinidad.context.RenderingContext;

/**
 * Icon class for a completely empty, null icon.
 */
public class NullIcon extends Icon
{
  static public Icon sharedInstance()
  {
    return _ICON;
  }

  // Most NullIcon clients should use NullIcon.sharedInstance() to
  // retrieve a NullIcon instance.  This constructor is provided
  // specifically for clients that are using a NullIcon instance
  // as a marker object, eg. see RequestSkinWrapper.
  public NullIcon()
  {
  }

  @Override
  public void renderIcon(
    FacesContext context,
    RenderingContext arc,
    Map<String, ? extends Object> attrs
    )
  {
    // null icons don't render anything
  }

  @Override
  public boolean isNull()
  {
    return true;
  }

  static private final NullIcon _ICON = new NullIcon();
}


