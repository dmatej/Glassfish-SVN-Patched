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
package org.apache.myfaces.trinidadinternal.taglib;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.UIXCollection;
import org.apache.myfaces.trinidad.webapp.UIXComponentELTag;

/**
 * Manually written because we don't auto-generate UIXCollection,
 * and we can't currently auto-generate only one of the two.
 */
abstract public class UIXCollectionTag extends UIXComponentELTag
{
  /**
   * Construct an instance of the UIXCollectionTag.
   */
  public UIXCollectionTag()
  {
  }

  private String _var;
  public void setVar(String var)
  {
    _var = var;
  }

  @Override
  protected void setProperties(
    FacesBean bean)
  {
    super.setProperties(bean);
    bean.setProperty(UIXCollection.VAR_KEY, _var);
  }

  @Override
  public void release()
  {
    super.release();
    _var = null;
  }
}


