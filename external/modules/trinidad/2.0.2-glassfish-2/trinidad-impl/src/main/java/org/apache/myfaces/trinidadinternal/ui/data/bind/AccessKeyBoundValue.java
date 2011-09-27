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
package org.apache.myfaces.trinidadinternal.ui.data.bind;

import org.apache.myfaces.trinidadinternal.util.nls.StringUtils;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * A BoundValue that will decompose a second bound value
 * into a text string and an access key using conventional
 * ampersand ('&') notation.  Clients will need
 * to create two instances of this class, one for
 * the access key, the other for the text.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bind/AccessKeyBoundValue.java#0 $) $Date: 10-nov-2005.18:56:35 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class AccessKeyBoundValue implements BoundValue
{
  /**
   * Creates an AccessKeyBoundValue
   * @param textWithAccessKey the BoundValue that returns the text
   *        with an embedded access key
   * @param returnAccessKey if true, returns the access key Character;
   *            if false, returns the string with the access key removed
   */
  public AccessKeyBoundValue(
    BoundValue textWithAccessKey,
    boolean    returnAccessKey)
  {
    if (textWithAccessKey == null)
      throw new IllegalArgumentException();

    _textWithAccessKey = textWithAccessKey;
    _returnAccessKey   = returnAccessKey;
  }

  public Object getValue(UIXRenderingContext context)
  {
    Object embeddedText = _textWithAccessKey.getValue(context);

    if (embeddedText == null)
      return null;

    String embeddedTextString = embeddedText.toString();

    if (_returnAccessKey)
    {
      // set the acess key, if any
      int accessKeyIndex = StringUtils.getMnemonicIndex(embeddedTextString);

      if (accessKeyIndex == StringUtils.MNEMONIC_INDEX_NONE)
        return null;

      return Character.valueOf(embeddedTextString.charAt(accessKeyIndex + 1));
    }
    else
    {
      return StringUtils.stripMnemonic(embeddedTextString);
    }
  }

  private BoundValue _textWithAccessKey;
  private boolean    _returnAccessKey;
}
