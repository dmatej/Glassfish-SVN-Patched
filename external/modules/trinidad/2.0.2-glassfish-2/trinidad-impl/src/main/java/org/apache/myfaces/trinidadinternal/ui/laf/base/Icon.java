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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

import org.apache.myfaces.trinidad.share.io.NameResolver;

import org.apache.myfaces.trinidadinternal.style.CoreStyle;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel;

/**
 * Passed to an LafIconProvider by a look and feel
 * object to represent an icon.
 *
 * @see org.apache.myfaces.trinidadinternal.ui.laf.base.LafIconProvider
 * @see org.apache.myfaces.trinidadinternal.ui.laf.xhtml.IconArrayLafIconProvider
 * @see org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/Icon.java#0 $) $Date: 10-nov-2005.18:53:00 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class Icon
{

  /**
  *
  * @param gifName  name of gif without .gif at the end
  * @param lookAndFeel LookAndFeel class this icon belongs to
  * @param resolver NameResolver used to look up gif
  * @param isCoreColor whether or not icon is core color.
  *          Default should be false.
  * @param isSymmetric whether or not a flipped version needs to be created
  *          for rtl languages. Default should be false.
  * @param isTransparent whether or not image is transparent.
  *          Default should be true.
  */
  public Icon(
    String             gifName,
    Class<LookAndFeel> lookAndFeel,
    NameResolver       resolver,
    boolean            isCoreColor,
    boolean            isSymmetric,
    boolean            isTransparent
  )
  {
    this(gifName, 
         lookAndFeel, 
         resolver, 
         isCoreColor, 
         isSymmetric, 
         isTransparent, 
         null, 
         null);
  }

  /**
  *
  * @param gifName  name of gif without .gif at the end
  * @param lookAndFeel LookAndFeel class this icon belongs to
  * @param resolver NameResolver used to look up gif
  * @param isCoreColor whether or not icon is core color.
  *          Default should be false.
  * @param isSymmetric whether or not a flipped version needs to be created
  *          for rtl languages. Default should be false.
  * @param isTransparent whether or not image is transparent.
  *          Default should be true.
   * @param styleClass The style class for the image icon
   * @param inlineStyle The inline style for the image icon
  */
  public Icon(
    String             gifName,
    Class<LookAndFeel> lookAndFeel,
    NameResolver       resolver,
    boolean            isCoreColor,
    boolean            isSymmetric,
    boolean            isTransparent,
    String             styleClass,
    CoreStyle              inlineStyle
  )
  {
    this(gifName, isCoreColor, isSymmetric, isTransparent);
    
    if ( lookAndFeel == null ||
         resolver == null )
      throw new IllegalArgumentException();

    _lookAndFeel   = lookAndFeel;
    _resolver      = resolver;
    _styleClass    = styleClass;
    _inlineStyle   = inlineStyle;
  }


  /**
  *
  * @param gifName  name of gif without .gif at the end
  * @param isCoreColor whether or not icon is core color.
  *          Default should be false.
  * @param isSymmetric whether or not a flipped version needs to be created
  *          for rtl languages. Default should be false.
  * @param isTransparent whether or not image is transparent.
  *          Default should be true.
  */
  public Icon(
    String       gifName,
    boolean      isCoreColor,
    boolean      isSymmetric,
    boolean      isTransparent
  )
  {
    if ( gifName == null )
      throw new IllegalArgumentException();

    _gifName       = gifName;
    _isCoreColor   = isCoreColor;
    _isSymmetric   = isSymmetric;
    _isTransparent = isTransparent;
  }


  public String getName()
  {
    return _gifName;
  }

  public Class<LookAndFeel> getLookAndFeel()
  {
    return _lookAndFeel;
  }

  public NameResolver getNameResolver()
  {
    return _resolver;
  }

 /*
  * default is false
  */
  public boolean isCoreColor()
  {
    return  _isCoreColor ;
  }

 /*
  * default is false
  */
  public boolean isSymmetric()
  {
    return  _isSymmetric ;
  }

 /*
  * default is true
  */
  public boolean isTransparent()
  {
    return  _isTransparent ;
  }

  /**
   * Returns the style class
   */
  public String getStyleClass()
  {
    return _styleClass;
  }

  /**
   * Returns the inline style
   */
  public CoreStyle getInlineStyle()
  {
    return _inlineStyle;
  }

  private String _gifName ;
  private Class<LookAndFeel> _lookAndFeel ;
  private NameResolver _resolver;

  // core is blue in blaf
  private boolean _isCoreColor;

  // is image direction independent
  private boolean _isSymmetric;

  // is image transparent
  private boolean _isTransparent;

  private String _styleClass;
  private CoreStyle  _inlineStyle;
}
