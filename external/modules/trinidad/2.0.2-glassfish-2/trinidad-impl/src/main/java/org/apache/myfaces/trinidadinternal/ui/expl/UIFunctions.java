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
package org.apache.myfaces.trinidadinternal.ui.expl;

import java.util.AbstractMap;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.Icon;

/**
 * Implements some of the EL functions in the
 * { @link UIConstants#MARLIN_NAMESPACE }.
 * In the examples in this javaDoc assume that the prefix 'ui' is bound to
 * { @link UIConstants#MARLIN_NAMESPACE }.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/expl/UIFunctions.java#0 $) $Date: 10-nov-2005.18:56:28 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public final class UIFunctions
{
  /**
   * Given the logical name of a parameter, return the parameter
   * key that should be used in the URL.  This function should not
   * be used for parameter values.
   * Example: <pre>
   * &lt;formValue name="${ui:encodeParameter(uix,'event')}"
   *            value="viewSource"/&gt;
   * </pre>
   * @see org.apache.myfaces.trinidadinternal.share.url.URLEncoder#encodeParameter
   */
  public static String encodeParameter(UIImplicitObject uix,
                                       String parameterName)
  {
    return
      uix.getRenderingContext().getURLEncoder().encodeParameter(parameterName);
  }


  /**
   * Returns arg1 unless it is null, in which case returns arg2.
   * Example: <pre> ${ui:defaulting(null,'The default')}</pre>
   */
  public static Object defaulting(Object arg1, Object arg2)
  {
    return (arg1 == null) ? arg2 : arg1;
  }

  /**
   * Performs a test.
   * @param test the condition to test
   * @param ifTrue the object to return if the test is true
   * @param ifFalse the object to return if the test is false
   * @return ifTrue if test is true. IfFalse otherwise.  Example:<pre>
   * ${ui:cond(blockSize > 100, 'Too Many Records', 'Detailed List')}</pre>
   */
  public static Object cond(boolean test, Object ifTrue, Object ifFalse)
  {
    return test ? ifTrue : ifFalse;
  }

  /**
   * Inserts the soft hyphen character.  The following example inserts a soft
   * hyphen after each '/' character in the directory string:<pre>
   * ${ui:hyphenate(uix, '/src/oracle/cabo/ui/laf/oracle/desktop/table','/')}</pre>
   * @param text the soft hyphen character, &amp;shy; , is inserted into this
   * string.
   * @param breakChars &amp;shy; is inserted after each occurence of any of
   * the characters in this string.
   */
  public static String hyphenate(UIImplicitObject uix,
                                 String text, String breakChars)
  {
    UIXRenderingContext rc = uix.getRenderingContext();
    TrinidadAgent.Application agentApp = rc.getAgent().getAgentApplication();


    // bug 3364275
    // possible characters to use to indicate to the browser where it is safe
    // to break a line:

    // &shy; 0xAD - HTML Spec, but does not work in Moz 1.6. The character is
    // part of the string during cut/paste operations.
    // 0x200B - HTML Spec. works in both IE and Moz. The character is
    // part of the string during cut/paste operations.

    // <wbr> - not in HTML spec. works in IE3+ and Moz 1.6. Does not work in
    // Safari 1.2. The character is not
    // part of the string during cut/paste operations.


    final char breakHint;
    if ((agentApp == TrinidadAgent.Application.IEXPLORER) ||
        (agentApp == TrinidadAgent.Application.GECKO) ||
        (agentApp == TrinidadAgent.Application.NETSCAPE))
    {
      // insert the special unicode character that tells the output method to
      // use the <wbr> tag. This character is recognized by HTMLEscapes:
      breakHint = 0x2027;
    }
    else
    {
      //breakHint = 0x200B; // zero width space
      breakHint = 0x200B; // &shy; 
    }

    int sz = text.length();
    StringBuffer sb = new StringBuffer(sz);
    for(int i=0; i<sz; i++)
    {
      char curr = text.charAt(i);
      sb.append(curr);
      if (breakChars.indexOf(curr) >= 0)
        sb.append(breakHint);
    }
    return sb.toString();
  }

  /**
   * Concatenates two strings.
   * Example:<pre> ${ui:concat('Julius','Caesar')}</pre>
   */
  public static String concat(String str1, String str2)
  {
    return str1 + str2;
  }

  /**
   * Gets the Skin image Icon for the specified namespace/name.
   * <p>
   * The object that is returned returned provides access to three
   * values:
   * <ul>
   * <li>uri: The URI of the image icon
   * <li>width: The width of the image icon
   * <li>height: The height of the image icon
   * </ul>
   * 
   * @param uix The UIX implicit object
   * @param namespace The namespace of the Image to retrieve
   * @param name The name of the icon to retrieve.
   * @todo REMOVE
   */
  public static Object imageIcon(
    UIImplicitObject uix,
    String           name
    )
  {
    // Make sure we have a name
    if (name != null)
    {
      // Get the Icon from the Skin 
      UIXRenderingContext context = uix.getRenderingContext();
      RenderingContext arc = RenderingContext.getCurrentInstance();
      FacesContext fContext = context.getFacesContext();
      Skin skin = context.getSkin();
      Icon icon = skin.getIcon(name);

      Object uri = icon.getImageURI(fContext, arc);

      if (uri != null)
      {
        

        Integer width = icon.getImageWidth(arc);
        Integer height = icon.getImageHeight(arc);

        // =-=ags Perhaps we should consider caching ImageData 
        //        instances in request/render scope, so that repeated 
        //        calls to ui:imageIcon() during the same page render
        //        return the same ImageData object?  We would also
        //        avoid repeated and possibly unnecessary calls
        //        to Icon.getImageURI(), which might be expensive.

        return new ImageData(uri, width, height);
      }
    }

    return null;
  }


  // Provides access to Image uri, width and height values.
  private static final class ImageData extends AbstractMap<String, Object>
  {
    public ImageData(
      Object uri,
      Integer width,
      Integer height
      )
    {
      _uri = uri;
      _width = width;
      _height = height;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Set<Map.Entry<String, Object>> entrySet()
    {
      return Collections.EMPTY_SET;
    }

    @Override
    public Object get(Object key)
    {
      Object data = null;

      if ("uri".equals(key))
        data = _uri;
      else if ("width".equals(key))
        data = _width;
      else if ("height".equals(key))
        data = _height;

      return data;
    }

    private Object  _uri;
    private Integer _width;
    private Integer _height;
  }

  private UIFunctions()
  {
  }
}
