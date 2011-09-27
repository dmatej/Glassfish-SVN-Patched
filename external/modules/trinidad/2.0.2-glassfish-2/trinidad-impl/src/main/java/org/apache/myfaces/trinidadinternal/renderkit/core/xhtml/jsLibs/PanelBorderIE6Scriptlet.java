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

package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs;

import java.io.IOException;
import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidad.context.RenderingContext;


// Since IE6 cannot stretch components using docking, we are forced to use
// JavaScript to do the dirty work
public class PanelBorderIE6Scriptlet extends Scriptlet
{
  static public Scriptlet sharedInstance()
  {
    return _sInstance;
  }
  
  @Override
  public Object getScriptletKey()
  {
    return "TrPanelBorderLayoutResizeIE6()";
  }

  @Override
  protected void outputScriptletContent(
    FacesContext context,
    RenderingContext rc
    ) throws IOException
  {
    context.getResponseWriter().writeText(IE_JS_CODE, null);
  }
  
  private PanelBorderIE6Scriptlet()
  {
    super();
  }
  
  
  private static final String IE_JS_CODE =
    "function TrPanelBorderLayoutResizeIE6(elem)\n" +
    "{\n" +
    "  var id = elem.id;\n" +
    "  var innerElems = elem._trInnerElems;\n" +
    "  if (innerElems == null)\n" +
    "  {\n" +
    "    innerElems = elem._trInnerElems = new Array(\n" +
    "      document.getElementById(id + '::top'),\n" +
    "      document.getElementById(id + '::bottom'),\n" +
    "      document.getElementById(id + '::left'),\n" +
    "      document.getElementById(id + '::right'),\n" +
    "      document.getElementById(id + '::center'),\n" +
    "      document.getElementById(id + '::innerTop'),\n" +
    "      document.getElementById(id + '::innerBottom'),\n" +
    "      document.getElementById(id + '::innerLeft'),\n" +
    "      document.getElementById(id + '::innerRight'),\n" +
    "      document.getElementById(id + '::innerCenter'));\n" +
    "  }\n" +
    "  var topHeight = innerElems[0] ? innerElems[0].offsetHeight : 0;\n" +
    "  var bottomHeight = innerElems[1] ? innerElems[1].offsetHeight : 0;\n" +
    "  var leftWidth = innerElems[2] ? innerElems[2].offsetWidth : 0;\n" +
    "  var rightWidth = innerElems[3] ? innerElems[3].offsetWidth : 0;\n" +
    "  var center = innerElems[4];\n" +
    "  var innerCenter = innerElems[9];\n" +
    "  var width = elem.clientWidth;\n" +
    "  var height = elem.clientHeight;\n" +
    "  var centerHeight = (height - topHeight - bottomHeight);\n" +
    "  var centerWidth = (width - leftWidth - rightWidth);\n" +
    "  center.style.height = centerHeight + 'px';\n" +
    "  center.style.width = centerWidth + 'px';\n" +
    "  if (innerElems[2]) innerElems[2].style.height = centerHeight + 'px';\n" +
    "  if (innerElems[3]) innerElems[3].style.height = centerHeight + 'px';\n" +
    "  var innerTopHeight = innerElems[5] ? innerElems[5].offsetHeight : 0;\n" +
    "  var innerBottomHeight = innerElems[6] ? innerElems[6].offsetHeight : 0;\n" +
    "  var innerLeftWidth = innerElems[7] ? innerElems[7].offsetWidth : 0;\n" +
    "  var innerRightWidth = innerElems[8] ? innerElems[8].offsetWidth : 0;\n" +
    "  var innerCenterHeight = (centerHeight - innerTopHeight - innerBottomHeight);\n" +
    "  var innerCenterWidth = (centerWidth - innerLeftWidth - innerRightWidth);\n" +
    "  innerCenter.style.height = innerCenterHeight + 'px';\n" +
    "  innerCenter.style.width = innerCenterWidth + 'px';\n" +
    "  if (innerElems[7]) innerElems[7].style.height = innerCenterHeight + 'px';\n" +
    "  if (innerElems[8]) innerElems[8].style.height = innerCenterHeight + 'px';\n" +
    "}";
  
  
  static private final Scriptlet _sInstance = new PanelBorderIE6Scriptlet();
  
}
