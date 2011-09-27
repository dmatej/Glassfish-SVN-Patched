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
package org.apache.myfaces.trinidadinternal.renderkit;

import org.xml.sax.SAXException;

import java.io.IOException;
import java.util.ArrayList;

import javax.faces.context.FacesContext;
import javax.faces.component.UIColumn;
import javax.faces.component.UIComponentBase;
import javax.faces.component.UIViewRoot;
import javax.faces.component.html.HtmlDataTable;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.component.html.HtmlSelectOneRadio;
import javax.faces.component.UISelectItem;

import org.apache.myfaces.trinidad.component.core.data.CoreColumn;
import org.apache.myfaces.trinidad.component.core.data.CoreTable;
import org.apache.myfaces.trinidad.component.core.data.CoreSelectRangeChoiceBar;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelGroupLayout;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputText;
import org.apache.myfaces.trinidad.component.core.input.CoreInputHidden;
import org.apache.myfaces.trinidad.component.core.input.CoreInputText;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectOneChoice;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectOneRadio;
import org.apache.myfaces.trinidad.component.core.input.CoreInputDate;


public class CoreRenderKitPerf extends RenderKitPerfTestCase
{
  public CoreRenderKitPerf() throws IOException, SAXException
  {
  }

  public CoreRenderKitPerf(String testName) throws IOException, SAXException
  {
    super(testName);
  }

  public void testSimplePerf() throws IOException
  {
    CoreOutputText out = _createCoreOutputText();
    out.setValue("Plain value");
    UIViewRoot root = createTestTree(out, "testSimplePerf()");
    renderRoot(root);

    root = createTestTree(out, "testSimplePerf() 2");
    renderRoot(root);
  }

  public void testManyAttributes() throws IOException
  {
    CoreOutputText out = _createCoreOutputText();
    out.setValue("Plain value");
    out.setEscape(true);
    out.setId("OutId");
    out.setShortDesc("Short Desc");
    out.setStyleClass("Style Class");
    out.setOnclick("on click");

    /*
    out.setTruncateAt(100);
    out.setOnclick("on click");
    out.setOndblclick("on dblclick");
    out.setOnkeydown("on keydown");
    out.setOnkeyup("on keyup");
    out.setOnkeypress("on keypress ");
    out.setOnmousedown("on mousedown");
    out.setOnmousemove("on mousemove");
    out.setOnmouseout("on mouseout ");
    out.setOnmouseover("on mouseover ");
    out.setOnmouseup("on mouseup ");*/

    UIViewRoot root = createTestTree(out, "testManyAttributes()");
    renderRoot(root);

    root = createTestTree(out, "testManyAttributes() 2");
    renderRoot(root);
  }

  public void testManyInputAttributes() throws IOException
  {
    CoreInputText out = new CoreInputText();

    out.setSimple(true);
    out.setValue("Plain value");
    out.setId("OutId");
    out.setShortDesc("Short Desc");
    out.setStyleClass("Style Class");
    out.setOnclick("on click");

    out.setOnclick("on click");
    out.setOndblclick("on dblclick");
    out.setOnkeydown("on keydown");
    out.setOnkeyup("on keyup");
    out.setOnkeypress("on keypress ");
    out.setOnmousedown("on mousedown");
    out.setOnmousemove("on mousemove");
    out.setOnmouseout("on mouseout ");
    out.setOnmouseover("on mouseover ");
    out.setOnmouseup("on mouseup ");

    UIViewRoot root = createTestTree(out, "testManyInputAttributes()");
    renderRoot(root);

    root = createTestTree(out, "testManyInputAttributes() 2");
    renderRoot(root);
  }

  public void testRIManyInputAttributes() throws IOException
  {
    HtmlInputText out = new HtmlInputText();
    out.setValue("Plain value");
    out.setId("OutId");
    out.setTitle("Title");
    out.setStyleClass("Style Class");
    out.setOnclick("on click");

    out.setOnclick("on click");
    out.setOndblclick("on dblclick");
    out.setOnkeydown("on keydown");
    out.setOnkeyup("on keyup");
    out.setOnkeypress("on keypress ");
    out.setOnmousedown("on mousedown");
    out.setOnmousemove("on mousemove");
    out.setOnmouseout("on mouseout ");
    out.setOnmouseover("on mouseover ");
    out.setOnmouseup("on mouseup ");

    UIViewRoot root = createTestTree(out, "testRIManyInputAttributes()");
    renderRoot(root);

    root = createTestTree(out, "testRIManyInputAttributes() 2");
    renderRoot(root);
  }


  public void testManyNonSimpleInputAttributes() throws IOException
  {
    CoreInputText out = new CoreInputText();
    out.setSimple(false);
    out.setValue("Plain value");
    out.setId("OutId");
    out.setShortDesc("Short Desc");
    out.setStyleClass("Style Class");
    out.setOnclick("on click");

    out.setOnclick("on click");
    out.setOndblclick("on dblclick");
    out.setOnkeydown("on keydown");
    out.setOnkeyup("on keyup");
    out.setOnkeypress("on keypress ");
    out.setOnmousedown("on mousedown");
    out.setOnmousemove("on mousemove");
    out.setOnmouseout("on mouseout ");
    out.setOnmouseover("on mouseover ");
    out.setOnmouseup("on mouseup ");

    UIViewRoot root = createTestTree(out, "testManyNonSimpleInputAttributes()");
    renderRoot(root);

    root = createTestTree(out, "testManyNonSimpleInputAttributes() 2");
    renderRoot(root);
  }


  public void testRISimplePerf() throws IOException
  {
    HtmlOutputText out = _createHtmlOutputText();
    out.setValue("Plain value");
    UIViewRoot root = createTestTree(out, "testRISimplePerf()");
    renderRoot(root);

    root = createTestTree(out, "testRISimplePerf() 2");
    renderRoot(root);
  }

  public void testRIManyAttributes() throws IOException
  {
    HtmlOutputText out = _createHtmlOutputText();
    out.setValue("Plain value");
    out.setEscape(true);
    out.setId("OutId");
    out.setTitle("Short Desc");
    out.setStyleClass("Style Class");
    out.setStyle("The Style");

    UIViewRoot root = createTestTree(out, "testRIManyAttributes()");
    renderRoot(root);

    root = createTestTree(out, "testRIManyAttributes() 2");
    renderRoot(root);
  }


  @SuppressWarnings("unchecked")
  public void testRITable() throws IOException
  {
    HtmlDataTable table = new HtmlDataTable();
    ArrayList<Integer> l = new ArrayList<Integer>();
    for (int i = 0 ; i < 10; i++)
      l.add(new Integer(i));
    
    table.setValue(l);
    table.setStyleClass("TableContent");
    table.setHeaderClass("af_column_header-text SomeBorderStyle");
    table.setColumnClasses("af_column_cell-text OraTableBorder1111");

    for (int i = 0 ; i < 3; i++)
    {
      UIColumn col = new UIColumn();
      /*
      HtmlOutputText header = new HtmlOutputText();
      header.setValue("Header " + i);
      col.setHeader(header);
      //HtmlOutputText text = new HtmlOutputText();
      HtmlInputText text = new HtmlInputText();
      text.setValue("Column " + i);
      col.getChildren().add(text);
      */

      col.setHeader(new NullComp());
      col.getChildren().add(new NullComp());
      table.getChildren().add(col);
    }

    UIViewRoot root = createTestTree(table, "testRITable()", 500);
    renderRoot(root);
  }


  @SuppressWarnings("unchecked")
  public void testTable() throws IOException
  {
    CoreTable table = new CoreTable();
    ArrayList<Integer> l = new ArrayList<Integer>();
    for (int i = 0 ; i < 10; i++)
      l.add(new Integer(i));
    table.setValue(l);

    for (int i = 0 ; i < 3; i++)
    {
      CoreColumn col = new CoreColumn();

      /*
      col.setHeaderText("Header " + i);
      // Use headerText instead of adding the facet; easier and faster.
      //CoreOutputText header = new CoreOutputText();
      //header.setValue("Header " + i);
      //col.setHeader(header);
      //CoreOutputText text = new CoreOutputText();
      CoreInputText text = new CoreInputText();
      text.setSimple(true);
      text.setValue("Column " + i);
      col.getChildren().add(text);
      */
      col.setHeader(new NullComp());
      col.getChildren().add(new NullComp());
      table.getChildren().add(col);
    }

    UIViewRoot root = createTestTree(table, "testTable()", 500);
    renderRoot(root);
  }


  @SuppressWarnings("unchecked")
  public void testPanelGroupPerf() throws IOException
  {
    CorePanelGroupLayout group = new CorePanelGroupLayout();
    group.setLayout(CorePanelGroupLayout.LAYOUT_VERTICAL);
    for (int i = 0; i < 8; i++)
    {
      /*
      CoreOutputText out = _createCoreOutputText();
      out.setValue("Plain value");
      group.getChildren().add(out);*/
      group.getChildren().add(new NullComp());
    }

    UIViewRoot root = createTestTree(group, "testPanelGroup()");
    renderRoot(root);

    root = createTestTree(group, "testPanelGroup() 2");
    renderRoot(root);
  }

  public void testInputHidden() throws IOException
  {
    CoreInputHidden cih = new CoreInputHidden();
    cih.setValue("value");

    UIViewRoot root = createTestTree(cih, "testInputHidden");
    renderRoot(root);
  }


  public void notestSelectRangeChoicePerf() throws IOException
  {
    CoreSelectRangeChoiceBar bar = new CoreSelectRangeChoiceBar();
    bar.setValue(new MVariableResolver.BigList());
    bar.setRows(30);

    UIViewRoot root = createTestTree(bar, "testSelectRangeChoiceBar");
    renderRoot(root);
  }


  public void notestSelectInputDate() throws IOException
  {
    CoreInputDate date = new CoreInputDate();
    date.setLabel("Label");

    UIViewRoot root = createTestTree(date, "testSelectInputDate");
    renderRoot(root);
  }

  @SuppressWarnings("unchecked")
  public void testSelectOneChoice() throws IOException
  {
    CoreSelectOneChoice choice = new CoreSelectOneChoice();
    choice.setSimple(true);
    choice.setValue(new Integer(2));
    for (int i = 0 ; i < 10; i++)
    {
      UISelectItem selectItem  = new UISelectItem();
      selectItem.setItemLabel("Item " + i);
      selectItem.setItemValue(new Integer(i));
      choice.getChildren().add(selectItem);
    }

    UIViewRoot root = createTestTree(choice, "testSelectOneChoice");
    renderRoot(root);

    root = createTestTree(choice, "testSelectOneChoice2");
    renderRoot(root);
  }


  @SuppressWarnings("unchecked")
  public void testRISelectOneMenu() throws IOException
  {
    //
    HtmlSelectOneMenu menu = new HtmlSelectOneMenu();
    menu.setValue(new Integer(2));
    for (int i = 0 ; i < 10; i++)
    {
      UISelectItem selectItem  = new UISelectItem();
      selectItem.setItemLabel("Item " + i);
      selectItem.setItemValue("" + i);
      menu.getChildren().add(selectItem);
    }

    UIViewRoot root = createTestTree(menu, "testRISelectOneMenu");
    renderRoot(root);

    root = createTestTree(menu, "testRISelectOneNavigation2");
    renderRoot(root);
  }


  @SuppressWarnings("unchecked")
  public void testSelectOneRadio() throws IOException
  {
    CoreSelectOneRadio radio = new CoreSelectOneRadio();
    radio.setSimple(true);
    radio.setValue(new Integer(2));
    for (int i = 0 ; i < 10; i++)
    {
      UISelectItem selectItem  = new UISelectItem();
      selectItem.setItemLabel("Item " + i);
      selectItem.setItemValue(new Integer(i));
      radio.getChildren().add(selectItem);
    }

    UIViewRoot root = createTestTree(radio, "testSelectOneRadio");
    renderRoot(root);

    root = createTestTree(radio, "testSelectOneRadio2");
    renderRoot(root);
  }


  @SuppressWarnings("unchecked")
  public void testRISelectOneRadio() throws IOException
  {
    //
    HtmlSelectOneRadio radio = new HtmlSelectOneRadio();
    radio.setValue(new Integer(2));
    for (int i = 0 ; i < 10; i++)
    {
      UISelectItem selectItem  = new UISelectItem();
      selectItem.setItemLabel("Item " + i);
      selectItem.setItemValue("" + i);
      radio.getChildren().add(selectItem);
    }

    UIViewRoot root = createTestTree(radio, "testRISelectOneRadio");
    renderRoot(root);

    root = createTestTree(radio, "testRISelectOneRadio2");
    renderRoot(root);
  }

  private HtmlOutputText _createHtmlOutputText()
  {
    return new HtmlOutputText();
  }

  private CoreOutputText _createCoreOutputText()
  {
    return new CoreOutputText();
    /*
    {
      public Renderer getRenderer(FacesContext context)
      {
        // Simulate the overhead of retrieving the renderer from the RenderKit
        return super.getRenderer(context);
        //return _fastRenderer;
      }
    };*/
  }

  static private class NullComp extends UIComponentBase
  {
    public NullComp()
    {
      setRendererType(null);
    }

    @Override
    public String getFamily()
    {
      return "org.apache.myfaces.trinidadtest.PerfComp";
    }

    @Override
    public boolean getRendersChildren()
    {
      return true;
    }

    @Override
    public void encodeBegin(FacesContext context) throws IOException
    {
    }

    @Override
    public void encodeChildren(FacesContext context) throws IOException
    {
    }

    @Override
    public void encodeEnd(FacesContext context) throws IOException
    {
    }
  }
}
