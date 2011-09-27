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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.FacesEvent;

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXNavigationTree;
import org.apache.myfaces.trinidad.component.UIXPage;
import org.apache.myfaces.trinidad.component.UIXTree;
import org.apache.myfaces.trinidad.component.UIXTreeTable;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.FocusEvent;
import org.apache.myfaces.trinidad.event.RowDisclosureEvent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.HiddenLabelUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TableRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;

/**
 * Class that handles all tree related events
 */
public final class TreeUtils
{
  private TreeUtils()
  {
  }

  // Names of the parameters used by TreeUtils.  The names
  // need to be public so form value can be prepared;
  // the meaning of the parameters are not public
  public static final String EVENT_PARAM = "event";
  public static final String SOURCE_PARAM = "source";


  /**
   * This method is used for writing the depth of node from it's root in
   * the case tree and HGrid, thus making it more accessible.
   */
  public static void writeNodeLevel(
    FacesContext          context,
    RenderingContext   arc,
    int              depth,
    String           nodeLevelTextKey
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("label", null);
    String pattern = arc.getTranslatedString(nodeLevelTextKey);
    String levelInfo =
      XhtmlUtils.getFormattedString(pattern,
                                    new String[]
                                    {
                                      IntegerUtils.getString(depth)
                                    });

    if (HiddenLabelUtils.supportsHiddenLabels(arc))
      XhtmlRenderer.renderStyleClass(context, arc,
                         SkinSelectors.HIDDEN_LABEL_STYLE_CLASS);

    writer.writeText(levelInfo, null);
    writer.endElement("label");
  }

  // TODO: this method should be removed.
  public static Object getFocusRowKey(UIXTreeTable treeTable)
  {
    Object path = treeTable.getFocusRowKey();
    if (path == null)
    {
      int oldIndex = treeTable.getRowIndex();
      treeTable.setRowIndex(0);
      Object rowKey = treeTable.getRowKey();
      treeTable.setRowIndex(oldIndex);
      return rowKey;
    }
    else
      return path;
  }

  /**
   * Returns tree focus node client id if any node is focused. Otherwise it
   * returns <tt>null</tt>.
   * 
   * @param context faces context
   * @param tree tree component
   * @return focus node client id
   */
  public static String getFocusNodeClientId(FacesContext context, UIXHierarchy tree) 
  {
    String focusNodeClientId = null;
    Object oldRowKey = tree.getRowKey();
    
    try 
    {
      Object focusRowKey = tree.getFocusRowKey();
      
      if (focusRowKey != null) 
      {
        tree.setRowKey(focusRowKey);
        focusNodeClientId = tree.getClientId(context);
      }
    }
    finally
    {
      tree.setRowKey(oldRowKey);
    }
    
    return focusNodeClientId;
  }

  /**
   * Utility method to expand the focusRowKey during initial
   * rendering.
   * @param tree
   */
  public static void expandFocusRowKey(UIXTree tree)
  {
    if (!RequestContext.getCurrentInstance().isPostback())
    {
      Object focusRowKey = tree.getFocusRowKey();
      if ( focusRowKey != null)
      {
        List<Object> focusPath = 
          new ArrayList<Object>(tree.getAllAncestorContainerRowKeys(focusRowKey));
        focusPath.add(focusRowKey);
        int size = focusPath.size();
        RowKeySet disclosedRowKeys = tree.getDisclosedRowKeys();
        for ( int i = 0 ; i < size; i++)
        {
          Object subkey = focusPath.get(i);
          disclosedRowKeys.add(subkey);
        }     
      }
    }
  }

  /**
   * Utility method to default the focusRowKey to the root node, if
   * it is not set
   * @param tree
   */
  public static void setDefaultFocusRowKey(UIXTree tree)
  {
    Object path = tree.getFocusRowKey();
    if (path == null)
    {
      Object oldKey = tree.getRowKey();
      tree.setRowKey(null);
      tree.setRowIndex(0);
      if (tree.isRowAvailable())
      {
        tree.setFocusRowKey(tree.getRowKey());
      }
      tree.setRowKey(oldKey);
    }    
  }  

  /**
   * writes the JS function needed for generating tree events.
   * @param buff the function source is appended to this buffer
   * @return
   */
  public static StringBuffer setupTreeCollectionComponent(
    StringBuffer buff,
    boolean      validate)
  {
    String validateString = validate?"1":"0";
    buff.append
      ("CollectionComponent.defineTree('"+
        EVENT_PARAM+"','"+
        SOURCE_PARAM+"','"+
        _PATH_PARAM+"','"+
        _START_PARAM+"','"+
        _GOTO+"','"+
        _FOCUS+"'," +
        validateString + ");"
      );

    return buff;
  }

  public static String setupJSTreeCollectionComponent(boolean validate)
  {
    String validateString = validate?"1":"0";
    return
       "CollectionComponent.defineTree('"+
        EVENT_PARAM+"','"+
        SOURCE_PARAM+"','"+
        _PATH_PARAM+"','"+
        _START_PARAM+"','"+
        _GOTO+"','"+
        _FOCUS+ "'," +
        validateString + ")";
  }

  public static String setupJSMultiSelectCollectionComponent(
    String selectedKey,
    String selectedModeKey,
    boolean autoSubmit)
  {
    return
      "CollectionComponent.defineMultiSelect('"+
        selectedKey+"','"+
        selectedModeKey+"',"+(autoSubmit ? "true" : "false")+")";
  }

  public static String createNewJSCollectionComponentState(String formName, String treeClientId)
  {
    // must not end with a ";". This is because this might be passed as an
    // argument to some other function:
    return "new CollectionComponent('"+formName+"','"+treeClientId+"')";
  }

  public static String callJSExpandNode(UIXHierarchy tree, String jsVarName,
                                        boolean isExpand)
  {
    String path = _getPathParam(tree);
    return jsVarName+".action('"+(isExpand ? _SHOW : _HIDE)+
      "','"+path+"',this);return false;";
  }

  public static String callJSGotoNode(UIXHierarchy tree, String jsVarName,
                                      int rangeStart)
  {
    String path = _getPathParam(tree);
    return jsVarName+".range('"+path+"',"+rangeStart+");return false;";
  }

  public static String callJSFocusNode(UIXHierarchy tree, String jsVarName)
  {
    String path = _getPathParam(tree);
    return jsVarName+".focus('"+path+"',this);return false;";
  }

  public static String callJSExpandAll(UIXHierarchy tree, String jsVarName,
                                      boolean isExpand)
  {
    return jsVarName+".action('"+(isExpand ? _SHOW : _HIDE)+
      "','"+_ALL_VALUE+"',this);return false;";
  }

  public static String callJSSelectAll(String jsVarName, boolean isSelectAll)
  {
    return jsVarName+".multiSelect("+(isSelectAll ? "true" : "false")+");return false;";
  }

  public static void decodeGotoEvent(final Map<String, String> parameters,
                                     UIComponent tree)
  {
    Object event = parameters.get(EVENT_PARAM);
    if (_GOTO.equals(event))
    {
      PreserveState preserve = new PreserveState()
      {
        @Override
        protected void process(UIXHierarchy tree)
        {
          final int newStart;
          String startParam = parameters.get(_START_PARAM);
          if ((startParam == null) || ("".equals(startParam)))
          {
            // this must be a root level range change:
            startParam = parameters.get(XhtmlConstants.VALUE_PARAM);
            newStart = Integer.parseInt(startParam) - 1; // value is based at one.
            Object focusRowKey = tree.getFocusRowKey();
            tree.setRowKey(focusRowKey);
            tree.setRowIndex(newStart);
            // queue a focusChange event as well as range change event.
            // TODO - The FocusRowKey has not changed only the start row index has changed.
            // Is queueing a FocusEvent really necessary?
            new FocusEvent(tree, focusRowKey,  focusRowKey).queue();
          }
          else // large record set navigation
          {
            // set the currency to be the container that was scrolled:
            _restorePathFromParam(parameters, tree);
            newStart = Integer.parseInt(startParam);
          }
          TableRenderer.createRangeChangeEvent(tree, newStart).queue();
        }
      };
      preserve.run((UIXHierarchy) tree);
    }
  }

  public static void decodeFocusEvent(final Map<String, String> parameters,
                                      UIComponent tree)
  {
    Object event = parameters.get(EVENT_PARAM);
    if (_FOCUS.equals(event))
    {
      PreserveState preserve = new PreserveState()
      {
        @Override
        protected void process(UIXHierarchy tree)
        {
          // if the current focusRowKey is null,  and the FocusRowKey attribute
          // of the component is EL-bound,  this will call the EL getter, otherwise
          // we get the "local" FocusRowKey set on the component
          Object oldKey = tree.getFocusRowKey();
          _restorePathFromParam(parameters, tree);
          Object newKey = tree.getRowKey();
          new FocusEvent(tree, oldKey, newKey).queue();
        }
      };
      preserve.run((UIXHierarchy) tree);
    }
  }

  public static void decodeExpandEvents(final Map<String, String> parameters,
                                        final UIComponent tree,
                                        final Object focusRowKey)
  {
    Object event = parameters.get(EVENT_PARAM);
    final Boolean expand;
    if (_HIDE.equals(event))
    {
      expand = Boolean.FALSE;
    }
    else if (_SHOW.equals(event))
    {
      expand = Boolean.TRUE;
    }
    else
    {
      return;
    }

    PreserveState preserve = new PreserveState()
    {
      @Override
      protected void process(UIXHierarchy tree)
      {
        final FacesEvent event;

        Object key = parameters.get(_PATH_PARAM);
        if (_ALL_VALUE.equals(key)) // expandAll event
        {
          if (focusRowKey == null)
          {
            _LOG.severe("UNEXPECTED_TREE_STATE");
            return;
          }
          else
          {
            tree.setRowKey(focusRowKey);
            RowKeySet old = _getExpandedRowKeys(tree);
            RowKeySet newset = old.clone();
            if (expand)
              newset.addAll();
            else
              newset.removeAll();
            event = new RowDisclosureEvent(old, newset, tree);
          }
        }
        else  // expand/collapse event
        {
          _restorePathFromParam(parameters, tree);
          RowKeySet old = _getExpandedRowKeys(tree);
          RowKeySet newset = old.clone();
          newset.setContained(expand);
          event = new RowDisclosureEvent(old, newset, tree);
        }
        event.queue();
      }
    };

    preserve.run((UIXHierarchy) tree);
  }
  
  /**
   * Returns <code>String</code> object containing encoded 
   * parameter name and value pair for Non-JavaScript browsers.
   */   
  public static String renderEncodedNameAttri(
                    FacesContext context,
                    RenderingContext rc,
                    UIXHierarchy tree,
                    String treeName,
                    boolean isExpand) 
    throws IOException
  {

    return XhtmlUtils.getEncodedNameAttribute ( 
                       // Array should be in the order of name
                       // and value pair
                          new String[]{ XhtmlConstants.SOURCE_PARAM,
                                        treeName,
                                        XhtmlConstants.EVENT_PARAM,
                                        isExpand ? _SHOW : _HIDE,
                                        _PATH_PARAM,
                                        _getPathParam(tree)});
                                        
  }

  private static RowKeySet _getExpandedRowKeys(UIXHierarchy tree)
  {
    if (tree instanceof UIXTree)
      return ((UIXTree) tree).getDisclosedRowKeys();
    if (tree instanceof UIXNavigationTree)
      return ((UIXNavigationTree) tree).getDisclosedRowKeys();
    if (tree instanceof UIXPage)
      return ((UIXPage) tree).getDisclosedRowKeys();
    throw new IllegalArgumentException("Don't know how to get disclosedRowKeys from:"+tree);
  }

  private static void _restorePathFromParam(
      Map<String, String> parameters, 
      UIXHierarchy tree)
  {
    String currencyString = parameters.get(_PATH_PARAM);
    tree.setClientRowKey(currencyString);
  }

  private static String _getPathParam(UIXHierarchy tree)
  {
    String currencyString = tree.getClientRowKey();
    return currencyString;
  }

  private abstract static class PreserveState
  {
    public void run(UIXHierarchy tree)
    {
      Object oldPath = tree.getRowKey();
      try
      {
        process(tree);
      }
      finally
      {
        tree.setRowKey(oldPath);
      }
    }

    protected abstract void process(UIXHierarchy tree);
  }

  private static final String _GOTO = "goto";
  private static final String _HIDE = "hide";
  private static final String _SHOW = "show";
  private static final String _FOCUS = "focus";
  private static final String _ALL_VALUE = "all";
  private static final String _PATH_PARAM = "path";
  private static final String _START_PARAM = "start";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TreeUtils.class);
}
