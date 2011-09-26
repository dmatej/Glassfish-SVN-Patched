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
package org.apache.myfaces.trinidadinternal.menu;

import java.io.InputStream;
import java.io.IOException;
import java.io.Serializable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import java.util.Map;

import java.util.Stack;

import javax.faces.context.FacesContext;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.ChildPropertyTreeModel;
import org.apache.myfaces.trinidad.model.TreeModel;
import org.apache.myfaces.trinidad.model.XMLMenuModel;
import org.apache.myfaces.trinidad.model.XMLMenuModel.MenuContentHandler;

import org.xml.sax.helpers.DefaultHandler;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * Handler called by the SAXParser when parsing menu metadata
 * as part of the XML Menu Model of Trinidad Faces.
 * <p>
 * This is called through the Services API (See XMLMenuModel.java) to
 * keep the separation between API's and internal modules.
 * <p>
 * startElement() and endElement() are called as one would expect,
 * at the start of parsing an element in the menu metadata file and
 * at the end of parsing an element in the menu metadata file.
 *
 * The menu model is created as a List of itemNodes and groupNodes
 * which is available to and used by the XMLMenuModel to create the
 * TreeModel and internal Maps.
 *
 */
 /*
  * IMPORTANT NOTE: Much of the work and data structures used by the
  * XMLMenuModel are created (and kept) in this class.  This is necessarily the
  * case because the scope of the XMLMenuModel is request.  The
  * MenuContentHandlerImpl is shared so it does not get rebuilt upon each
  * request as the XMLMenuModel does. So the XMLMenuModel can get its data
  * each time it is rebuilt (on each request) without having to reparse and
  * recreate all of its data structures.  It simply gets them from here.
  *
  * As well as the tree, three hashmaps are created in order to be able to
  * resolve cases where multiple menu items cause navigation to the same viewId.
  * All 3 of these maps are created after the metadata is parsed and the tree is
  * built, in the _addToMaps method.
  *
  * o The first hashMap is called the viewIdFocusPathMap and is built by
  * traversing the tree after it is built (see endDocument()).
  * Each node's focusViewId is
  * obtained and used as the key to an entry in the viewIdHashMap.  An ArrayList
  * is used as the entry's value and each item in the ArrayList is a node's
  * rowkey from the tree. This allows us to have duplicate rowkeys for a single
  * focusViewId which translates to a menu that contains multiple items pointing
  * to the same page. In general, each entry will have an ArrayList of rowkeys
  * with only 1 rowkey, AKA focus path.
  * o The second hashMap is called the nodeFocusPathMap and is built at the
  * same time the viewIdHashMap is built. Each entry's key is the actual node
  * and the value is the row key.  Since the model keeps track of the currently
  * selected menu node, this hashmap can be used to resolve viewId's with
  * multiple focus paths.  Since we have the currently selected node, we just
  * use this hashMap to get its focus path.
  * o The third hashMap is called idNodeMap and is built at the same time as the
  * previous maps.  This map is populated by having each entry contain the node's
  * id as the key and the actual node as the value.  In order to keep track of
  * the currently selected node in the case of a GET, the node's id is appended
  * to the request URL as a parameter.  The currently selected node's id is
  * picked up and this map is used to get the actual node that is currently
  * selected.
  */
public class MenuContentHandlerImpl extends DefaultHandler
                                    implements MenuContentHandler,Serializable
{

  /**
    * Constructs a Menu Content Handler.
    */
  public MenuContentHandlerImpl()
  {
    super();

  }

  /**
   * Called by the SAX Parser at the start of parsing a document.
   */
  @Override
  public void startDocument()
  {
    _nodeDepth = 0;
    _menuNodes = new ArrayList<List<MenuNode>>();
    _menuList  = null;

    // Handler Id will have to change also to be unique
    _handlerId = Integer.toString(System.identityHashCode(_menuNodes));
  }

  /**
    * Start the parsing of an node element entry in the menu metadata file.
    * <p>
    * If the entry is for an itemNode or a destinationNode, create the node
    * and it to the List.  If the entry is for a sharedNode, a new submenu
    * model is created.
    *
    * @param nameSpaceUri - only used when passed to super class.
    * @param localElemName - only used when passed to super class.
    * @param qualifiedElemName - String designating the node type of the entry.
    * @param attrList - List of attributes in the menudata entry.
    * @throws SAXException
    */
  @SuppressWarnings("unchecked")
  @Override
  public void startElement(String nameSpaceUri, String localElemName,
                           String qualifiedElemName, Attributes attrList)
    throws SAXException
  {
    super.startElement(nameSpaceUri, localElemName, qualifiedElemName,
                       attrList);

    if (_ROOT_NODE.equals(qualifiedElemName))
    {
      // Unless both of these are specified, don't attempt to load
      // the resource bundle.
      String resBundle    = attrList.getValue(_RES_BUNDLE_ATTR);
      String resBundleKey = attrList.getValue(_VAR_ATTR);

      if (   (resBundle != null    && !"".equals(resBundle))
          && (resBundleKey != null && !"".equals(resBundleKey))
         )
      {
        // Load the resource Bundle.
        // Ensure the bundle key is unique by appending the
        // handler Id.
        MenuUtils.loadBundle(resBundle, resBundleKey + getHandlerId());
        _resBundleKey  = resBundleKey;
        _resBundleName = resBundle;
      }
    }
    else
    {
      // Either itemNode, destinationNode, or groupNode
      boolean isNonSharedNode = (   _ITEM_NODE.equals(qualifiedElemName)
                                 || _GROUP_NODE.equals(qualifiedElemName)
                                );

      if (isNonSharedNode)
      {
        _currentNodeStyle = (  _ITEM_NODE.equals(qualifiedElemName)
                             ? MenuConstants.NODE_STYLE_ITEM
                             : MenuConstants.NODE_STYLE_GROUP
                            );
        _nodeDepth++;

        if ((_skipDepth >= 0) && (_nodeDepth > _skipDepth))
        {
          // This sub-tree is being skipped, so just return
          return;
        }

        if (_menuNodes.size() < _nodeDepth)
        {
          _menuNodes.add(new ArrayList<MenuNode>());
        }

        _attrMap = _getMapFromList(attrList);

        // Create either an itemNode or groupNode.
        MenuNode menuNode = _createMenuNode();

        if (menuNode == null)
        {
          // No menu item is created, so note that we are
          // now skipping the subtree
          _skipDepth = _nodeDepth;
        }
        else
        {
          if (   (_resBundleName != null && !"".equals(_resBundleName))
              && (_resBundleKey  != null && !"".equals(_resBundleKey))
             )
          {
            menuNode.setResBundleKey(_resBundleKey);
            menuNode.setResBundleName(_resBundleName);
          }

          // Set the node's MenuContentHandlerImpl id so that when
          // the node's getLabel() method is called, we can
          // use the handlerId to insert into the label
          // if it is an EL expression.
          menuNode.setHandlerId(getHandlerId());

          // Set the root model on the node so we can call into
          // the root model from the node to populate its
          // idNodeMap (See XMLMenuModel.java)
          menuNode.setRootModelKey(getRootModelKey());

          // Set the local model (created when parsing a sharedNode)
          // on the node in case the node needs to get back to its
          // local model.
          menuNode.setModelId(getModelId());

          // menu nodes need to know how to refer
          // back to the specific xml menu model
          // so that they can mutate them.
          FacesContext facesContext = FacesContext.getCurrentInstance();
          Map<String, Object> requestMap =
            facesContext.getExternalContext().getRequestMap();
          if(!requestMap.containsKey(XMLMenuModel.SHARED_MODEL_INDICATOR_KEY))
            menuNode.setRootId( (getId() ) );
          else
            menuNode.setRootId((Integer) requestMap.get(XMLMenuModel.SHARED_MODEL_INDICATOR_KEY));

          List<MenuNode> list = _menuNodes.get(_nodeDepth-1);
          list.add(menuNode.getThreadSafeCopy());
        }
      }
      else if (_SHARED_NODE.equals(qualifiedElemName))
      {
        _nodeDepth++;

        // SharedNode's "ref" property points to another submenu's metadata,
        // and thus a new model, which we build here.  Note: this will
        // recursively call into this MenuContentHandlerImpl when parsing the
        // submenu's metadata.
        String expr = attrList.getValue(_REF_ATTR);
        
        // push this only when we are root model
        FacesContext facesContext = FacesContext.getCurrentInstance();
        Map<String, Object> requestMap =
          facesContext.getExternalContext().getRequestMap();
        Integer recurseLevel = (Integer) requestMap.get(_RECURSE_COUNTER);
        if(recurseLevel == null) 
          recurseLevel = 0;
        if(recurseLevel == 0)
          requestMap.put(XMLMenuModel.SHARED_MODEL_INDICATOR_KEY, this.getId());
        
        recurseLevel++;
        requestMap.put(_RECURSE_COUNTER, recurseLevel);
        

        // Need to push several items onto the stack now as we recurse
        // into another menu model.
        _saveModelData();

        // Create the sub menu model specified in the sharedNode
        XMLMenuModel menuModel = (XMLMenuModel)MenuUtils.getBoundValue(expr,
                                                              Object.class);


        // Now must pop the values cause we are back to the parent
        // model.
        _restoreModelData();
        
        recurseLevel = (Integer) requestMap.get(_RECURSE_COUNTER);
        recurseLevel --;
        requestMap.put(_RECURSE_COUNTER, recurseLevel);
        
        if(recurseLevel == 0)
          requestMap.remove(XMLMenuModel.SHARED_MODEL_INDICATOR_KEY);
        

        // Name of the managed bean that is the sub menu XMLMenuModel.
        String modelStr = expr.substring(expr.indexOf('{')+1,
                                         expr.indexOf('}'));

        // There are 2 ways that a Model can be invalid:
        // 1) Something such as a missing managed bean definition
        //    for the submenu model causes the creation of the
        //    XMLMenuModel for the submenu to fail. This will result
        //    in menuModel being NULL.
        // 2) Some kind of parsing error in its metadata.  If a node
        //    type is invalid, an exception will be thrown (see below)
        //    and caught in getTreeModel().  This will result in a
        //    null submenu list the following SAXException will also
        //    be logged.
        if (menuModel != null)
        {
          Object         subMenuObj  = menuModel.getWrappedData();
          List<MenuNode> subMenuList = null;

          if (subMenuObj instanceof ChildPropertyTreeModel)
          {
            subMenuList =
              (List<MenuNode>)((ChildPropertyTreeModel)subMenuObj).getWrappedData();
          }

          if (subMenuList != null)
          {
            // SharedNode could be the first child
            // So we need a new list for the children
            if (_menuNodes.size() < _nodeDepth)
            {
              _menuNodes.add(new ArrayList<MenuNode>());
            }

            List<MenuNode> list = _menuNodes.get(_nodeDepth-1);
            list.addAll(subMenuList);
          }
          else
          {
            // Let it go through but log it.  This way the rest of
            // the Tree gets built and this submenu is skipped.
            SAXException npe =
              new SAXException("Shared Node Model not created for " + modelStr);
          }
        }
        else
        {
          // Let it go through but log it.  This way the rest of
          // the Tree gets built and this submenu is skipped.
          NullPointerException npe =
            new NullPointerException("Shared Node Model not created for "
              + modelStr + ". Check for the existence of the corresponding "
              + "managed bean in your config files.");

          _LOG.severe (npe.getMessage(), npe);
        }
      }
      else
      {
        // Throw an Exception for any node that is not of type
        // menu, itemNode, groupNode, or sharedNode.  This will get
        // caught in getTreeModel()
        throw new SAXException("Invalid Node type: " + localElemName);
      }
    }
  }

  /**
   * Processing done at the end of parsing a node enty element from the
   * menu metadata file.  This manages the node depth properly so that
   * method startElement works correctly to build the List.
   *
   * @param nameSpaceUri - not used.
   * @param localElemName - not used.
   * @param qualifiedElemName - String designating the node type of the entry.
   */
  @Override
  public void endElement(String nameSpaceUri, String localElemName, String qualifiedElemName)
  {
    if (   _ITEM_NODE.equals(qualifiedElemName)
        || _GROUP_NODE.equals(qualifiedElemName)
       )
    {
      _nodeDepth--;

      if (_skipDepth >= 0)
      {
        if (_nodeDepth < _skipDepth)
        {
          _skipDepth = -1;
        }
      }
      else
      {
        if (_nodeDepth > 0)
        {
          // The parent menu item is the last menu item at the previous depth
          List<MenuNode> parentList = _menuNodes.get(_nodeDepth-1);
          MenuNode       parentNode = parentList.get(parentList.size()-1);

          parentNode.setChildren(_menuNodes.get(_nodeDepth));
        }

        // If we have dropped back two levels, then we are done adding
        // the parent's sub tree, remove the list of menu items at that level
        // so they don't get added twice.
        if (_nodeDepth == (_menuNodes.size() - 2))
        {
          _menuNodes.remove(_nodeDepth+1);
        }
      }
    }
    else if (_SHARED_NODE.equals(qualifiedElemName))
    {
      _nodeDepth--;

      // In processing a sharedNode in startElement(), it is possible
      // that a sharedNode model is not created properly. However,
      // we only log an error and let parsing continue so that the whole
      // Tree can get created w/o the failed sharedNode submenu model.
      // Thus we need the 2nd conditional here to detect if we are at
      // the end of parsing a failed sharedNode.
      if (_nodeDepth > 0  && _menuNodes.size() > _nodeDepth)
      {
        // The parent menu item is the last menu item at the previous depth
        List<MenuNode> parentList = _menuNodes.get(_nodeDepth-1);
        MenuNode       parentNode = parentList.get(parentList.size()-1);

        parentNode.setChildren(_menuNodes.get(_nodeDepth));
      }
    }
  }

  /**
   * Called by the SAX Parser at the end of parsing a document.
   * Here, the menuList is put on the menuList map.
   */
  @Override
  public void endDocument()
  {
    if (_menuNodes.isEmpty())
    {
      // Empty tree is created to prevent
      // later NPEs if menu model methods are called.
      _LOG.warning ("CREATE_TREE_WARNING: Empty Tree!");

      List<MenuNode> list = Collections.emptyList();
      _menuList = list;
    }
    else
    {
      _menuList = _menuNodes.get(0);

      // Create the treeModel
      ChildPropertyTreeModel treeModel =
                    new ChildPropertyTreeModel(_menuList, "children");


      if (_isRootHandler)
      {
        _viewIdFocusPathMap = new HashMap<String,List<Object>>();
        _nodeFocusPathMap   = new HashMap<Object, List<Object>>();
        _idNodeMap          = new HashMap<String, Object>();
        Object oldPath      = treeModel.getRowKey();

        treeModel.setRowKey(null);

        // Populate the maps
        _addToMaps(treeModel, _viewIdFocusPathMap, _nodeFocusPathMap, _idNodeMap);

        treeModel.setRowKey(oldPath);
      }
    }
  }

  /**
   * Get the Model's viewIdFocusPathMap
   *
   * @return the Model's viewIdFocusPathMap
   */
  public Map<String, List<Object>> getViewIdFocusPathMap(Object modelKey)
  {
    return _viewIdFocusPathMap;
  }

  /**
   * Get the Model's nodeFocusPathMap
   *
   * @return the Model's nodeFocusPathMap
   */
  public Map<Object, List<Object>> getNodeFocusPathMap(Object modelKey)
  {
    return _nodeFocusPathMap;
  }

  /**
   * Get the Model's idNodeMap
   *
   * @return the Model's idNodeMap
   */
  public Map<String, Object> getIdNodeMap(Object modelKey)
  {
    return _idNodeMap;
  }

  /**
    * Get the treeModel built during parsing
    *
    * @return List of menu nodes.
    */
  public TreeModel getTreeModel(String uri)
  {
     List<MenuNode> list = _menuList;
     
    // If we have a cached model, return it.
    if (list != null)
      return new ChildPropertyTreeModel(list,"children");

    synchronized(this)
    {
      list = _menuList;
      if (list == null)// double check inside lock
      {
        // Build a Tree model.  Parsing puts the tree model
        // in the map, see method endDocument().
        _currentTreeModelMapKey = uri;
        try
        {
          // Get a parser.  NOTE: we are using the jdk's 1.5 SAXParserFactory
          // and SAXParser here.
          SAXParser parser = _SAX_PARSER_FACTORY.newSAXParser();

          // Call the local menu model's getStream() method. This is a model
          // method so that it can be overridden by any model extending
          // XmlMenuModel.
          InputStream inStream = getModel().getStream(uri);

          // Parse the metadata
          parser.parse(inStream, this);

          inStream.close();
        } catch (SAXException saxex)
        {
          _LOG.severe("SAX Parse Exception parsing " + uri + ": " +
              saxex.getMessage(), saxex);
        } catch (IOException ioe)
        {
          _LOG.severe("Unable to open an InputStream to " + uri, ioe);
        } catch (IllegalArgumentException iae)
        {
          _LOG.severe("InputStream to " + iae + " is null", iae);
        } catch (ParserConfigurationException pce)
        {
          _LOG.severe("Unable to create SAX parser for " + uri, pce);
        }
        list = _menuList;
      }
    }
    return new ChildPropertyTreeModel(list,"children");
  }

  /**
   * Get the top-level, root menu model, which contains
   * the entire menu tree.
   *
   * @return root, top-level XMLMenuModel
   */
  @SuppressWarnings("unchecked")
  public XMLMenuModel getRootModel()
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    Map<String, Object> requestMap =
      facesContext.getExternalContext().getRequestMap();

    Map<String,XMLMenuModel> modelMap = (Map<String,XMLMenuModel>) requestMap.get(getRootModelKey());
    if(!requestMap.containsKey(XMLMenuModel.SHARED_MODEL_INDICATOR_KEY))
      return modelMap.get( (this.getId() ) );
    else
      return modelMap.get((Integer) requestMap.get(XMLMenuModel.SHARED_MODEL_INDICATOR_KEY));
  }

  /**
   * Get the top-level, root menu model's Request Map Key.
   *
   * @return root, top-level XMLMenuModel's Request Map Key.
   */
  public String getRootModelKey()
  {
    return _rootModelKey;
  }

  /**
   * Sets the root menu Model's Request map key.
   * <p>
   * This is always only the top-level, root model's Request map key.
   * We do this because the MenuContentHandlerImpl and nodes need to be able
   * to call into the root model to:
   * <ul>
   * <li>notify them root menu model of the currently selected node on a POST
   * <li>group node needs to find its referenced item node.
   * </ul>
   *
   * @param rootModelKey - String the root, top-level menu model's Request
   *        map key.
   */
  public void setRootModelKey(String rootModelKey)
  {
    _rootModelKey = rootModelKey;
  }

  /**
   * Get the local (sharedNode) menu model.
   *
   * @return sharedNode's XMLMenuModel
   */
  @SuppressWarnings("unchecked")
  public XMLMenuModel getModel()
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    Map<String, Object> requestMap =
      facesContext.getExternalContext().getRequestMap();

    return (XMLMenuModel) requestMap.get(getModelId());
  }

  /**
   * Get the local (sharedNode) menu model's Uri.
   *
   * @return sharedNode's XMLMenuModel Uri
   */
  public String getModelId()
  {
    return _localModelId;
  }

  /**
   * Sets the local (sharedNode's) menu Model's System Id.
   *
   * @param localModelId - String the root, top-level menu model's Uri.
   */
  public void setModelId(String localModelId)
  {
    _localModelId = localModelId;
  }

  /**
   * Sets the treeModel map key used to get a cached treeModel
   * from the MenuContentHandlerImpl.
   *
   * Note: this is set from the XMLMenuModel BEFORE parsing begins
   *
   * @param uri String path to the menu model's metadata
   */
  public void setTreeModelKey(String uri)
  {
    _currentTreeModelMapKey = uri;
  }

  
  public void setRootHandler(boolean isRoot)
  {
    _isRootHandler = isRoot;
  }
  
  /**
   * sets the id of this content handler
   * 
   * No synchronization necessary,let
   * the first thread set the id,
   * the rest of the threads will immediately
   * see the new value and will not try to 
   * set again.
   */
  public void setId(int id)
  {
    if(_id == -1)
      _id = id;
  }
  
  public int getId()
  {
    return _id;
  }
  //=======================================================================
  // Package Private Methods
  //=======================================================================
  /**
   * Gets the MenuContentHandlerImpl's id.
   *
   * This is set in the MenuContentHandlerImpl's Constructor
   * and is used to ensure that the all resource bundle keys
   * and node ids are unique.
   *
   * @return String handler id.
   */
  String getHandlerId()
  {
    return _handlerId;
  }

  /**
   * Returns the hashmap key for a resource bundle.
   *
   * This the value of the "var" attribute for the menu root node
   * from the menu's metadata
   *
   * @return String hashmap key.
   */
  String getBundleKey()
  {
    return _resBundleKey;
  }

  //=======================================================================
  // Private Methods
  //=======================================================================

 /**
   * Create a Map of name/value pairs from the attrList given
   * to us by the Sax parser.
   *
   * @param attrList List of attributes of an XML element
   * @return Map hashMap of attributes converted to name/value pairs.
   */
  @SuppressWarnings("unchecked")
  private Map<String, String> _getMapFromList(Attributes attrList)
  {
    Map<String, String> attrMap = new HashMap<String, String>();

    for (int i=0; i < attrList.getLength(); i++)
    {
      attrMap.put(attrList.getQName(i), attrList.getValue(i) );
    }

    return attrMap;
  }

 /**
   * Creates a MenuNode from attribute list.
   *
   * @return MenuNode used in the Menu List.
   */
  private MenuNode _createMenuNode ()
  {
    // Get generic attributes

    // If the node has rendered = false, do not create it.
    // This is a security risk and cannot be allowed
    String renderedStr = _getAndRemoveAttrValue(_RENDERED_ATTR);

    // We do not create nodes whose rendered attr is false
    // and if the Root model or the local model's (sharedNode
    // model) says that nodes whose rendered attribute is false
    // should not be created, then we don't either.
    //
    // This default value of false (don't create nodes whose
    // rendered attr is false) can be overridden by the
    // XMLMenuModel's managed property, createHiddenNodes.
    // Typically this is done in faces-config.xml
    //
    if (   "false".equals(renderedStr)
        && (   !getRootModel().getCreateHiddenNodes()
            || !getModel().getCreateHiddenNodes()
           )
       )
    {
      return null;
    }

    String label       = _getAndRemoveAttrValue(_LABEL_ATTR);
    String icon        = _getAndRemoveAttrValue(_ICON_ATTR);
    String disabledStr = _getAndRemoveAttrValue(_DISABLED_ATTR);
    String readOnlyStr = _getAndRemoveAttrValue(_READONLY_ATTR);
    String accessKey   = _getAndRemoveAttrValue(_ACCESSKEY_ATTR);
    String labelAndAccessKey = _getAndRemoveAttrValue(_LABEL_AND_ACCESSKEY_ATTR);
    String id          = _getAndRemoveAttrValue(_ID_ATTR);
    String visibleStr  = _getAndRemoveAttrValue(_VISIBLE_ATTR);

    MenuNode menuNode = (  _currentNodeStyle == MenuConstants.NODE_STYLE_ITEM
                         ? _createItemNode()
                         : _createGroupNode()
                        );

    // Set the generic attributes
    menuNode.setLabel(label);
    menuNode.setIcon(icon);
    menuNode.setDisabled(disabledStr);
    menuNode.setRendered(renderedStr);
    menuNode.setReadOnly(readOnlyStr);
    menuNode.setAccessKey(accessKey);
    menuNode.setId(id);
    menuNode.setVisible(visibleStr);

    if (labelAndAccessKey != null)
      menuNode.setLabelAndAccessKey(labelAndAccessKey);

    return menuNode;
  }

  /**
    * Creates an itemNode from attribute list obtained by parsing an
    * itemNode menu metadata entry.
    *
    * @return Node of type ItemNode.
    */
  private ItemNode _createItemNode()
  {
    // Create the itemNode
    ItemNode itemNode = new ItemNode();

    String action         = _getAndRemoveAttrValue(_ACTION_ATTR);
    String actionListener = _getAndRemoveAttrValue(_ACTIONLISTENER_ATTR);
    String launchListener = _getAndRemoveAttrValue(_LAUNCHLISTENER_ATTR);
    String returnListener = _getAndRemoveAttrValue(_RETURNLISTENER_ATTR);
    String immediate      = _getAndRemoveAttrValue(_IMMEDIATE_ATTR);
    String useWindow      = _getAndRemoveAttrValue(_USEWINDOW_ATTR);
    String windowHeight   = _getAndRemoveAttrValue(_WINDOWHEIGHT_ATTR);
    String windowWidth    = _getAndRemoveAttrValue(_WINDOWWIDTH_ATTR);
    String defaultFocusPathStr = _getAndRemoveAttrValue(_DEFAULT_FOCUS_PATH_ATTR);
    String focusViewId    = _getAndRemoveAttrValue(_FOCUS_VIEWID_ATTR);

    // Former Destination node attrs
    String destination = _getAndRemoveAttrValue(_DESTINATION_ATTR);
    String targetFrame = _getAndRemoveAttrValue(_TARGETFRAME_ATTR);

    // An item node with one of two(2) possible values:
    // 1) outcome
    // 2) EL method binding  (which can return either a URI or
    //    an outcome

    // Set its properties - null is ok.
    itemNode.setAction(action);
    itemNode.setActionListener(actionListener);
    itemNode.setLaunchListener(launchListener);
    itemNode.setReturnListener(returnListener);
    itemNode.setImmediate(immediate);
    itemNode.setUseWindow(useWindow);
    itemNode.setWindowHeight(windowHeight);
    itemNode.setWindowWidth(windowWidth);
    itemNode.setFocusViewId(focusViewId);
    itemNode.setDefaultFocusPath(defaultFocusPathStr);

    // Former destination node attrs
    itemNode.setDestination(destination);
    itemNode.setTargetFrame(targetFrame);

    // Set the Any Attributes Attrlist
    if (_attrMap.size() > 0)
    {
      itemNode.setCustomPropList(_attrMap);
    }

    return itemNode;
  }

  /**
    * Creates a GroupNode from attribute list passed obtained by parsing
    * a GroupNode menu metadata entry.
    *
    * @return Node of type GroupNode
    */
  private GroupNode _createGroupNode()
  {
    // Create the GroupNode
    GroupNode groupNode = new GroupNode();
    String idRef = _getAndRemoveAttrValue(_IDREF_ATTR);

    // Set its attributes - null is ok
    groupNode.setIdRef(idRef);

    return groupNode;
  }

  /**
   * Saves all information needed for parsing and building model data
   * before recursing into the new model of a sharedNode.
   *
   * Note: if you add a new push in this method, you must also add
   * a corresponding pop in _restoreModelData() below in the correct order.
   */
  @SuppressWarnings("unchecked")
  private void _saveModelData()
  {
    if (_saveDataStack == null)
    {
      _saveDataStack = new Stack<Object>();
    }

    // DO NOT CHANGE THE ORDER HERE.  IT MUST MATCH
    // "pops" DONE BELOW in _restoreModelData.
    int nodeDepthSave       = _nodeDepth;
    ArrayList<List<MenuNode>> menuNodesSave =
      new ArrayList<List<MenuNode>>(_menuNodes);


    ArrayList<Object> menuListSave  =
      (  _menuList != null
       ? new ArrayList<Object>(_menuList)
       : null
      );

    String mapTreeKeySave    = _currentTreeModelMapKey;
    String localModelIdSave = _localModelId;
    String handlerId         = _handlerId;
    String resBundleName     = _resBundleName;
    String resBundleKey      = _resBundleKey;
    _saveDataStack.push(nodeDepthSave);
    _saveDataStack.push(menuNodesSave);
    _saveDataStack.push(menuListSave);
    _saveDataStack.push(mapTreeKeySave);
    _saveDataStack.push(localModelIdSave);
    _saveDataStack.push(handlerId);
    _saveDataStack.push(resBundleName);
    _saveDataStack.push(resBundleKey);
  }

  /**
   * Restores data needed for parsing and building model data
   * as execution returns from creating a sharedNode child menu model.
   *
   * Note: if you add a new pop in this method, you must also add
   * a corresponding push in _saveModelData() above in the correct order.
   */
  @SuppressWarnings("unchecked")
  private void _restoreModelData()
  {
    // DO NOT CHANGE THE ORDER HERE.  IT MUST MATCH
    // "pushes" DONE ABOVE in _saveModelData.
    _resBundleKey           = (String) _saveDataStack.pop();
    _resBundleName          = (String) _saveDataStack.pop();
    _handlerId              = (String) _saveDataStack.pop();
    _localModelId          = (String) _saveDataStack.pop();
    _currentTreeModelMapKey = (String) _saveDataStack.pop();
    _menuList               = (ArrayList<MenuNode>) _saveDataStack.pop();
    _menuNodes              = (ArrayList<List<MenuNode>>) _saveDataStack.pop();
    _nodeDepth              = ((Integer)_saveDataStack.pop()).intValue();
  }

  /**
   * Gets the specified attribute's value from the Attributes List
   * passed in by the parser.  Also removes this attribute so that
   * once we are finished processing and removing all the known
   * attributes, those left are custom attributes.
   *
   * @param attrName
   * @return String value of the attribute in the Attributes List.
   */
  private String _getAndRemoveAttrValue(String attrName)
  {
    String attrValue = _attrMap.get(attrName);

    if (attrValue != null)
      _attrMap.remove(attrName);

    return attrValue;
  }

  /*=========================================================================
   * Menu Model Data Structure section.
   * ======================================================================*/
  /**
   * Traverses the tree and builds the model's viewIdFocusPathMap,
   * nodeFocusPathMap, and _idNodeMap
   *
   * @param tree
   */
  @SuppressWarnings("unchecked")
  private void _addToMaps(
    TreeModel tree,
    Map viewIdFocusPathMap,
    Map nodeFocusPathMap,
    Map idNodeMap)
  {
    for ( int i = 0; i < tree.getRowCount(); i++)
    {
      tree.setRowIndex(i);

      // Get the node
      MenuNode node = (MenuNode) tree.getRowData();

      // Get its focus path
      List<Object> focusPath = (List<Object>)tree.getRowKey();

      // Get the focusViewId of the node
      Object viewIdObject = node.getFocusViewId();

      if (viewIdObject != null)
      {
        // Put this entry in the nodeFocusPathMap
        nodeFocusPathMap.put(node, focusPath);

        // Does this viewId already exist in the _viewIdFocusPathMap?
        List<Object> existingFpArrayList =
          _viewIdFocusPathMap.get(viewIdObject);

        if (existingFpArrayList == null)
        {
          // This is a node with a unique focusViewId.  Simply create
          // and Arraylist and add the focusPath as the single entry
          // to the focus path ArrayList.  Then put the focusPath
          // ArrayList in the focusPath HashMap.
          List<Object> fpArrayList = new ArrayList<Object>();
          fpArrayList.add(focusPath);
          viewIdFocusPathMap.put(viewIdObject, fpArrayList);
        }
        else
        {
          // This is a node that points to the same viewId as at least one
          // other node.

          // If the node's defaultFocusPath is set to true, we move it to
          // the head of the ArrayList. The 0th element of the list is
          // always returned when navigation to a viewId occurs from outside
          // the menu model (that is _currentNode is null)
          boolean defFocusPath = node.getDefaultFocusPath();

          if (defFocusPath)
          {
            existingFpArrayList.add(0, focusPath);
          }
          else
          {
            existingFpArrayList.add(focusPath);
          }
        }
      }

      // Get the Id of the node
      String idProp = node.getUniqueId();

      if (idProp != null)
      {
        idNodeMap.put(idProp, node);
      }

      if (tree.isContainer() && !tree.isContainerEmpty())
      {
        tree.enterContainer();
        _addToMaps(tree, viewIdFocusPathMap, nodeFocusPathMap, idNodeMap);
        tree.exitContainer();
      }
    }
  }

  //========================================================================
  // Private variables
  //========================================================================

  private List<List<MenuNode>> _menuNodes;
  private volatile List<MenuNode>       _menuList;
  private String _currentTreeModelMapKey;
  private int    _nodeDepth;
  private int    _skipDepth = -1;
  private String _currentNodeStyle;
  private String _handlerId;
  private String _resBundleKey;
  private String _resBundleName;

  private Map<String, String>       _attrMap;
  private Stack<Object>             _saveDataStack;
  private Map<String, List<Object>> _viewIdFocusPathMap;
  private Map<Object, List<Object>> _nodeFocusPathMap;
  private Map<String, Object>       _idNodeMap;


  // Local (shared) Menu models Uri
  private String _localModelId = null;

  // Root Menu model's Session map key
  private String _rootModelKey  = null;
  
  private volatile boolean _isRootHandler;
  
  private volatile int _id  = -1;

  // Nodes
  private final static String _GROUP_NODE        = "groupNode";
  private final static String _ITEM_NODE         = "itemNode";
  private final static String _SHARED_NODE       = "sharedNode";
  private final static String _ROOT_NODE         = "menu";

  // Attributes
  private final static String _LABEL_ATTR        = "label";
  private final static String _RENDERED_ATTR     = "rendered";
  private final static String _ID_ATTR           = "id";
  private final static String _IDREF_ATTR        = "idref";
  private final static String _ICON_ATTR         = "icon";
  private final static String _DISABLED_ATTR     = "disabled";
  private final static String _DESTINATION_ATTR  = "destination";
  private final static String _ACTION_ATTR       = "action";
  private final static String _REF_ATTR          = "ref";
  private final static String _READONLY_ATTR     = "readOnly";
  private final static String _VAR_ATTR          = "var";
  private final static String _RES_BUNDLE_ATTR   = "resourceBundle";
  private final static String _FOCUS_VIEWID_ATTR = "focusViewId";
  private final static String _ACCESSKEY_ATTR    = "accessKey";
  private final static String _LABEL_AND_ACCESSKEY_ATTR = "labelAndAccessKey";
  private final static String _TARGETFRAME_ATTR  = "targetframe";
  private final static String _ACTIONLISTENER_ATTR = "actionListener";
  private final static String _LAUNCHLISTENER_ATTR = "launchListener";
  private final static String _RETURNLISTENER_ATTR = "returnListener";
  private final static String _IMMEDIATE_ATTR      = "immediate";
  private final static String _USEWINDOW_ATTR      = "useWindow";
  private final static String _WINDOWHEIGHT_ATTR   = "windowHeight";
  private final static String _WINDOWWIDTH_ATTR    = "windowWidth";
  private final static String _DEFAULT_FOCUS_PATH_ATTR  = "defaultFocusPath";
  private final static String _VISIBLE_ATTR        = "visible";

  private static final SAXParserFactory _SAX_PARSER_FACTORY;
  static
  {
      _SAX_PARSER_FACTORY = SAXParserFactory.newInstance();
  }
  
  private final static TrinidadLogger _LOG =
                        TrinidadLogger.createTrinidadLogger(MenuContentHandlerImpl.class);
  
  private static final String _RECURSE_COUNTER =
      "org.apache.myfaces.trinidadinternal.menu.MenuContentHandlerImpl._RECURSE_COUNTER";
  /**
   * 
   */
  private static final long serialVersionUID = -5330432089846748485L;

} // endclass MenuContentHandlerImpl
