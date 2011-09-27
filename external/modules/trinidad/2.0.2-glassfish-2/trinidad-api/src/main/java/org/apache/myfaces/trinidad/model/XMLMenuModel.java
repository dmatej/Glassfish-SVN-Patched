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
package org.apache.myfaces.trinidad.model;

import java.io.InputStream;
import java.io.Serializable;

import java.net.URL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import javax.el.ELContext;
import javax.el.ELResolver;
import javax.el.PropertyNotFoundException;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.util.ContainerUtils;
import org.apache.myfaces.trinidad.util.TransientHolder;


/**
 * Creates a Menu Model from a TreeModel where nodes in the treeModel
 * contain viewId information.
 * <p>
 * Each node must have either a bean getter method or a Map property
 * that returns a viewId. There are several restrictions on the data:
 * <ul>
 * o The nodes in the tree must either be all beans or all maps,
 * but not a mix of beans and maps.
 * o The viewId of a node can be null, but if set it must be unique.
 * o The tree cannot be mutable.
 * </ul>
 * <p>
 * The getFocusRowKey method
 * <ul>
 * o gets the current viewId by calling
 * FacesContext.getCurrentInstance().getViewRoot().getViewId()
 * o compares the current viewId with the viewId's in the viewIdFocusPathMap
 * that was built by traversing the tree when the model was created.
 * o returns the focus path to the node with the current viewId or null if the
 * current viewId can't be found.
 * o in the case where a viewId has multiple focus paths, the currently
 * selected node is used as a key into the nodeFocusPathMap to return the
 * correct focus path.
 * </ul>
 * <p>
 * The Model is created by specifying it in the faces-config.xml file
 * as follows
 * <pre>
 *   &lt;managed-bean&gt;
 *    &lt;managed-bean-name&gt;hr_menu&lt;/managed-bean-name&gt;
 *    &lt;managed-bean-class&gt;
 *      org.apache.myfaces.trinidad.model.XMLMenuModel
 *    &lt;/managed-bean-class&gt;
 *    &lt;managed-bean-scope&gt;request&lt;/managed-bean-scope&gt;
 *    &lt;managed-property&gt;
 *      &lt;property-name&gt;source&lt;/property-name&gt;
 *      &lt;property-class&gt;java.lang.String&lt;/property-class&gt;
 *      &lt;value&gt;/WEB-INF/hr-menu.xml&lt;/value&gt;
 *    &lt;/managed-property&gt;
 *  &lt;/managed-bean&gt;
 * </pre>
 *
 * Objects of this class are not thread safe and should be used
 * only in request scope.
 *
 */

/*
 * Three hashmaps are also created in order to be able to resolve cases where
 * multiple menu items cause navigation to the same viewId.  All 3 of these maps
 * are created after the metadata is parsed and the tree is built, in the
 * MenuContentHandlerImpl.
 *
 * o The first hashMap is called the viewIdFocusPathMap and is built by
 * traversing the tree when the model is created.  Each node's focusViewId is
 * obtained and used as the key to an entry in the viewIdHashMap.  An ArrayList
 * is used as the entry's value and each item in the ArrayList is a node's
 * rowkey from the tree. This allows us to have duplicate rowkeys for a single
 * focusViewId which translates to a menu that contains multiple items pointing
 * to the same page. In general, each entry will have an ArrayList of rowkeys
 * with only 1 rowkey, AKA focus path.
 * o The second hashMap is called the nodeFocusPathMap and is built at the
 * same time the viewIdHashMap is built. Each entry's key is the actual node and
 * the value is the row key.  Since the model keeps track of the currently
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
 *
 * Keeping track of the currently selected menu item/node.
 *
 * If an itemNode in the metadata uses its "action" attribute, a POST is done
 * and the node's "doAction" method is called when the menu item is clicked. At
 * that time, the model is notified through its setCurrentlyPostedNode() method,
 * where the current node is set and the request method is set to POST.
 *
 * If an itemNode in the metadata uses its "destination" attribute, a GET is
 * done.  Nothing is called on the model when the menu item is clicked.  However
 * at the time the page is rendered the "getDestination" method for all nodes
 * using the "destination" attribute is called.  At this point
 * we append the node's id to the value of the destination attribute URL, as
 * a parameter, and return it. So when getFocusRowKey() is called, we get the
 * request the node's parameter matching the currently selected node's id.
 * Using the node id, we find the matching node in the idNodeMap and voila, we
 * have the currently selected node!
 */
public class XMLMenuModel extends BaseMenuModel
                          
{
  public XMLMenuModel()
  {
    super();
    _modelId = Integer.valueOf(System.identityHashCode(this)).toString();
  }
  
  /**
   * This needs to be overriden by classes extending XmlMenuModel and using APIs for the nodes
   * of XmlMenuModel. Default value returned is true for backward compatibilty. The models using
   * the external APIs for their nodes must return false.
   * @return boolean
   */
  protected boolean isCompatibilityMode()
  {
    return true;
  }

  /**
   * setSource - specifies the XML metadata and creates
   * the XML Menu Model.
   *
   * @param menuMetadataUri - String URI to the XML metadata.
   */
  public void setSource(String menuMetadataUri)
  {
    if (menuMetadataUri == null || "".equals(menuMetadataUri))
      return;

    _mdSource = menuMetadataUri;
    _createModel();
  }

  /**
   * Makes the TreeModel part of the menu model.  Also creates the
   * _viewIdFocusPathMap, _nodeFocusPathMap, and idNodeMaps.
   *
   * @param data The Tree Model instance
   */
  @Override
  public void setWrappedData(Object data)
  {
    super.setWrappedData(data);

    // The only thing the child menu models are needed for are their
    // menuLists, which get incorporated into the Root Model's tree.
    // There is no need to create the hashmaps or anything
    // on the child menu models.  A lot of overhead (performance and
    // memory) would be wasted.
    if (_isRoot)
    {
      _viewIdFocusPathMap = _contentHandler.getViewIdFocusPathMap(_mdSource);
      _nodeFocusPathMap   = _contentHandler.getNodeFocusPathMap(_mdSource);
      _idNodeMap          = _contentHandler.getIdNodeMap(_mdSource);
    }
  }

  /**
   * Returns the rowKey to the current viewId, or in the case of where the
   * model has nodes with duplicate viewId's and one is encountered, we
   * return the rowKey of the currently selected node.
   * <p>
   *
   * The getFocusRowKey method
   * <ul>
   * <li>gets the current viewId by calling
   * FacesContext.getCurrentInstance().getViewRoot().getViewId()
   * <li>compares the current viewId with the viewId's in the viewIdFocusPathMap
   * that was built by traversing the tree when the model was created.
   * <li>returns the focus path to the node with the current viewId or null if
   * the current viewId can't be found.
   * <li>in the case where a viewId has multiple focus paths, the currently
   * selected node is used as a key into the nodeFocusPathMap to return the
   * correct focus path.
   * </ul>
   *
   * @return  the rowKey to the node with the current viewId or null if the
   * current viewId can't be found.
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getFocusRowKey()
  {
    Object focusPath        = null;
    String currentViewId    = _getCurrentViewId();
    FacesContext context    = FacesContext.getCurrentInstance();

    // getFocusRowKey() is called multiple times during the Process Validations
    // Phase and again during the Render Response Phase during each Request.
    // During each phase, as described below, the same viewId is passed in. To
    // prevent unnecessary looking up of the focus path each time, the previous
    // focus path is returned after the first call in each phase, as described
    // below.
    //
    // ** Process Validations Phase:
    // During the Process Validations Phase, the prevViewId is initially
    // null (gets set to this at the start of each Request). The first time
    // getFocusRowKey is called, the currentViewId is that of the node we are
    // navigating "from" during the Request.  This is stored in prevViewId.
    // Because the currentViewId is not equal to the prevViewId,
    // the node is looked up, its focus path stored in prevFocusPath, and the
    // focus path is returned. On subsequent calls during the Process
    // Validations Phase, the currentViewId is always that of the "from" node,
    // the currentViewId is equal to the prevViewId, and so we simply
    // return prevFocusPath.
    //
    // ** Render Response Phase:
    // During the Render Response Phase, the prevViewId is initially
    // that of the "from" node. The first time getFocusRowKey is called
    // the currentViewId is that of the node we are navigating "to" during
    // the Request.  This is stored in prevViewId.
    // Because the currentViewId is not equal to the prevViewId,
    // the node is looked up, its focus path stored in prevFocusPath, and the
    // focus path is returned. On subsequent calls during the Render
    // Response Phase, the currentViewId is always that of the "to" node,
    // the currentViewId is equal to the prevViewId, and so we simply
    // return prevFocusPath.
    //
    // IMPORTANT: Code that returns the correct focus path for duplicate nodes
    // in the node tree actually depends on this optimization.
    //
    if ((_prevViewId != null) && _prevViewId.equals(currentViewId))
      return _prevFocusPath;

    // Initializations
    _prevViewId    = currentViewId;

    // How did we get to this page?
    // 1) Clicked on a menu item with its action attribute set.  This does
    //    a POST.
    // 2) Clicked on a menu item with its destination attribute set.  This
    //    does a GET.
    // 3) Navigation to a viewId within our model but done from outside the
    //    model.  Examples, button, text link, etc.
    //

    // Case 1: POST method.  Current Node has already been set and so has the
    // request method.  The doAction() method of the clicked node calls
    // the setCurrentlyPostedNode() method of this model, which sets both. So
    // we have nothing to do in this case.

    if (_getRequestMethod() != _METHOD_POST)
    {
      // Case 2: GET method.  We have hung the selected node's id off the
      // request's URL, which enables us to get the selected node and also
      // to know that the request method is GET.
      Map<String, String> paramMap =
        context.getExternalContext().getRequestParameterMap();
      String UrlNodeId = paramMap.get(_NODE_ID_PROPERTY);

      if (UrlNodeId != null)
      {
        _setCurrentlySelectedNode(_getNodeFromURLParams(UrlNodeId));
        _setRequestMethod(_METHOD_GET);
      }
    }

    // Case 3: Navigation to a page within the model from an outside
    // method, e.g. button, link text, etc.  In this case we set the
    // currently selected node to null.  This tells us to get the 0th
    // element of the ArrayList returned from the viewId hashMap.  This
    // should be a focus path match to the node whose "defaultFocusPath"
    // attribute was set to 'true'.
    if (_getRequestMethod() == _METHOD_NONE)
    {
      _setCurrentlySelectedNode(null);
    }

    // Get the matching focus path ArrayList for the currentViewId.
    // This is an ArrayList because our map allows nodes with the same
    // viewId, that is, different focus paths to the same viewId.
    ArrayList<Object> fpArrayList =
     (ArrayList<Object>) _viewIdFocusPathMap.get(currentViewId);

    if (fpArrayList != null)
    {
      if (_prevRequestNode != null)
      {
        // _prevRequestNode will only be non-null when the previous
        // request's node (AKA the "from" node) was a duplicate.
        // We need to return the correct focus path, so we must
        // use the node.  If we don't do this, the wrong focus path
        // could be returned from the _viewIdFocusPathMap.
        focusPath = _nodeFocusPathMap.get(_prevRequestNode);

        // Reset it to null
        _prevRequestNode = null;
      }
      else
      {
        // Get the currently selected node
        Object currentNode = _getCurrentlySelectedNode();

        if (fpArrayList.size() == 1 || currentNode == null)
        {
          // For fpArrayLists with multiple focusPaths,
          // the 0th entry in the fpArrayList carries the
          // focusPath of the node with its defaultFocusPath
          // attribute set to "true", if there is one.  If
          // not, the 0th element is the default.
          focusPath = fpArrayList.get(0);

          // Not a duplicate node so set this to null
          _prevRequestNode = null;
        }
        else
        {
          // This will be a duplicate node, meaning it navigates to the
          // the same page as at least one other node in the tree.
          focusPath = _nodeFocusPathMap.get(currentNode);

          // Save this node for the next request.  Otherwise, the
          // next Request will go into previous part of this conditional
          // and return (possibly) the wrong focus path during the
          // Process Validations Phase.
          _prevRequestNode = currentNode;
        }
      }
    }

    // Save all pertinent information
    _prevFocusPath = focusPath;

    // Reset this to _METHOD_NONE so we will know when
    // Navigation to a viewId within our model has been
    // done from outside the model, e.g. link, button.
    // If this is not done, the current request method
    // will be from the previous navigation and could
    // be incorrrect.  We always reset it to _METHOD_NONE
    // so that the correct navigation method (see comment at top
    // of getFocusRowKey() ) is determined each time.
    _setRequestMethod(_METHOD_NONE);

    return focusPath;
  }


  /**
   * Gets the URI to the XML menu metadata.
   *
   * @return String URI to the XML menu metadata.
   */
  public String getSource()
  {
    return _mdSource;
  }

  /**
   * Sets the boolean value that determines whether or not to create
   * nodes whose rendered attribute value is false.  The default
   * value is false.
   *
   * This is set through a managed property of the XMLMenuModel
   * managed bean -- typically in the faces-config.xml file for
   * a faces application.
   */
  public void setCreateHiddenNodes(boolean createHiddenNodes)
  {
    _createHiddenNodes = createHiddenNodes;
  }

  /**
   * Gets the boolean value that determines whether or not to create
   * nodes whose rendered attribute value is false.  The default
   * value is false.
   *
   * This is called by the contentHandler when parsing the XML metadata
   * for each node.
   *
   * @return the boolean value that determines whether or not to create
   * nodes whose rendered attribute value is false.
   */
  public boolean getCreateHiddenNodes()
  {
    return _createHiddenNodes;
  }


  /**
   * Maps the focusPath returned when the viewId is newViewId
   * to the focusPath returned when the viewId is aliasedViewId.
   * This allows view id's not in the treeModel to be mapped
   * to a focusPath.
   *
   * @param newViewId the view id to add a focus path for.
   * @param aliasedViewId the view id to use to get the focusPath to use
   *        for newViewId.
   */
  @SuppressWarnings("unchecked")
  public void addViewId(String newViewId, String aliasedViewId)
  {
    List<Object> focusPath =
      _viewIdFocusPathMap.get(aliasedViewId);

    if (focusPath != null)
    {
      _viewIdFocusPathMap.put(newViewId, focusPath);
    }
  }

  /**
   * Sets the currently selected node and the request method.
   * This is called by a selected node's doAction method.  This
   * menu node must have had its "action" attribute set, thus the
   * method is POST.
   *
   * @param currentNode  The currently selected node in the menu
   */
  public void setCurrentlyPostedNode(Object currentNode)
  {
    _setCurrentlySelectedNode(currentNode);
    _setRequestMethod(_METHOD_POST);


    // Do this in the case where a menu item is selected
    // that has the same viewId as the previous menu item
    // that is selected.  If not, the test at the beginning
    // of getFocusRowKey() (currentViewId == _prevViewId)
    // is true and just returns, even though we have selected
    // a new node and the focus path should change.
    _prevViewId = null;
  }

  /**
   * Get a the MenuNode corresponding to the key "id" from the
   * node id hashmap.
   *
   * @param id - String node id key for the hashmap entry.
   * @return The Node Object that corresponds to id.
   */
  public Object getNode (String id)
  {
    XMLMenuModel rootModel = _getRootModel();
    Map<String, Object> idNodeMap = rootModel._getIdNodeMap();

    if (idNodeMap == null)
      return null;

    // This needs to be public because the nodes call into this map
    return idNodeMap.get(id);
  }

  /**
   * Gets the list of custom properties from the node
   * and returns the value of propName.  Node must be an itemNode.
   * If it is not an itemNode, the node will not have any custom
   * properties and null will be returned.
   *
   * @param node Object used to get its list of custom properties
   * @param propName String name of the property whose value is desired
   *
   * @return Object value of propName for Object node.
   */
  @SuppressWarnings("unchecked")
  public Object getCustomProperty(Object node, String propName)
  {
    if (node == null)
      return null;

    FacesContext context = FacesContext.getCurrentInstance();
    ELContext elContext  = context.getELContext();
    ELResolver resolver  = elContext.getELResolver();
    String value         = null;

    try
    {
      Map<String, String> propMap =
        (Map<String, String>) resolver.getValue(elContext,
                                                node, _CUSTOM_ATTR_LIST);

      // Need to check to see if propMap is null.  If there are
      // no custom properties for this itemNode, there will be
      // no propMap.  See MenuContentHandler._createItemNode().
      if (propMap == null)
        return null;

      value = propMap.get(propName);
    }
    catch (PropertyNotFoundException ex)
    {
      // if the node is not an itemNode, the node
      // has no custom properties, so we simply
      // return null
      return null;
    }

    // If it is an EL expression, we must evaluate it
    // and return its value
    if (   value != null
        && ContainerUtils.isValueReference(value)
       )
     {
       Object elValue = null;

       try
       {
         elValue = context.getApplication().evaluateExpressionGet(context,
                                                                  value,
                                                                  Object.class);
       }
       catch (Exception ex)
       {
         _LOG.warning("INVALID_EL_EXPRESSION", value);
         _LOG.warning(ex);
         return null;
       }
       return elValue;
     }

    return value;
  }

  /**
   * getStream - Opens an InputStream to the provided URI.
   *
   * @param uri - String uri to a data source.
   * @return InputStream to the data source.
   */
  public InputStream getStream(String uri)
  {
    // This is public so that extended menu models can override
    // and provide their own InputStream metadata source.
    // And it is called by the MenuContentHandlerImpl.
    try
    {
      // Open the metadata
      FacesContext context = FacesContext.getCurrentInstance();
      URL url = context.getExternalContext().getResource(uri);
      return url.openStream();
    }
    catch (Exception ex)
    {
      _LOG.severe("OPEN_URI_EXCEPTION", uri);
      _LOG.severe(ex);
      return null;
    }
  }

  /**
   * Get the Model's viewIdFocusPathMap
   *
   * @return the Model's viewIdFocusPathMap
   */
  public Map<String, List<Object>> getViewIdFocusPathMap()
  {
    if (!_isRoot || _contentHandler == null)
      return null;

    if (_viewIdFocusPathMap == null)
      _viewIdFocusPathMap = _contentHandler.getViewIdFocusPathMap(_mdSource);

    return _viewIdFocusPathMap;
  }

  /**
 * Returns the map of content handlers
 * which hold the state of one XML tree.
 * @param scopeMap
 * @return
 */
  protected Map<Object, List<MenuContentHandler> > getContentHandlerMap()
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    ExternalContext externalContext = facesContext.getExternalContext();
    Map<String, Object> scopeMap =
        externalContext.getApplicationMap();
   Object lock  = externalContext.getContext();
   
   // cannot use double checked lock here as
   // we cannot mark the reference as volatile
   // therefore any reads should happen inside
   // a synchronized block.
   synchronized (lock)
   {
     TransientHolder<Map<Object, List<MenuContentHandler> >> holder =  
       (TransientHolder<Map<Object, List<MenuContentHandler> >>) scopeMap.get(_CACHED_MODELS_KEY);
      Map<Object, List<MenuContentHandler>> contentHandlerMap = (holder != null) ? holder.getValue() : null;
      if (contentHandlerMap == null)
      {
        contentHandlerMap =
            new ConcurrentHashMap<Object, List<MenuContentHandler>>();
        scopeMap.put(_CACHED_MODELS_KEY, TransientHolder.newTransientHolder( contentHandlerMap) );
        scopeMap.put(_CACHED_MODELS_ID_CNTR_KEY,new AtomicInteger(-1));
      }
      return contentHandlerMap;
    }
    
  }
  
  protected int getContentHandlerId()
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    ExternalContext externalContext = facesContext.getExternalContext();
    Map<String, Object> scopeMap =
        externalContext.getApplicationMap();
    AtomicInteger counter = (AtomicInteger) scopeMap.get(_CACHED_MODELS_ID_CNTR_KEY);
    return counter.getAndIncrement();
  }
  protected Object getCacheKey()
  {
    return _mdSource;
  }
  
  /* ====================================================================
   * Private Methods
   * ==================================================================== */

  private  Map<String, Object> _getIdNodeMap()
  {
    return (_isRoot) ? _idNodeMap : null;
  }

  /**
   * Get a the MenuNode corresponding to the key "id" from the
   * node id hashmap.
   *
   * @param id - String node id key for the hashmap entry.
   * @return The MenuNode that corresponds to id.
   */
  private Object _getNodeFromURLParams (String urlNodeId)
  {
    // This needs to be public because the nodes call into this map
    return _idNodeMap.get(urlNodeId);
  }

  /**
    * Creates a menu model based on the menu metadata Uri.
    * This is accomplished by:
    * <ol>
    * <li> Get the MenuContentHandlerImpl through the Services API.
    * <li> Set the root model and current model on the content handler, which,
    * in turn, sets the models on each of the nodes.
    * <li> Parse the metadata.  This calls into the MenuContentHandler's
    * startElement and endElement methods, where a List of nodes and a TreeModel
    * are created, along with the 3 hashMaps needed by the Model.</li>
    * <li> Use the TreeModel to create the XMLMenuModel.</li>
    * </ol>
    */
  private void _createModel()
  {
    try
    {
      // this block of code handles the injection of the 
      // correct content handler for this xml menu model.
      _isRoot = _isThisRootModel();
      
      boolean newHandlerCreated = false;
      List<MenuContentHandler> listOfHandlers = getContentHandlerMap().get(getCacheKey());
      Map<Integer,XMLMenuModel> requestModelMap = _getRootModelMap();
      if(listOfHandlers != null)
      {
        if(requestModelMap == null)
          _contentHandler = listOfHandlers.get(0);
        else
        {
          
          for (MenuContentHandler handler : listOfHandlers)
          {
            int id = handler.getId();
            if (!requestModelMap.containsKey(id))
            {
              _contentHandler = handler;
              break;
            }
          }
        }
      }
      
      if(_contentHandler == null)
      {
        List<MenuContentHandler> services =
          ClassLoaderUtils.getServices(_MENUCONTENTHANDLER_SERVICE);

        if (services.isEmpty())
        {
          throw new IllegalStateException(_LOG.getMessage(
            "NO_MENUCONTENTHANDLER_REGISTERED"));
        }
        
        if(isCompatibilityMode())
        {
          _contentHandler = services.get(0);
        }
        else
        {
          _contentHandler = services.get(1);
        }
        
        if (_contentHandler == null)
        {
          throw new NullPointerException();
        }
        newHandlerCreated = true;
      }
      
      if(_isRoot)
      {
        if(listOfHandlers == null)
        {
          listOfHandlers =  Collections.synchronizedList(new ArrayList<MenuContentHandler>());
          getContentHandlerMap().put(getCacheKey(), listOfHandlers);
        }
        if(newHandlerCreated)
        {
          listOfHandlers.add(_contentHandler);
          _contentHandler.setRootHandler(true);
          _contentHandler.setId(getContentHandlerId());
        }
        
      }

      // Set the root, top-level menu model's URI on the contentHandler.
      // In this model, the menu content handler and nodes need to have
      // access to the model's data structures and to notify the model
      // of the currently selected node (in the case of a POST).
      _populateRootModelMap();
      _setRootModelKey(_contentHandler);

      // Set the local model (model created by a sharedNode) on the
      // contentHandler so that nodes can get back to their local model
      // if necessary.
      
      // Nodes never get back to their local models,which is good
      // because if they did they would not have found them as the 
      // hash code of newly created XMLMenuModels would be different
      // and hence the modelIds of the menu nodes would be stale
      // as they are longer lived than the menu models
      
      // On the other hand the content handler does refer to it's
      // local model during parsing at which time it is guaranteed
      // to be referring to just one model due to synchronization
      // of the parsing activity.
      
      // Therefore we only set the model id if this is a newly
      // created content handler,as we know it will start parsing
      // shortly.This is also necessary so that the model id of
      // the content handler does not change during the time it
      // is initializing it's state by parsing.Thus we can do
      // without synchronization here as the protection is needed only during
      // parsing.Any other request thread for the same meta-data URI will
      // find the content handler already published on the concurrent hash map
      // of content handlers.
     
      if(newHandlerCreated)
        _setModelId(_contentHandler);

      TreeModel treeModel = _contentHandler.getTreeModel(_mdSource);
      setWrappedData(treeModel);
    }
    catch (Exception ex)
    {
      _LOG.severe("ERR_CREATE_MENU_MODEL", _mdSource);
      _LOG.severe(ex);
      return;
    }
  }

  
  /**
   * _setRootModelKey - sets the top-level, menu model's Key on the
   * menu content handler. This is so nodes will only operate
   * on the top-level, root model.
   *
   */
  @SuppressWarnings("unchecked")
  private void _setRootModelKey(MenuContentHandler contentHandler)
  {
    contentHandler.setRootModelKey(_ROOT_MODEL_KEY);
  }
  

  /*
   * sets the model into the requestMap
   */
  private void _populateRootModelMap()
  {
    if (_isRoot)
    {
      Map<String, Object> requestMap = _getRequestMap();
      Map<Integer, XMLMenuModel> modelMap =
          (Map<Integer, XMLMenuModel>) requestMap.get(_ROOT_MODEL_KEY);
      if(modelMap == null)
      {
        modelMap =  new HashMap<Integer,XMLMenuModel>();
        requestMap.put(_ROOT_MODEL_KEY, modelMap);
      }
      modelMap.put(_contentHandler.getId(), this);
    }
  }
  
  private boolean _isThisRootModel()
  {
   Map<String, Object> requestMap = _getRequestMap();
   return !requestMap.containsKey(SHARED_MODEL_INDICATOR_KEY);
  }
  
  private Map<String, Object> _getRequestMap()
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    Map<String, Object> requestMap =
      facesContext.getExternalContext().getRequestMap();
    return requestMap;
  }
  

  /**
   * Returns the root menu model.
   *
   * @return XMLMenuModel the root menu model.
   */
  @SuppressWarnings("unchecked")
  private XMLMenuModel _getRootModel()
  {
    Map<Integer, XMLMenuModel> map = _getRootModelMap();
    return map.get(_contentHandler.getId());
  }
  
  private Map<Integer,XMLMenuModel> _getRootModelMap()
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    Map<String, Object> requestMap =
      facesContext.getExternalContext().getRequestMap();
    Map<Integer,XMLMenuModel> map = (Map<Integer,XMLMenuModel>) requestMap.get(_ROOT_MODEL_KEY);
    return map;
  }


  /**
   * _getModelId - gets the local, menu model's Sys Id.
   *
   * @return String the model's System Id.
   */
  private String _getModelId()
  {
    return _modelId;
  }

  /**
   * _setModelId - sets the local, menu model's id on the
   * menu content handler.
   */
  @SuppressWarnings("unchecked")
  private void _setModelId(MenuContentHandler contentHandler)
  {
    String modelId = _getModelId();

    // Put the local model on the Request Map so that it
    // Can be picked up by the nodes to call back into the
    // local model
    FacesContext facesContext = FacesContext.getCurrentInstance();
    Map<String, Object> requestMap =
      facesContext.getExternalContext().getRequestMap();

    requestMap.put(modelId, this);

    // Set the key (modelId) to the local model on the content
    // handler so that it can then be set on each of the nodes
    contentHandler.setModelId(modelId);
  }

  /**
   * Returns the current viewId.
   *
   * @return  the current viewId or null if the current viewId can't be found
   */

  private String _getCurrentViewId()
  {
    String currentViewId =
        FacesContext.getCurrentInstance().getViewRoot().getViewId();

    return currentViewId;
  }

  /**
   * Gets the currently selected node in the menu
   */
  private Object _getCurrentlySelectedNode()
  {
    return _currentNode;
  }

  /**
   * Sets the currently selected node.
   *
   * @param currentNode.  The currently selected node in the menu.
   */
  private void _setCurrentlySelectedNode(Object currentNode)
  {
    _currentNode = currentNode;
  }

  /**
   * Sets the request method
   *
   * @param method
   */
  private void _setRequestMethod(String method)
  {
    _requestMethod = method;
  }

  /**
   * Get the request method
   */
  private String _getRequestMethod()
  {
    return _requestMethod;
  }

  /* ================================================================
   * Public inner interface for the menu content handler
   * implementation
   * ================================================================ */

  /*
   * Interface corresponding to the MenuContentHandlerImpl
   * in org.apache.myfaces.trinidadinternal.menu.   This is used to achieve
   * separation between the api (trinidad) and the implementation
   * (trinidadinternal). It is only used by the XMLMenuModel, thus it is
   * an internal interface.
   */
  public interface MenuContentHandler
  {
    /**
      * Get the TreeModel built while parsing metadata.
      *
      * @param uri String mapkey to a (possibly) treeModel cached on
      *        the MenuContentHandlerImpl.
      * @return TreeModel.
      */
    public TreeModel getTreeModel(String uri);

    /**
      * Sets the root model's request map key on the ContentHandler so
      * that the nodes can get back to their root model
      * through the request map.
      */
    public void setRootModelKey(String key);

    /**
      * Sets the local, sharedNode model's Model id on the ContentHandler so that
      * the local model can be gotten to, if necessary.
      */
    public void setModelId(String modelId);

    /**
     * Get the Model's idNodeMap
     *
     * @return the Model's idNodeMap
     */
    public Map<String, Object> getIdNodeMap(Object modelKey);

    /**
     * Get the Model's nodeFocusPathMap
     *
     * @return the Model's nodeFocusPathMap
     */
    public Map<Object, List<Object>> getNodeFocusPathMap(Object modelKey);

    /**
     * Get the Model's viewIdFocusPathMap
     *
     * @return the Model's viewIdFocusPathMap
     */
    public Map<String, List<Object>> getViewIdFocusPathMap(Object modelKey);
    
    public void setRootHandler(boolean isRoot);
    
    /**
     * sets the id of this content handler
     */
    public void setId(int id);
    
    /**
     * gets the id of this content handler
     * @return the id
     */
    public int getId();
    
  }

  private Object  _currentNode       = null;
  private Object  _prevFocusPath     = null;
  private String  _prevViewId        = null;
  private String  _requestMethod     = _METHOD_NONE;
  private String  _mdSource          = null;
  private boolean _createHiddenNodes = false;
  private String  _modelId           = null;

  private Map<String, List<Object>> _viewIdFocusPathMap;
  private Map<Object, List<Object>> _nodeFocusPathMap;
  private Map<String, Object>       _idNodeMap;

  private MenuContentHandler _contentHandler  = null;
  private boolean _isRoot;
  

  // Only set this if _currentNode is a duplicate
   private Object             _prevRequestNode = null;

   // this key is used to store the map of models in the request.
  static private final String _ROOT_MODEL_KEY =
    "org.apache.myfaces.trinidad.model.XMLMenuModel.__root_menu__";
  
  // this key is used to store the map of content handlers in the scope map
  static private final String _CACHED_MODELS_KEY =
    "org.apache.myfaces.trinidad.model.XMLMenuModel.__handler_key__";
  
  // this supplies the id of the content handlers
  static private final String _CACHED_MODELS_ID_CNTR_KEY =
    "org.apache.myfaces.trinidad.model.XMLMenuModel.__id_cntr_key__";
  
/**
 * This key is used to store information about the included xml menu
 * models which are constructed during parsing.
 */
  static public final String SHARED_MODEL_INDICATOR_KEY =
    "org.apache.myfaces.trinidad.model.XMLMenuModel.__indicator_key__";

  static private final String _NODE_ID_PROPERTY     = "nodeId";
  static private final String _METHOD_GET           = "get";
  static private final String _METHOD_POST          = "post";
  static private final String _METHOD_NONE          = "none";
  static private final String _CUSTOM_ATTR_LIST     = "customPropList";
  static private final String _MENUCONTENTHANDLER_SERVICE =
            "org.apache.myfaces.trinidad.model.XMLMenuModel$MenuContentHandler";

  static private final TrinidadLogger _LOG =
         TrinidadLogger.createTrinidadLogger(XMLMenuModel.class);
  
}
