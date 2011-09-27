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
package org.apache.myfaces.trinidad.util;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

/**
 * A utility to store a reference to an <code>UIComponent</code>. Application developers
 * should use this tool if they need to have a reference to an instance of the
 * <code>UIComponent</code> class in <code>managed beans</code> that are longer than <b>requested scoped</b>
 * --for example Session and Application Scoped.  The reference will return the UIComponent, if any, with
 * the same scoped id as the Component used to create the reference, in the current UIViewRoot.
 * 
 * Use <code>newUIComponentReference()</code> to create a <code>ComponentReference</code> and
 * use the <code>getComponent()</code> to look up the referenced <code>UIComponent</code>.
 *  
 * For example, a current weather application might have have a session scoped weatehrBean
 * containing the current list of locations to report the weather on and support using a
 * selectMany component to remove the locations:
 * 
 * <pre>
 * <tr:selectManyCheckbox label="Locations" id="smc1" valuePassThru="true"
 *                        binding="#{weatherBean.locationsSelectManyComponent}"
 *                       value="#{weatherBean.locationsToRemove}">
 *    <f:selectItems value="#{weatherBean.locationSelectItems}" id="si1"/>
 * </tr:selectManyCheckbox>
 * <tr:commandButton id="deleteCB" text="Remove Locations"
 *                  actionListener="#{weatherBean.removeLocationListener}">
 * </tr:commandButton>
 * </pre>
 * The weatherBean might looks like this:
 * <pre>
 * public class WeatherBean implements Serializable
 * {
 *   public void setLocationsToRemove(UIXSelectMany locationsToRemove)
 *   {
 *     _locationsToRemove = locationsToRemove;
 *   }
 *   
 *   public UIXSelectMany getLocationsToRemove()
 *   {
 *     return _locationsToRemove;
 *   }
 *   
 *   public void removeLocationListener(ActionEvent actionEvent)
 *   {
 *     ... code calling getLocationsToRemove() to get the UIXSelectMany ...
 *   }
 *
 *   private UIXSelectMany _locationsToRemove
 * }
 * </pre>
 * This code has several problems:
 * <ol>
 *   <li>Since UIComponents aren't Serializable, the class will fail serialization during fail-over
 *   when default Serialization attempts to serialize _locationsToRemove.</li>
 *   <li>If the user opens two windows on this page, only the last window rendered have the
 *   correct UIXSelectMany instance.  If the remove locations button is pressed on the first
 *   window after rendering the second window, the wrong UIXSelectMany instance will be used.</li>
 *   <li>Since UIComponents aren't thread-safe, the above case could also result in bizare
 *   behavior if requests from both windows were being processed by the application server at the
 *   same time.</li>
 *   <li>If the Trinidad view state token cache isn't used, or if the user navigates to this page
 *   using the backbutton, a new UIXSelectMany instance will be created, which also won't match
 *   the instance we are holding onto.</li>
 *   <li>If we don't clear the UIXSelectMany instance when we navigate off of this page, we will
 *   continue to pin the page's UIComponent tree in memory for the lifetime of the Session.
 *   </li>
 * </ol>
 * Rewritten using ComponentReference, the weatherBean might looks like this:
 * <pre>
 * public class WeatherBean implements Serializable
 * {
 *   public void setLocationsToRemove(UIXSelectMany locationsToRemove)
 *   {
 *     _locationsToRemoveRef = UIComponentReference.newUIComponentReference(locationsToRemove);
 *   }
 *   
 *   public UIXSelectMany getLocationsToRemove()
 *   {
 *     return _locationsToRemoveRef.getComponent();
 *   }
 *   
 *   public void removeLocationListener(ActionEvent actionEvent)
 *   {
 *     ... code calling getLocationsToRemove() to get the UIXSelectMany ...
 *   }
 *
 *   private UIComponentReference<UIXSelectMany> _locationsToRemoveRef
 * }
 * </pre>
 * The above code saves a reference to the component passed to the managed bean and then
 * retrieves the correct instance given the current UIViewRoot for this request whenever
 * <code>getLocationsToRemove()</code> is called.
 * <p><b>Please note:</b>
 * <ul>
 * <li>This class is <b>not completely</b> thread-safe, since it depends on <code>UIComponent</code>
 * APIs, however the class is safe to use as long as either of the following is true
 * <ol>
 * <li>The component passed to <code>newUIComponentReference</code> has an id and is in the
 * component hierarchy when newUIComponentReference is called and all subsequent calls to
 * <code>getComponent</code> are made from Threads with a valid FacesContext
 * </li>
 * <li>The first call to <code>getComponent</code> is on the same Thread that
 * <code>newUIComponentReference</code> was called on</li>
 * </ol>
 * </li>
 * <li>The passed in <code>UIComponent</code> is <b>required</b> to have an <code>ID</code></li>
 * <li>The reference will break if the <code>UIComponent</code> is moved between
 * <code>NamingContainer</code>s <b>or</b>
 * if any of the ancestor <code>NamingContainer</code>s have their IDs changed.</li>
 * <li>The reference is persistable. <b>However</b> <code>UIComponent</code>s are not
 * <code>Serializable</code> and therefore can not be used at any scope longer than request.</li>
 * </ul>
 * 
 * @see ComponentReference#newUIComponentReference(UIComponent)
 * @see ComponentReference#getComponent()
 */
public abstract class ComponentReference<T extends UIComponent> implements Serializable
{
  // don't allow other subclasses
  private ComponentReference(List<Object> componentPath)
  {
    _componentPath = componentPath;
  }

  /**
   * Factory method to create an instance of the <code>ComponentReference</code> class, which
   * returns a Serializable and often thread-safe reference to a 
   * <code>UIComponent</code>.
   * 
   * @param component the <code>UIComponent</code> to create a reference to.
   * @return <code>ComponentReference</code> the reference to the component
   * @throws NullPointerException if component is <code>null</code>
   */
  public static <T extends UIComponent> ComponentReference<T> newUIComponentReference(T component)
  {
    // store the id of the component as a transient field since we can grab it from the scoped id
    // but want it available to validate the component we found from the path
    String compId = component.getId();

    // if the component is in the hierarchy, the topmost component will be the UIViewRoot
    if ((compId != null) && (getUIViewRoot(component) != null))
    {
      // component has an id and is in the hierarachy, so we can use a stable reference
      String scopedId = calculateScopedId(component, compId);
      
      return new StableComponentReference(scopedId, compId, calculateComponentPath(component));
    }
    else
    {
      // Oh well, deferred reference it is
      ComponentReference<T> reference = new DeferredComponentReference<T>(component);
      
      // Add to the list of Referernces that may need initialization
      _addToEnsureInitializationList(reference);
      
      return reference;
    }
  }
  
  /**
   * This method will use a calculated "component path" to walk down to the <code>UIComponent</code>
   * that is referenced by this class. If the component can not be found, the <code>getComponent()</code>
   * will return <code>null</code>. 
   * 
   * @return the referenced <code>UIComponent</code> or <code>null</code> if it can not be found.
   * @throws IllegalStateException if the component used to create the
   * ComponentReference is not in the component tree or does <b>not</b> have an <code>Id</code>
   * @see ComponentReference#newUIComponentReference(UIComponent)
   */
   @SuppressWarnings("unchecked")
   public final T getComponent()
   {
     // get the scopedId, calculating it if necessary
     String scopedId = getScopedId();
         
     UIComponent foundComponent = null;

     // In order to find the component with its
     // calculated path, we need to start at the ViewRoot;
     UIViewRoot root = FacesContext.getCurrentInstance().getViewRoot();

     List<Object> componentPath = _componentPath;
     
     if (componentPath != null) 
     {
       // Walk down the component tree, to the component we are looking for.
       // We start at the ViewRoot and use the previous calculated "component path"
       foundComponent = _walkPathToComponent(root, componentPath);
     }

     // Check if we really found it with the previously created "component path"
     if (foundComponent == null || (!getComponentId().equals(foundComponent.getId())))
     {
       // OK, we were not luck with the calculated "component path", let's
       // see if we can find it by using the "scoped ID" and the regular 
       // findComponent();
       foundComponent = root.findComponent(scopedId);

       // was the regular findComponent() successful ?
       if (foundComponent != null)
       {        
         // OK, now let's rebuild the path
         _componentPath = calculateComponentPath(foundComponent);
       }
     }

     return (T)foundComponent;
   }

   /**
    * Called by the framework to ensure that deferred ComponentReferences are completely
    * initialized before the UIComponent that the ComponentReference is associated with
    * is not longer valid.
    * @throws IllegalStateException if ComponentReference isn't already initialized and 
    * the component used to create the
    * ComponentReference is not in the component tree or does <b>not</b> have an <code>Id</code>
    */
   public abstract void ensureInitialization();
    
  /**
   * ComponentRefs are required to test for equivalence by the equivalence of their scoped ids
   * @param o
   * @return
   */
  @Override
  public final boolean equals(Object o)
  {
    if (o == this)
    {
      return true;
    }
    else if (o instanceof ComponentReference)
    {
      return getScopedId().equals(((ComponentReference)o).getScopedId());
    }
    else
    {
      return false;
    }
  }
 
   /**
    * ComponentRefs must use the hash code of their scoped id as their hash code
    * @return
    */
  @Override
  public final int hashCode()
  {
    return getScopedId().hashCode();
  }
  
  @Override
  public final String toString()
  {
    return super.toString() + ":" + getScopedId();
  }
  
  /**
   * Returns the scoped id for this ComponentReference
   * @return
   */
  protected abstract String getScopedId();

  /**
   * Returns the id of the Component that this ComponentReference points to
   * @return
   */
  protected abstract String getComponentId();

  protected final void setComponentPath(List<Object> componentPath)
  {
    _componentPath = componentPath;
  }

  /**
   * Creates the "component path" started by the given <code>UIComponent</code> up the <code>UIViewRoot</code>
   * of the underlying component tree. The hierarchy is stored in a <code>List</code> of <code>Object</code>s
   * (the <code>componentHierarchyList</code> parameter). If the given <code>UIComponent</code> is nested in a 
   * <code>Facet</code> of a <code>UIComponent</code>, we store the name of the actual <code>facet</code> in
   * the list. If it is a regular child, we store its position/index. 
   * 
   * <p> 
   * To calculate the <code>scopedID</code> we add the ID of every <code>NamingContainer</code> that we hit,
   * while walking up the component tree, to the <code>scopedIdList</code>. 
   * 
   * @param component The <code>UIComponent</code> for this current iteration
   * 
   * @see #newUIComponentReference
   */
  protected static List<Object> calculateComponentPath(UIComponent component)
  {
    // setUp of list that stores information about the FACET name or the COMPONENT index
    List<Object> componentHierarchyList = new ArrayList<Object>();

    // stash the component and parent , for the loop
    UIComponent currComponent = component;
    UIComponent currParent = currComponent.getParent();

    // enter the loop, if there is a parent for the current component
    while(currParent != null)
    {
      int childIndex = currParent.getChildren().indexOf(currComponent);
      
      // is the given component a child of the parent?
      if (childIndex != -1)
      {          
        // if so, add the INDEX (type: int) at the beginning of the list
        componentHierarchyList.add(childIndex);
      }
      else
      {
        // If the component is not a child, it must be a facet.
        // When the component is nested in a facet, we need to find
        // the name of the embedding FACET
        Set<Map.Entry<String, UIComponent>> entries = currParent.getFacets().entrySet();
        for(Map.Entry<String, UIComponent> entry : entries)
        {
          if (currComponent.equals(entry.getValue()))
          {
            // once we identified the actual component/facet,
            // we store the name (type: String)at the
            // beginning of the list and quite the loop afterwards
            componentHierarchyList.add(entry.getKey());
            break;
          }
        }
      }

      // set references for the next round of the loop
      currComponent = currParent;
      currParent = currParent.getParent();
    }

    // done with the loop as >currComponent< has no own parent. Which
    // means we must talk to <code>UIViewRoot</code> here.
    // Otherwise the component is not connected to the tree, but we should have already checked this
    // before calling this function
    if (!(currComponent instanceof UIViewRoot))
      throw new IllegalStateException(
                   "The component " + component + " is NOT connected to the component tree");
  
    return componentHierarchyList;
  }
  
  protected static String calculateScopedId(
    UIComponent component,
    String      componentId)
  {
    if (componentId == null)
      throw new IllegalStateException("Can't create a ComponentReference for component " +
                                      component +
                                      " no id");    
    int scopedIdLength = componentId.length();
  
    List<String> scopedIdList = new ArrayList<String>();
   
    // determine how many characters we need to store the scopedId.  We skip the component itself,
    // because we have already accounted for its id
    UIComponent currAncestor = component.getParent();
    
    while (currAncestor != null)
    {
      // add the sizes of all of the NamingContainer ancestors, plus 1 for each NamingContainer separator
      if (currAncestor instanceof NamingContainer)
      {
        String currId = currAncestor.getId();
        scopedIdLength += currId.length() + 1;
      
        // add the NamingContainer to the list of NamingContainers
        scopedIdList.add(currId);
      }
      
      currAncestor = currAncestor.getParent();
    }
    
    // now append all of the NamingContaintes
    return _createScopedId(scopedIdLength, scopedIdList, componentId);
  }

  protected Object writeReplace() throws ObjectStreamException
  {
    // Only use the proxy when Serializing
    return new SerializationProxy(getScopedId());
  }
  
  private void readObject(@SuppressWarnings("unused") ObjectInputStream stream) throws InvalidObjectException
  {
    // We can't be deserialized directly
    throw new InvalidObjectException("Proxy required");
  } 

  /**
   * Add a reference to the list of References that may need initialization later
   * @param reference
   */
  private static void _addToEnsureInitializationList(ComponentReference<?> reference)
  {
    Map<String, Object> requestMap = 
                            FacesContext.getCurrentInstance().getExternalContext().getRequestMap();
    
    Collection<ComponentReference<?>> initializeList = (Collection<ComponentReference<?>>)
                                             requestMap.get(_FINISH_INITIALIZATION_LIST_KEY);
    
    if (initializeList == null)
    {
      initializeList = new ArrayList<ComponentReference<?>>();
      requestMap.put(_FINISH_INITIALIZATION_LIST_KEY, initializeList);
    }
    
    initializeList.add(reference);
  }

  /**
   * Transform the <code>scopedIdList</code> of "important" component IDs to 
   * generate the <code>scopedID</code> for the referenced <code>UIComponent</code>.
   * 
   * Uses the <code>scopedIdLength</code>
   */
  private static String _createScopedId(int scopedIdLength, List<String> scopedIdList, String componentId)
  {
    StringBuilder builder = new StringBuilder(scopedIdLength);

    for (int i = scopedIdList.size() - 1; i >= 0 ; i--)
    {
      builder.append(scopedIdList.get(i));
      builder.append(NamingContainer.SEPARATOR_CHAR);
    }

    builder.append(componentId);
    
    // store the (final) scopedId
    return builder.toString();
  }
  
  protected static UIViewRoot getUIViewRoot(UIComponent component)
  {
    // stash the component and parent , for the loop
    UIComponent currComponent = component;
    UIComponent currParent = currComponent.getParent();

    while(currParent != null)
    {
      currComponent = currParent;
      currParent = currParent.getParent();
    }
    
    return (currComponent instanceof UIViewRoot) ? (UIViewRoot)currComponent : null;
  }

  /**
   * Starts to walk down the component tree by the given <code>UIViewRoot</code>. It
   * uses the <code>hierarchyInformationList</code> to check if the it needs to
   * walk into a FACET or an INDEX of the component.
   * 
   * @see ComponentReference#calculateComponentPath(UIComponent)
   */
  private UIComponent _walkPathToComponent(UIViewRoot root, List<Object> componentPath)
  {
    UIComponent currFound = root;

    // iterate backwards since we appending the items starting from the component
    for (int i = componentPath.size() - 1; i >= 0 ; i--)
    {
      Object location = componentPath.get(i);

      // integer means we need to get the kid at INDEX obj
      // but let's not try to lookup from a component with
      // no kids
      if (location instanceof Integer)
      {
        int childIndex = ((Integer)location).intValue();
        
        List<UIComponent> children = currFound.getChildren();

        // make sure there is actually a child at this index
        if (childIndex < children.size())
        {
          currFound = children.get(childIndex);
        }
        else
        {
          // something changed, there aren't enough children so give up
          return null;
        }
      }
      else
      {
        // there is only ONE child per facet! So get the
        // component of FACET "obj"
        String facetName = location.toString();

        currFound = currFound.getFacets().get(facetName);

        // component isn't under the same facet anymore, so give up
        if (currFound == null)
          return null;
      }
    }
    return currFound;
  }
  
  /**
   * ComponentReference where the scopedId is calculatable at creation time
   */
  private static final class StableComponentReference extends ComponentReference
  {
    private StableComponentReference(String scopedId)
    {      
      this(scopedId,
           // String.substring() is optimized to return this if the entire string
           // is the substring, so no further optimization is necessary
           scopedId.substring(scopedId.lastIndexOf(NamingContainer.SEPARATOR_CHAR)+1),
           null);
    }
 
    private StableComponentReference(
      String scopedId,
      String componentId,
      List<Object> componentPath)
    {
      super(componentPath);

      if (scopedId == null)
        throw new NullPointerException();
      
      _scopedId = scopedId;
      _componentId = componentId;
    }
    
    public void ensureInitialization()
    {
      // do nothing--stable references are always fully initialized
    }

    protected String getScopedId()
    {
      return _scopedId;
    }

    protected String getComponentId()
    {
      return _componentId;
    }
 
    private final String _componentId;
    private final String _scopedId;

    private static final long serialVersionUID = 1L;
  }
  
  /**
   * ComponentReference where the component isn't ready to have its ComponentReference calculated at
   * creation time.  Instead we wait until getComponent() is called, or the ComponentReference is
   * Serialized.
   */
  private static final class DeferredComponentReference<T extends UIComponent> extends ComponentReference
  {
    /**
     * Private constructor, used by <code>ComponentReference.newUIComponentReference</code> 
     * @param component the <code>UIComponent</code> we want to store the path for
     */
    private DeferredComponentReference(T component)
    {
      super(null);
      
      // temporarily store away the component
      _component = component;
    }

    public void ensureInitialization()
    {
      // getScopedId() ensures we are initialized
      getScopedId();
    }

    protected String getScopedId()
    {
      String scopedId = _scopedId;
      
      // we have no scopedId, so calculate the scopedId and finish initializing
      // the DeferredComponentReference
      if (scopedId == null)
      {
        UIComponent component = _component;
        
        // need to check that component isn't null because of possible race condition if this
        // method is called from different threads.  In that case, scopedId will have been filled
        // in, so we can return it
        if (component != null)
        {
          String componentId = component.getId();
                    
          scopedId = calculateScopedId(component, componentId);
          _scopedId = scopedId;
          _componentId = componentId;
          
          // store away our component path while we can efficiently calculate it
          setComponentPath(calculateComponentPath(component));
          
          _component = null;
        }
        else
        {
          scopedId = _scopedId;
        }
      }
      
      return scopedId;
    }

    protected String getComponentId()
    {
      return _componentId;
    }
    
    private transient T _component;
    private transient volatile String _componentId;
    private volatile String _scopedId;

    private static final long serialVersionUID = 1L;
  }

  /**
   * Proxy class for serializing ComponentReferences.  The Serialized for is simply the scopedId
   */
  private static final class SerializationProxy implements Serializable
  {
    SerializationProxy(String scopedId)
    {
      _scopedId = scopedId;
    }

    private Object readResolve()
    {
      return new StableComponentReference(_scopedId);
    }
      
    private final String _scopedId;

    private static final long serialVersionUID = 1L;
  }

  private transient volatile List<Object> _componentPath;

  private static final String _FINISH_INITIALIZATION_LIST_KEY = ComponentReference.class.getName() +
                                                                "#FINISH_INITIALIZATION";
  
  private static final long serialVersionUID = -6803949693688638969L;
}