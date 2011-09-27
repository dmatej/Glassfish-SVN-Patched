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
package org.apache.myfaces.trinidad.context;

import java.awt.Color;

import java.io.IOException;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitHint;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.config.RegionManager;
import org.apache.myfaces.trinidad.event.WindowLifecycleListener;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;


/**
 * Context class for all per-request and per-webapp information
 * required by Trinidad.  A <code>RequestContext</code> object can be
 * retrieved with the static {@link #getCurrentInstance} method.
 * There is one and only one <code>RequestContext</code> object
 * active in any one thread.
 * <p>
 * This class does not extend <code>FacesContext</code>;  this is intentional,
 * as extending <code>FacesContext</code> requires taking over the
 * <code>FacesContextFactory</code>.
 * <p>
 */
// TODO Refactor this class after everything gets added to it.
// TODO There's some values in here that seem to affect only output (e.g.,
//  right-to-left); that's not great, since ideally that detail would be
//  buried in something more renderer-specific.
abstract public class RequestContext
{
  /**
   * Name of the EL implicit variable ("requestContext") that is used to
   * expose this context object.
   */
  static public final String VARIABLE_NAME =
    "requestContext";

  // Omitted APIs:
  //
  // LocaleContext
  // =============
  // Locale getTranslationLocale
  //
  // DateFormatContext:
  // =================
  // int getTwoDigitYearStart
  // boolean isLenient (very lame API, definitely gone)


  /**
   * Retrieves the RequestContext active for the current thread.
   */
  static public RequestContext getCurrentInstance()
  {
    return _CURRENT_CONTEXT.get();
  }



  /**
   * Creates an RequestContext.  RequestContext is abstract
   * and may not be instantiated directly.
   * @see RequestContextFactory
   */
  protected RequestContext()
  {
  }

  //
  // State related APIs
  //

  /**
   * Returns a Map of objects at "pageFlow" scope.
   */
  public abstract Map<String, Object> getPageFlowScope();

  /**
   * @deprecated use getPageFlowScope()
   */
  @Deprecated
  final public Map<String, Object> getProcessScope()
  {
    return getPageFlowScope();
  }

  /**
   * Method to obtain a Map stored on the view.
   * <p>This calls {@link #getViewMap(boolean)} with a true value for create.</p>
   * <p>This is a pre-cursor implementation to the JSF 2.0 UIViewRoot.getViewMap() function.
   * The implementation is taken from the initial UIViewRoot implementation in JSF but without
   * the event notification support.</p>
   */
  public abstract Map<String, Object> getViewMap();

  /**
   * Method to obtain a Map stored on the view.
   * <p>This is a pre-cursor implementation to the JSF 2.0 UIViewRoot.getViewMap() function.
   * The implementation is taken from the initial UIViewRoot implementation in JSF but without
   * the event notification support.</p>
   *
   * @param create if the map should be created if it already does not exist.
   */
  public abstract Map<String, Object> getViewMap(boolean create);

  /**
   * Returns a Map of objects associated with the current window if any.  If there is no
   * current window, the Session Map is returned.
   * @return Map for storing objects associated with the current window.
   * @see org.apache.myfaces.trinidad.context.Window#getWindowMap
   */
  public Map<String, Object> getWindowMap()
  {
    WindowManager wm = getWindowManager();

    ExternalContext extContext = FacesContext.getCurrentInstance().getExternalContext();

    Window window = wm.getCurrentWindow(extContext);

    if (window != null)
    {

      return window.getWindowMap();
    }
    else
    {
      return extContext.getSessionMap();
    }
  }

  //
  // Dialog APIs
  //

  /**
   * Returns from a dialog raised by a
   * {@link org.apache.myfaces.trinidad.component.UIXCommand UIXCommand} component,
   * or any component implementing
   * {@link org.apache.myfaces.trinidad.component.DialogSource DialogSource},
   * or any direct calls to {@link #launchDialog launchDialog()}.
   * <p>
   * @see org.apache.myfaces.trinidad.event.ReturnEvent
   * @param returnValue the value to be delivered in the the ReturnEvent
   */
  // TODO Do we need an explicit "cancelled" concept, or
  // is a null returnValue good enough?
  public abstract void returnFromDialog(
    Object returnValue,
    Map<Object, Object> returnParameters);


  /**
   * Returns an DialogService, which exposes a number
   * of APIs needed by component and framework developers.  This
   * will only rarely be needed by page authors.
   */
  public abstract DialogService getDialogService();

  /**
   * Launch a dialog, optionally raising it in a new dialog window.
   * <p>
   * The dialog will receive a new <code>pageFlowScope</code> map,
   * which includes all the values of the currently available pageFlowScope
   * as well as a set of properties passed to this function in
   * the <code>dialogParameters</code> map.  Changes to this newly
   * created scope will not be visible once the dialog returns.
   * <p>
   * @param dialogRoot the UIViewRoot for the page being launched
   * @param dialogParameters a set of parameters to populate the
   *   newly created pageFlowScope
   * @param source the UIComponent that launched the dialog and
   *   should receive the {@link org.apache.myfaces.trinidad.event.ReturnEvent}
   *   when the dialog is complete.
   * @param useWindow if true, use a popup window for the dialog
   *    if available on the current user agent device
   * @param windowProperties the set of UI parameters used to
   *   modify the window, if one is used.  The set of properties that\
   *   are supported will depend on the <code>RenderKit</code>, but
   *   common examples include "width", "height", "top" and "left".
   */
  public abstract void launchDialog(
    UIViewRoot  dialogRoot,
    Map<String, Object> dialogParameters,
    UIComponent source,
    boolean     useWindow,
    Map<String, Object> windowProperties);

  //
  // General Apache Trinidad
  //

  /**
   * Returns true if JSF is currently processing a postback request.
   * <code>isPostback()</code> will return false if this is a request
   * for an initial render of a page (that is, if Apply Request Values
   * never executes), or if during the request the user is navigated
   * to a different page (because of a navigation rule, etc).  For
   * example, during a request that results in a navigation to a new
   * page, <code>isPostback()</code> will return true from Apply
   * Request Values to Invoke Application, then false afterwards;
   * whereas if there was no navigation, it would return true
   * <p>
   * The value of this method is undefined during (or before)
   * the Restore View phase, but can be used in the afterPhase()
   * method of a PhaseListener for Restore View.
   * </p>
   */
  public abstract boolean isPostback();

  /**
   * Method to indicate if this current HTTP request is a
   * partial page rendering request.
   *
   * @param context the <code>FacesContext</code> object for
   * the request we are processing
   * @return is this request a PPR request?
   */
  public abstract boolean isPartialRequest(FacesContext context);
  /**
   * Returns true if output should contain debugging information.
   */
  public abstract boolean isDebugOutput();

  /**
   * Returns true if client-side validation should be disabled.
   */
  public abstract boolean isClientValidationDisabled();

  /**
   * Returns the "output mode" - printable, etc.
   */
  public abstract String getOutputMode();


  /**
   * Returns the name of the preferred skin family.
   */
  public abstract String getSkinFamily();

  /**
   * Returns the name of the skin version that goes with the skin-family.
   */
  public String getSkinVersion()
  {
    return null;
  }

  /**
   * Determines whether the current View Root is an internal view
   * @param context Faces context
   * @return true if the current View Root is an internal view, false otherwise
   */
  public abstract boolean isInternalViewRequest(FacesContext context);

  public enum Accessibility
  {
    /**
     * Output supports accessibility features
     */
    DEFAULT("default"),

    /**
     * Accessibility-specific constructs are stripped out to optimize output size
     */
    INACCESSIBLE("inaccessible"),

    /**
     * Accessibility-specific constructs are added to improve behavior under a screen reader
     * (but may affect other users negatively)
     */
    SCREEN_READER("screenReader");

    Accessibility(String name)
    {
      _name = name;
    }

    @Override
    public String toString()
    {
      return _name;
    }

    private final String _name;
  };


  public enum ClientValidation
  {
    ALERT("alert"),
    INLINE("inline"),
    DISABLED("disabled");

    ClientValidation(String name)
    {
      _name = name;
    }

    @Override
    public String toString()
    {
      return _name;
    }

    private final String _name;
  };

  /**
   * Returns the name of the current accessibility mode.
   */
  public abstract Accessibility getAccessibilityMode();

  /**
   * Returns the accessibility profile for the current request.
   */
  public abstract AccessibilityProfile getAccessibilityProfile();

  /**
   * Returns the name of the current client validation mode.
   */
  public abstract ClientValidation getClientValidation();

  /**
   * Returns the system wide setting to turn animation on/off.
   */
  public abstract boolean isAnimationEnabled();

  //
  //  General localization
  //

  /**
   * Returns true if the user should be shown output in right-to-left.
   */
  public abstract boolean isRightToLeft();

  /**
   * Returns the formatting locale.  Converters without an explicit locale
   * should use this to format values.  If not set, converters should
   * default to the value of FacesContext.getViewRoot().getLocale().
   * This will, by default, simply return null.
   */
  public abstract Locale getFormattingLocale();

  //
  //  Number formatting
  //

  /**
   * Return the separator used for groups of numbers.  If NUL (zero),
   * use the default separator for the current language.
   */
  public abstract char getNumberGroupingSeparator();

  /**
   * Return the separator used as the decimal point.  If NUL (zero),
   * use the default separator for the current language.
   */
  public abstract char getDecimalSeparator();

  /**
   * Return the ISO 4217 currency code used by default for formatting
   * currency fields when those fields do not specify an explicit
   * currency field via their converter.  If this returns null, the default
   * code for the current locale will be used.
   */
  // TODO do we need to provide getCurrencySymbol() as well?
  public abstract String getCurrencyCode();

  //
  // DateFormating API
  //
  /**
   * Returns the year offset for parsing years with only two digits.
   * If not set this is defaulted to <code>1950</code>
   * This is used by @link{org.apache.myfaces.trinidad.faces.view.converter.DateTimeConverter}
   * while converting strings to Date object.
   */
  public abstract int getTwoDigitYearStart();

  //
  // Help APIs
  //

  /**
   * Return the URL to an Oracle Help for the Web servlet.
   */
  // TODO Add support for non-OHW help systems
  public abstract String getOracleHelpServletUrl();

  /**
   * Returns a Map that will accept topic names as keys, and return
   * an URL as a result.
   */
  public abstract Map<String, Object> getHelpTopic();

  /**
   * Returns a Map that will accept help system properties as keys, and return
   * an URL as a result.
   */
  public abstract Map<String, Object> getHelpSystem();

  //
  // Date formatting
  //

  /**
   * Returns the default TimeZone used for interpreting and formatting
   * date values.
   */
  public abstract TimeZone getTimeZone();

  /**
   * Gets the ChangeManager for the current application.
   */
  public abstract ChangeManager getChangeManager();

  /**
   * Gets a per application concurrent map. There is no synchronization
   * with ServletContext attributes.
   */
   public ConcurrentMap<String, Object> getApplicationScopedConcurrentMap()
   {
     ClassLoader cl = _getClassLoader();

     ConcurrentMap<String, Object> classMap = _APPLICATION_MAPS.get(cl);

     if (classMap == null)
     {
       ConcurrentMap<String, Object> newClassMap = new ConcurrentHashMap<String, Object>();
       ConcurrentMap<String, Object> oldClassMap = _APPLICATION_MAPS.putIfAbsent(cl, newClassMap);

       classMap = ((oldClassMap != null)? oldClassMap : newClassMap);

       assert(classMap != null);
     }

     return classMap;
   }

  /**
    * Gets the PageFlowScopeProvider for the current application.
    */
  public abstract PageFlowScopeProvider getPageFlowScopeProvider();

  /**
    * Gets the PageResolver for the current application.
    */
  public abstract PageResolver getPageResolver();

  /**
   * Gets the RegionManager for the current application.
   */
  public abstract RegionManager getRegionManager();

  //
  // Partial Page Rendering support
  //
  /**
   * Add a component as a partial target. In response to a partial event, only
   * components registered as partial targets are re-rendered.  For
   * a component to be successfully re-rendered when it is manually
   * added with this API, it should have an explictly set "id".  If
   * not, partial re-rendering may or may not work depending on the
   * component.
   */
  public abstract void addPartialTarget(UIComponent newTarget);

  /**
   * Add components relative to the given component as partial targets.
   * <p>
   * See {@link #addPartialTarget(UIComponent)} for more information.
   * </p>
   * @param from the component to use as a relative reference for any
   * relative IDs in the list of targets
   * @param targets array of targets relative to the from component that
   * should be added as targets.
   * @see ComponentUtils#findRelativeComponent(UIComponent, String)
   */
  public abstract void addPartialTargets(UIComponent from, String... targets);

  /**
   * Returns the set of partial targets related to a given UIComponent.
   */
  public abstract Set<UIComponent> getPartialTargets(UIComponent newTarget);

  /**
   * Adds a listener on a set of particular triggering components. If one of
   * the named components gets updated in response to a partial event, then
   * this listener component will be rerendered during the render phase (i.e.
   * it will be added as a partialTarget). The list should consist of names
   * suitable for use with the findComponent method on UIComponent.
   */
  public abstract void addPartialTriggerListeners(UIComponent listener,
                                                  String[] trigger);

  /**
   * Called when any component gets updated. Any partial target components
   * listening on this component will be added to the partialTargets list in
   * the render phase.
   */
  public abstract void partialUpdateNotify(UIComponent updated);

  //
  // Miscellaneous functionality
  //

  /**
   * <p>Creates a VisitContext instance for use with
   * {@link UIComponent#visitTree}.</p>
   *
   * @param context the FacesContext for the current request
   * @param ids the client ids of the components to visit.  If null,
   *   all components will be visited.
   * @param hints the VisitHints to apply to the visit
   * @param phaseId.  ignored.
   * @return a VisitContext instance that is initialized with the
   *   specified ids and hints.
   * @deprecated use {@link VisitContext#createVisitContext(FacesContext, Collection<String>, Set<VisitHint>)} instead
   */
  public final VisitContext createVisitContext(
    FacesContext context,
    Collection<String> ids,
    Set<VisitHint> hints,
    PhaseId phaseId)
  {
    return VisitContext.createVisitContext(context, ids, hints);
  }

  public abstract UploadedFileProcessor getUploadedFileProcessor();

  public abstract Long getUploadedFileMaxMemory();

  public abstract Long getUploadedFileMaxDiskSpace();

  public abstract String getUploadedFileTempDir();

  /**
   * Returns a Map that takes color palette names as keys, and returns
   * the color palette as a result.
   */
  public abstract Map<String, List<Color>> getColorPalette();

  /**
   * Returns a Map that performs message formatting with a recursive Map
   * structure.  The first key must be the message formatting mask, and the
   * second the first parameter into the message. (The formatter Map supports
   * only a single parameter at this time.)
   */
  public abstract Map<Object, Map<Object,String>> getFormatter();

  /**
   * Returns the Agent information for the current context
   */
  public abstract Agent getAgent();

  /**
   * Saves the state of a UIComponent tree into an Object.  The
   * Object will be serializable, unless a UIComponent
   * in this tree contains a non-serializable property.  This
   * method does not check that condition.
   * @param component the component
   * @return an Object that can be passed to restoreComponent()
   *  to reinstantiate the state
   */
  public abstract Object saveComponent(UIComponent component);

  /**
   * Restores the state of a component.
   * @param state an Object created by a prior call to saveComponent().
   * @return the component
   */
  public abstract UIComponent restoreComponent(Object state)
                            throws ClassNotFoundException,
                                   InstantiationException,
                                   IllegalAccessException;

  /**
   * <p>
   * Returns the WindowManager for this request.  A non-null WindowManager
   * will always be returned.
   * </p><p>
   * The default implementation uses the first WindowManagerFactory specified
   * implementation class in a file named
   * <code>org.apache.myfaces.trinidad.context.WindowManagerFactory</code>
   * in the <code>META-INF/services</code> directory and uses the WindowManagerFactory
   * to create the WindowManager for this Session.  If no WindowManagerFactory is
   * found, a default WindowManager that never returns any Windows is used.
   * </p>
   * @return the WindowManager used for this Session.
   */
  public WindowManager getWindowManager()
  {
    // implement getWindowManager() in RequestContext for backwards compatibility

    // check if we have cached it for the request
    WindowManager windowManager = _windowManager;

    // get instance using the WindowManagerFactory
    if (windowManager == null)
    {
      FacesContext context = FacesContext.getCurrentInstance();

      // just in case we're called before the real JSF lifecycle starts
      if (context != null)
      {
        // check if we have cached it per session
        ExternalContext extContext = context.getExternalContext();

        // create a new instance using the WindowManagerFactory
        ConcurrentMap<String, Object> concurrentAppMap = getApplicationScopedConcurrentMap();

        WindowManagerFactory windowManagerFactory = (WindowManagerFactory)concurrentAppMap.get(
                                                              _WINDOW_MANAGER_FACTORY_CLASS_NAME);

        if (windowManagerFactory == null)
        {
          // we haven't registered a WindowManagerFactory yet, so use the services api to see
          // if a factory has been registered
          List<WindowManagerFactory> windowManagerFactories =
                                  ClassLoaderUtils.getServices(_WINDOW_MANAGER_FACTORY_CLASS_NAME);

          if (windowManagerFactories.isEmpty())
          {
            // no factory registered so use the factory that returns dummy stub WindowManagers
            windowManagerFactory = _STUB_WINDOW_MANAGER_FACTORY;
          }
          else
          {
            // only one WindowManager is allowed, use it
            windowManagerFactory = windowManagerFactories.get(0);
          }

          // save the WindowManagerFactory to the application if it hasn't already been saved
          // if it has been saved, use the previously registered WindowManagerFactory
          WindowManagerFactory oldWindowManagerFactory = (WindowManagerFactory)
                              concurrentAppMap.putIfAbsent(_WINDOW_MANAGER_FACTORY_CLASS_NAME,
                                                           windowManagerFactory);

          if (oldWindowManagerFactory != null)
            windowManagerFactory = oldWindowManagerFactory;
        } // create WindowManagerFactory

        // get the WindowManager from the factory.  The factory will create a new instance
        // for this session if necessary
        windowManager = windowManagerFactory.getWindowManager(extContext);

        // remember for the next call on this thread
        _windowManager = windowManager;
      }
    }

    return windowManager;
  }

  /**
   * Get the component context manager.
   * @return The manager of component context changes to be used to suspend and resume
   * component specific context changes.
   */
   public ComponentContextManager getComponentContextManager()
   {
     if (_componentContextManager == null)
     {
       _componentContextManager = new ComponentContextManagerImpl();
     }

     return _componentContextManager;
   }

  /**
   * Releases the RequestContext object.  This method must only
   * be called by the code that created the RequestContext.
   * @exception IllegalStateException if no RequestContext is attached
   * to the thread, or the attached context is not this object
   */
  public void release()
  {
    if (_LOG.isFinest())
    {
      _LOG.finest("RequestContext released.",
                  new RuntimeException("This is not an error. This trace is for debugging."));
    }

    Object o = _CURRENT_CONTEXT.get();
    if (o == null)
      throw new IllegalStateException(
              _addHelp("RequestContext was already released or " +
                       "had never been attached."));
    if (o != this)
      throw new IllegalStateException(_LOG.getMessage(
        "RELEASE_DIFFERENT_REQUESTCONTEXT_THAN_CURRENT_ONE"));

    _CURRENT_CONTEXT.remove();
  }

  /**
   * Attaches a RequestContext to the current thread.  This method
   * should only be called by a RequestContext object itself.
   * @exception IllegalStateException if an RequestContext is already
   * attached to the thread
   */
  public void attach()
  {
    if (_LOG.isFinest())
    {
      _LOG.finest("RequestContext attached.",
                  new RuntimeException(_LOG.getMessage(
                    "DEBUGGING_TRACE_NOT_ERROR")));
    }

    Object o = _CURRENT_CONTEXT.get();
    if (o != null)
    {
      throw new IllegalStateException(
              _addHelp("Trying to attach RequestContext to a " +
                       "thread that already had one."));
    }
    _CURRENT_CONTEXT.set(this);
  }

  private static String _addHelp(String error)
  {
    if (!_LOG.isFinest())
    {
      error += " To enable stack traces of each RequestContext attach/release call," +
        " enable Level.FINEST logging for the "+RequestContext.class;
    }
    return error;
  }

  //
  // Pick a ClassLoader
  //
  private ClassLoader _getClassLoader()
  {
    return Thread.currentThread().getContextClassLoader();
  }

  /**
   * Default WindowManagerFactory implementation that returns the StubWindowManager
   */
  private static final class StubWindowManagerFactory extends WindowManagerFactory
  {
    public WindowManager getWindowManager(ExternalContext extContext)
    {
      return _STUB_WINDOW_MANAGER;
    }

    private static final WindowManager _STUB_WINDOW_MANAGER = new StubWindowManager();
  }

  /**
   * Default WindowManager implementation that returns no Windows
   */
  private static final class StubWindowManager extends WindowManager
  {
    @Override
    public Window getCurrentWindow(ExternalContext extContext)
    {
      return null;
    }

    @Override
    public Map<String, Window> getWindows(ExternalContext extContext)
    {
      return Collections.emptyMap();
    }

    @Override
    public void addWindowLifecycleListener(
      ExternalContext extContext,
      WindowLifecycleListener windowListener)
    {
      // do nothing
    }

    @Override
    public void removeWindowLifecycleListener(
      ExternalContext extContext,
      WindowLifecycleListener windowListener)
    {
      // do nothing
    }

    @Override
    public void writeState(FacesContext context) throws IOException
    {
      // do nothing
    }
  }

  /* singleton for WindowManagerFactory that returns WindowManagers that don't do anything */
  private static final WindowManagerFactory _STUB_WINDOW_MANAGER_FACTORY =
                                                                    new StubWindowManagerFactory();

  private static final String _WINDOW_MANAGER_FACTORY_CLASS_NAME =
                                                              WindowManagerFactory.class.getName();

  @SuppressWarnings({"CollectionWithoutInitialCapacity"})
  private static final ConcurrentMap<ClassLoader, ConcurrentMap<String, Object>> _APPLICATION_MAPS =
       new ConcurrentHashMap<ClassLoader, ConcurrentMap<String, Object>>();
  static private final ThreadLocal<RequestContext> _CURRENT_CONTEXT =
    new ThreadLocal<RequestContext>();
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(RequestContext.class);

  private ComponentContextManager _componentContextManager;

  // window manager for this request
  private WindowManager _windowManager;
}
