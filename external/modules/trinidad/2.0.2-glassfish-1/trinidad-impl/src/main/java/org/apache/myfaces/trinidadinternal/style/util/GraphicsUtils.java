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
package org.apache.myfaces.trinidadinternal.style.util;

import java.awt.GraphicsEnvironment;
import java.awt.Toolkit;

import java.util.Map;
import java.util.Hashtable;

import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Graphics-related utilities.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/util/GraphicsUtils.java#0 $) $Date: 10-nov-2005.18:58:50 $
 */
public class GraphicsUtils
{
  /**
   * Tests whether the current environment supports graphical
   * operations (ie. AWT Graphics operations).
   * Note: On Unix, this calling this method will cause a connection to
   * the X server to be established, if the X server is available.
   *
   * @param return Returns true if this is a graphical environment - ie. if
   *   we can perform AWT graphical operations in this environment.
   */
  public static boolean isGraphicalEnvironment()
  {
    if (_sGraphicsLoaded)
      return _sIsGraphical;

    // We may have multiple requests running in different threads which
    // calling isGraphicalEnvironment() simultaneously.  We only want to
    // do the work once, so we synchronize this work using our class
    // as the lock.
    synchronized (GraphicsUtils.class)
    {
      if (!_sGraphicsLoaded)
      {
        if (_sGraphicsLoaderThread == null)
        {
          _LOG.fine(_GRAPHICS_INIT_MESSAGE);

          _sGraphicsLoaderThread = new Thread(new GraphicsLoader(),
                                              _GRAPHICS_LOADER_NAME);

          // We lower the priority on the loader thread in order to
          // keep the request thread responsive.  Otherwise, the loader
          // thread might get all of the time and end up timing out JServ.
          // Note: We only bother lowering the thread priority when we
          // aren't running in headless mode.  Under headless mode,
          // graphics initialization should be a very quick operation.
          if (!_isHeadless())
          {
            int priority = Thread.currentThread().getPriority();
            _sGraphicsLoaderThread.setPriority(priority - 1);
          }

          _sGraphicsLoaderThread.start();
        }

        _waitTillDone(_GRAPHICS_WAIT_MESSAGE, true);
      }
    }

    return _sIsGraphical;
  }

  /**
   * Tests whether the specified name is the name of a font in
   * the current graphical environment.
   *
   * @param name The name of the font to test
   * @return Returns true if the name is a valid font name in this graphical
   *   environment.  isFontName() also returns true if the current
   *   environment is non-graphical.
   */
  public static boolean isFontName(String name)
  {
    // Force the graphics environment to be loaded before we do anything.
    // If we don't have a graphics environment, just treat all names as
    // valid.
    if (!isGraphicalEnvironment())
      return true;

    // If the font names have already been loaded, perform the lookup
    if (_sFontsLoaded)
      return _isFontName(name);

    // If font names have not been loaded, load them now.
    // We may have multiple requests running in different threads which
    // calling isFontName simultaneously.  We only want to do the work once,
    // so we synchronize this work using our class as the lock.
    synchronized (GraphicsUtils.class)
    {
      if (!_sFontsLoaded)
      {
        if (_sFontLoaderThread == null)
        {
          _LOG.fine(_FONT_INIT_MESSAGE);

          _sFontLoaderThread = new Thread(new FontLoader(),
                                          _FONT_LOADER_NAME);

          // We lower the priority on the loader thread in order to
          // keep the request thread responsive.  Otherwise, the loader
          // thread might get all of the time and end up timing out JServ.
          // Note: We only bother lowering the thread priority when we
          // aren't running in headless mode.  Under headless mode,
          // font initialization should be a very quick operation.
          if (!_isHeadless())
          {
            int priority = Thread.currentThread().getPriority();
            _sFontLoaderThread.setPriority(priority - 1);
          }

          _sFontLoaderThread.start();
        }

        _waitTillDone(_FONT_WAIT_MESSAGE, false);
      }
    }

    return _isFontName(name);
  }

  // Called by GraphicsLoader thread upon completion
  static void __setGraphicsLoaded(boolean isGraphical)
  {
    synchronized (GraphicsUtils.class)
    {
      _sIsGraphical = isGraphical;
      _sGraphicsLoaded = true;
      _sGraphicsLoaderThread = null;

      if (_sIsGraphical)
        _LOG.fine(_GRAPHICS_SUCCESS_MESSAGE);
      else
        _LOG.warning(_GRAPHICS_FAILURE_MESSAGE);

      GraphicsUtils.class.notifyAll();
    }
  }

  // Called by FontLoader thread upon completion
  static void __setFontsLoaded(
    Map<String, Boolean> names
    )
  {
    synchronized (GraphicsUtils.class)
    {
      _sFontNames = names;
      _sFontsLoaded = true;
      _sFontLoaderThread = null;


      if (_sFontNames == null)
      {
        _LOG.warning(_FONT_FAILURE_MESSAGE);

        // If we didn't get any fonts, just use the built-in JAVA virutal fonts
        ArrayMap<String, Boolean> defaultFonts = 
          new ArrayMap<String, Boolean>(_DEFAULT_FONT_NAMES.length);
        
        for (int i = 0; i < _DEFAULT_FONT_NAMES.length; i++)
          defaultFonts.put(_DEFAULT_FONT_NAMES[i].toLowerCase(), Boolean.TRUE);

        _sFontNames = defaultFonts;
      }
      else
      {
        _LOG.fine(_FONT_SUCCESS_MESSAGE);
      }

      GraphicsUtils.class.notifyAll();
    }
  }

  // Looks up the specified name in the font table
  private static boolean _isFontName(String name)
  {
    if (name == null)
      return false;

    return (_sFontNames.get(name.toLowerCase()) != null);
  }

  // Waits until GraphicsLoader completes.  Due to problems in the
  // reference implementation of JDK 1.2.2, JServ may time out during
  // calls to getLocalGraphicsEnvironment() or getAvailableFontFamilyNames().
  // (See bug 1472548.)  We work around these problems by making these
  // calls in a separate thread.  We also must loop while waiting the
  // thread to complete.  We should be able to join the GraphicsLoader
  // thread, but it seems that if we don't wake up every once in a while,
  // we'll end up timing out.
  private static void _waitTillDone(
    String waitMessage,
    boolean loadingGraphics
    )
  {
    synchronized (GraphicsUtils.class)
    {
      long start = System.currentTimeMillis();
      long last = 0;

      boolean warned = false;  // Have we warned about delay?

      // If we are running in headless mode, we wait as long as necessary
      // for the graphics initialization to complete, since there
      // is no reason why the initialization should fail.  On the
      // other hand, if we are not running in headless mode, we
      // only wait for a limited interval, since we seen cases
      // where we have ended up waiting indefinitely for an X server
      // connection to initialize.
      long infoTimeout = 2000;
      long warningTimeout = 10000;
      long abortTimeout = 20000;

      boolean headless = _isHeadless();
      if (headless)
      {
        // If we are in headless, we never timeout - so set abort
        // intervals to a value that we'll never exceed.
        abortTimeout = Long.MAX_VALUE;
      }

      while ((loadingGraphics && !_sGraphicsLoaded) ||
              (!loadingGraphics && !_sFontsLoaded))
      {
        long current = System.currentTimeMillis();

        // Only log a message every second...
        if ((current - last) > infoTimeout)
        {
          _LOG.fine(waitMessage);
          last = current;
        }

        // Make sure that the haven't waited for too long.  After
        // 10 seconds, we warn about the delay in connecting to the
        // X server.  After 20 seconds, we abandon our attempt to
        // initialize the graphics environment
        if (loadingGraphics)
        {
          if (!warned && (current - start) > warningTimeout)
          {
            _LOG.warning(_GRAPHICS_DELAY_MESSAGE);
            warned = true;
          }
          else if ((current - start) > abortTimeout)
          {
            // Don't know whether interrupt() is useful here,
            // but give it a whirl
            if (_sGraphicsLoaderThread != null)
              _sGraphicsLoaderThread.interrupt();

            // Disable the graphics environment
            __setGraphicsLoaded(false);

            // Stop waiting
            return;
          }
        }

        try
        {
          GraphicsUtils.class.wait(250);
        }
        catch (InterruptedException e)
        {
          Thread.currentThread().interrupt();
          break;
        }
      }
    }
  }

  // Tests whether we are running in headless mode
  private static boolean _isHeadless()
  {
    // Check the java.awt.headless property.  Should be set to
    // "true" if we are running in headless mode.
    String headless = System.getProperty("java.awt.headless");

    return "true".equalsIgnoreCase(headless);
  }

  // Runnable which loads the graphics environment
  private static final class GraphicsLoader implements Runnable
  {
    public GraphicsLoader()
    {
    }

    public void run()
    {
      GraphicsEnvironment ge = null;
      Toolkit toolkit = null;

      try
      {
        ge = GraphicsEnvironment.getLocalGraphicsEnvironment();

        // It seems that checking the GraphicsEnvironment is not
        // sufficient on Windows (see bug 2604804).  Let's try
        // checking the Toolkit too...
        if (ge != null)
          toolkit = Toolkit.getDefaultToolkit();
      }
      catch (Throwable t)
      {
        // If any exception occurs during getLocalGraphicsEnvironment(),
        // we assume we've got a non-graphical environment
        _LOG.warning(_EXCEPTION_MESSAGE, t);
      }

      __setGraphicsLoaded(((ge != null) && (toolkit != null)));
    }

    private static final String _EXCEPTION_MESSAGE =
      "The following exception was thrown while initialiazing the graphics environment: ";
  }

  // Runnable which loads the list of available font family names
  private static final class FontLoader implements Runnable
  {
    public FontLoader()
    {
    }

    public void run()
    {
      GraphicsEnvironment ge = null;
      // -= Simon Lessard =-
      // TODO: Check if synchronization is required
      Hashtable<String, Boolean> fontNames = null;

      try
      {
        ge = GraphicsEnvironment.getLocalGraphicsEnvironment();

        if (ge != null)
        {
          String[] families = ge.getAvailableFontFamilyNames();

          if ((families != null) && (families.length > 0))
          {
            fontNames = new Hashtable<String, Boolean>(families.length);
            for (int i = 0; i < families.length; i++)
            {
              String name = families[i].toLowerCase();
              fontNames.put(name, Boolean.TRUE);
            }
          }
        }
      }
      catch (Throwable t)
      {
        // If any exception occurs during getLocalGraphicsEnvironment(),
        // we assume we've got a non-graphical environment
        _LOG.warning(_EXCEPTION_MESSAGE, t);
      }

      __setFontsLoaded(fontNames);
    }

    private static final String _EXCEPTION_MESSAGE =
      "The following exception was thrown while initialiazing fonts: ";
  }


  // Is this a graphical environment?
  private static boolean _sIsGraphical = false;

  // Map of valid font names
  private static Map<String, Boolean> _sFontNames = null;

  // Have we loaded the graphical environment yet?
  private static boolean _sGraphicsLoaded = false;

  // Have we loaded the font family names yet?
  private static boolean _sFontsLoaded = false;

  // The thread that we load the graphics environment from
  private static Thread _sGraphicsLoaderThread = null;

  // The thread that we load the font family names from
  private static Thread _sFontLoaderThread = null;

  // Name of the graphics loader thread
  private static final String _GRAPHICS_LOADER_NAME = "Ocelot-Graphics-Loader";

  // Name of the font loader thread
  private static final String _FONT_LOADER_NAME = "Ocelot-Graphics-Loader";

  private static final String _GRAPHICS_INIT_MESSAGE =
    "Initializing graphics environment...";

  private static final String _GRAPHICS_WAIT_MESSAGE =
    "Waiting for graphics environment initialization...";

  private static final String _GRAPHICS_DELAY_MESSAGE =
    "Could not yet initialize the graphics environment.  Will continue attempting to initialize graphics environment...";

  private static final String _GRAPHICS_SUCCESS_MESSAGE =
    "Finished initializing graphics environment.";

  private static final String _GRAPHICS_FAILURE_MESSAGE =
    "Could not initialize the graphical environment.  Please make sure that the DISPLAY environment variable is set correctly.  Proceeding with image generation disabled...";

  private static final String _FONT_INIT_MESSAGE =
    "Initializing fonts...";

  private static final String _FONT_WAIT_MESSAGE =
    "Waiting for font initialization...";

  private static final String _FONT_SUCCESS_MESSAGE =
    "Finished initializing fonts.";

  private static final String _FONT_FAILURE_MESSAGE =
    "Could not initialize fonts.  Using default font names";

  // List of default font names
  private static final String[] _DEFAULT_FONT_NAMES =
    new String[] { "dialog",
                   "dialoginput",
                   "monospaced",
                   "serif",
                   "sansserif",
                   "symbol" };

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(GraphicsUtils.class);
}
