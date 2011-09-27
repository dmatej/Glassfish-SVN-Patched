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
package org.apache.myfaces.trinidadinternal.renderkit.core;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.OptionalDataException;
import java.io.StringReader;
import java.io.StringWriter;

import java.util.Map;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import javax.faces.FacesException;
import javax.faces.application.StateManager;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.render.ResponseStateManager;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.Base64InputStream;
import org.apache.myfaces.trinidad.util.Base64OutputStream;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidadinternal.application.StateManagerImpl;
import org.apache.myfaces.trinidadinternal.util.ObjectInputStreamResolveClass;

/**
 * ResponseStateManager implementation for the Core RenderKit.
 * <p>
 */
public class CoreResponseStateManager extends ResponseStateManager
{

  @Override
  public Object getState(FacesContext context, String viewId)
  {
    // TODO see doc in StateManagerImpl.restoreView
    // (search for StateManagerImpl.RESPONSE_STATE_MANAGER_STATE_KEY) to see doc
    // about what's going on here
    Object state = context.getExternalContext().getRequestMap().get(
                                              StateManagerImpl.RESPONSE_STATE_MANAGER_STATE_KEY);

    if (state != null)
      return state;
    else
      return super.getState(context, viewId);
  }


  /**
   * Name of the form field that encodes the UI state.
   */
  static public final String FORM_FIELD_NAME = "org.apache.myfaces.trinidad.faces.FORM";

  /**
   * Write the state into the page.
   * @todo Stream the resulting state into the page's content
   *       instead of buffering it.
   * @todo Don't directly write out hidden input field;  use an abstraction
   */
  @Override
  public void writeState(
    FacesContext context,
    StateManager.SerializedView serializedView) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("input", null);
    rw.writeAttribute("type", "hidden", null);
    rw.writeAttribute("name", VIEW_STATE_PARAM, null);
    // Don't write out the ID, as it can be written
    // out twice
    //    rw.writeAttribute("id", VIEW_STATE_PARAM, null);

    String s = encodeSerializedViewAsString(serializedView);
    rw.writeAttribute("value", s, null);

    rw.endElement("input");
  }

  /**
   * A request is a postback if it contains the state parameter.
   */
  @Override
  public boolean isPostback(FacesContext context)
  {
    Map requestParams = context.getExternalContext().getRequestParameterMap();
    return requestParams.containsKey(VIEW_STATE_PARAM);
  }


  protected String encodeSerializedViewAsString(
    StateManager.SerializedView serializedView) throws IOException
  {
    if ((serializedView.getState() == null) &&
        (serializedView.getStructure() instanceof String))
      return _TOKEN_PREFIX + serializedView.getStructure();

    StringWriter sw = new StringWriter();
    BufferedWriter bw = new BufferedWriter(sw);
    Base64OutputStream b64_out = new Base64OutputStream(bw);
    GZIPOutputStream zip = new GZIPOutputStream(b64_out, _BUFFER_SIZE);
    ObjectOutput output = new ObjectOutputStream(zip);

    output.writeObject(serializedView.getStructure());
    output.writeObject(serializedView.getState());

    // this will flush the ObjectOutputStream, and close the GZIP stream, which
    // will call finish() on the GZIP stream, and then close the B64 stream,
    // which will cause it to write out the proper padding, and then close
    // the BufferedWriter which will also cause it to flush:
    output.close();

    String retVal = sw.toString();

    assert(!retVal.startsWith(_TOKEN_PREFIX));
    return retVal;
  }

  @Override
  public Object getTreeStructureToRestore(FacesContext context,
                                          String viewId)
  {
    Object[] view = _restoreSerializedView(context);
    if (view == null)
      return null;

    return view[0];
  }

  @Override
  public Object getComponentStateToRestore(FacesContext context)
  {
    Object[] view = _restoreSerializedView(context);
    if (view == null)
      return null;

    return view[1];
  }

  @Override
  public String getViewState(FacesContext context, Object state)
  {
    StateManager.SerializedView serializedView = _getSerializedView(context, state);
    try
    {
      return encodeSerializedViewAsString(serializedView);
    }
    catch (IOException e)
    {
      throw new FacesException();
    }
  }

  private StateManager.SerializedView _getSerializedView(FacesContext context, Object state)
  {

    StateManager.SerializedView view;
    if (state instanceof StateManager.SerializedView)
    {
      view = (StateManager.SerializedView) state;
    }
    else
    {
      if (state instanceof Object[])
      {
        Object[] stateArray = (Object[]) state;

        // in theory the state should be a black box, but the RI makes assumptions
        // that the state is an array of length 2
        if (stateArray.length == 2)
        {
          StateManager stateManager =
            context.getApplication().getStateManager();
          view =
              stateManager.new SerializedView(stateArray[0], stateArray[1]);
        }
        else
        {
          throw new IllegalArgumentException(_LOG.getMessage("UNEXPECTED_STATE"));
        }
      }
      else
      {
        throw new IllegalArgumentException(_LOG.getMessage("UNEXPECTED_STATE"));
      }
    }

    return view;
  }

  /**
   * Returns the State token if a valid state token is available in the request, or null if not.
   * @param external
   * @return
   */
  public static String getStateToken(ExternalContext external)
  {
    return _tokenStringFromStateString(_getStateString(external));
  }

  /**
   * Returns the State String for the request (either token or client state value)
   * @param external
   * @return
   */
  private static String _getStateString(ExternalContext external)
  {
    return external.getRequestParameterMap().get(VIEW_STATE_PARAM);
  }
  
  /** 
   * Give a client state String, return the tokenString for it if present, or null
   */
  private static String _tokenStringFromStateString(String stateString)
  {
    if (stateString != null)
    {
      // See if we've got a token;  that'll be the case if we're
      // prefixed by _TOKEN_PREFIX
      if (stateString.startsWith(_TOKEN_PREFIX))
      {
        return stateString.substring(_TOKEN_PREFIX.length());
      }
    }
    
    return null;     
  }


  /**
   * Restore the serialized view from the incoming request.
   * @todo ensure that the caching never gets confused by two
   *       different views being reconstituted in the same request?
   */
  @SuppressWarnings("unchecked")
  private Object[] _restoreSerializedView(
     FacesContext context)
  {
    ExternalContext external = context.getExternalContext();
    
    Map<String, Object> requestMap = external.getRequestMap();

    Object[] view = (Object[]) requestMap.get(_CACHED_SERIALIZED_VIEW);
    if (view == null)
    {
      String stateString = _getStateString(external);
      String tokenString = _tokenStringFromStateString(stateString);
 
      if (tokenString != null)
      {
        view = new Object[]{tokenString, null};
      }
      // Nope, let's look for a regular state field
      else
      {
        if (stateString != null)
        {
          StringReader sr = new StringReader(stateString);
          BufferedReader br = new BufferedReader(sr);
          Base64InputStream b64_in = new Base64InputStream(br);


          try
          {
            ObjectInputStream ois;
            ois = new ObjectInputStreamResolveClass( new GZIPInputStream( b64_in, _BUFFER_SIZE ));

            Object structure = ois.readObject();
            Object state = ois.readObject();
            ois.close();
            view = new Object[]{structure, state};
          }
          catch (OptionalDataException ode)
          {
            _LOG.severe(ode);
          }
          catch (ClassNotFoundException cnfe)
          {
            _LOG.severe(cnfe);
          }
          catch (IOException ioe)
          {
            _LOG.severe(ioe);
          }
        }
      }

      if (view != null)
        requestMap.put(_CACHED_SERIALIZED_VIEW, view);
    }

    return view;
  }




  /* Test code for dumping out the page's state
  static private void _dump(Object o)
  {
    System.out.println("DUMPING STATE");
    _dump(o, 0);
  }

  static private void _dump(Object o, int depth)
  {
    if (o instanceof Object[])
    {
      Object[] array = (Object[]) o;
      _spaces(depth);
      System.out.println("array of length " + array.length);
      for (int i = 0; i < array.length; i++)
        _dump(array[i], depth + 1);
    }
    else
    {
      _spaces(depth);
      System.out.println(o);
    }
  }

  static private void _spaces(int count)
  {
    int i = 0;
    for (; i + 5 < count; i += 5)
      System.out.print("     ");
    for (; i < count; i++)
      System.out.print(" ");
  }

  */

  static private final int _BUFFER_SIZE = 2048;
  static private final String _CACHED_SERIALIZED_VIEW =
    "org.apache.myfaces.trinidadinternal.renderkit.CACHED_SERIALIZED_VIEW";

  // Exclamation marks are not legit Base64 characters;  only
  // A-Z, a-z, 0-9, +, /, and = can ever show up.
  static private final String _TOKEN_PREFIX = "!";

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CoreResponseStateManager.class);
}
