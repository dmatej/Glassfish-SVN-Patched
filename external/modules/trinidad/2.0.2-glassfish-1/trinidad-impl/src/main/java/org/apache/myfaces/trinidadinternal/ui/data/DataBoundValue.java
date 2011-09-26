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
package org.apache.myfaces.trinidadinternal.ui.data;

import java.lang.reflect.UndeclaredThrowableException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;


/**
 * DataBoundValue implements BoundValue to retrieve a DataObject
 * from the current rendering context, and perform a select on
 * that object.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/DataBoundValue.java#0 $) $Date: 10-nov-2005.18:56:30 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class DataBoundValue implements BoundValue
{
  /**
   * Creates a BoundValue bound to the context's "current data object".
   * This will return the equivalent of the following code:
   * <pre>
   *   return context.getCurrentDataObject().selectValue(context, select);
   * </pre>
   * @param select the select string
   */
  public DataBoundValue(
    Object select
    )
  {
    this(null, null, select);
  }


  /**
   * Creates a BoundValue bound to a named data object on the context.
   * This will return the equivalent of the following code:
   * <pre>
   *   return context.getDataObject(namespaceURI, localName).
   *                                   selectValue(context, select);
   * </pre>
   * If the select string is null, DataBoundValue will directly
   * return the named data object:
   * <pre>
   *   return context.getDataObject(namespaceURI, localName);
   * </pre>
   * @param namespaceURI the namespace of the DataObject to use
   * @param localName the name of the DataObject to use
   * @param select the select string
   */
  public DataBoundValue(
    String namespaceURI,
    String localName,
    Object select
    )
  {
    _namespace = namespaceURI;
    _localName = localName;
    _select   = select;
  }


  /**
   * Called to retrieve a value based on the current rendering
   * context.
   * @param context the rendering context
   */
  public Object getValue(
    UIXRenderingContext context
    )
  {
    if (context != null)
    {
      try
      {
        DataObject source = ((_localName == null)
                        ? context.getCurrentDataObject()
                        : context.getDataObject(_namespace, _localName));

        if (_select == null)
          return source;

        if (source != null)
        {
          return source.selectValue(context, _select);
        }
      }
      catch (RuntimeException re)
      {
        handleException(context, re);
      }
    }

    return null;
  }


  /**
   * Returns the select key.
   */
  public Object getSelect()
  {
    return _select;
  }

  /**
   * Returns the local name.
   */
  public String getLocalName()
  {
    return _localName;
  }

  /**
   * Returns the namespace.
   */
  public String getNamespaceURI()
  {
    return _namespace;
  }

  /**
   * Handles a Throwable;  the exception is swallowed after being logged.
   */
  static public void handleException(
    UIXRenderingContext context,
    Throwable throwable)
  {
    if (throwable == null)
      throw new NullPointerException("throwable is null");

    if (throwable instanceof ThreadDeath)
    {
      throw ((ThreadDeath)throwable);
    }
    else if (throwable instanceof RuntimeException)
    {
      handleException(context, (RuntimeException)throwable);
    }
    else
    {
      handleException(context, new UndeclaredThrowableException(throwable));
    }
  }

  /**
   * Handles a RuntimeException;
   * the exception is swallowed after being logged.
   */
  static public void handleException(
    UIXRenderingContext context,
    RuntimeException exception)
  {
    // Catch and log all exceptions.
    _LOG.severe(exception);
  }

  private String _namespace;
  private String _localName;
  private Object _select;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(DataBoundValue.class);
}
