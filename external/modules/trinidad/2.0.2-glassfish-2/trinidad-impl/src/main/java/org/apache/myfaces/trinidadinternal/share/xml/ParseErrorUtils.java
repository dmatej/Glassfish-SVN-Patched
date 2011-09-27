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
package org.apache.myfaces.trinidadinternal.share.xml;

import java.util.logging.Level;

import org.xml.sax.Locator;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Error reporting utilities.  Many of these utility methods
 * are available in even more convenient form on BaseNodeParser.
 * <p>
 * @see org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser#getRequiredAttribute
 * @see org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser#logWarning
 * @see org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser#logError
 * @see org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser#logUnexpectedElement
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/ParseErrorUtils.java#0 $) $Date: 10-nov-2005.18:59:13 $
 */
public class ParseErrorUtils
{
  /**
   * Sends a message to the error log, automatically pulling
   * line and column information from the context
   * @param context the parsing context
   * @param message the string message, if any
   * @param e the associated exception, if any
   * @param verbosity the verbosity level of the problem
   */
  static public void log(
    ParseContext context,
    String       message,
    Exception    e,
    Level        verbosity,
    TrinidadLogger       log)
  {
    log.log(verbosity, getErrorMessage(context, message), e);
  }


  /**
   * Sends a message to the error log, including full
   * line and column information.
   * @param context the parsing context
   * @param message the string message, if any
   * @param e the associated exception, if any
   * @param line the line number
   * @param column the column number
   * @param verbosity the verbosity level of the problem
   */
  static public void log(
    ParseContext context,
    String       message,
    Exception    e,
    int          line,
    int          column,
    String       systemId,
    Level        verbosity,
    TrinidadLogger       log)
  {
    log.log(verbosity, _getErrorMessage(message, line, column, systemId), e);
  }


  /**
   * Creates an error message that will include line number,
   * column number, and other information from the parsing context.
   * @param context the parsing context
   * @param message a base message
   */
  static public String getErrorMessage(
    ParseContext context,
    String       message)
  {
    int line;
    int column;
    String systemId;

    Locator locator = context.getLocator();
    if (locator == null)
    {
      line   = -1;
      column = -1;
      systemId = null;
    }
    else
    {
      line = locator.getLineNumber();
      column = locator.getColumnNumber();
      systemId = locator.getSystemId();
    }

    return _getErrorMessage(message, line, column, systemId);
  }


  /**
   * Returns an error message.
   */
  static private String _getErrorMessage(
    String       message,
    int          line,
    int          column,
    String       systemId)
  {
    String parseMessage = null;

    if (systemId != null)
      parseMessage = systemId + ": ";

    String append;
    if ((line < 0) || (column < 0))
      append = "Parsing error: ";
    else
      append = ("Parsing error, line " + line +
                ", column " + column + ": ");
    if (parseMessage == null)
      parseMessage = append;
    else
      parseMessage += append;

    if (message != null)
      parseMessage = parseMessage + message;

    return parseMessage;
  }
}
