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
package org.apache.myfaces.trinidadinternal.share.expl;

/**
 * ExpressionParser is the abstraction for parsing expressions and literal
 * values.  <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/expl/ExpressionParser.java#0 $) $Date: 10-nov-2005.19:00:13 $
 */
public abstract class ExpressionParser
{
  /**
   * Returns the parsed expression as a BoundValue or literal.
   * 
   * @param context   the binding context
   * @param text      the expression to parse
   * @param type      the type of the parsed literal
   *                  or the return type of the parsed bound value
   *
   * @return the parsed expression as a BoundValue or literal
   */
  public abstract Object parseExpression(
    ExpressionContext context,
    String         text,
    Class<?>       type) throws ExplException;

  /**
   * @param context   the binding context
   * @param attrURI   the attribute namespace
   * @param attrName  the attribute name
   * @param attrText  the attribute string value
   * @return true if the value for the specified attribute is not a literal
   * and needs to be parsed into a BoundValue by {@link #parseExpression}.
   */
  public abstract boolean isBinding(ExpressionContext context,
                                    String attrURI,
                                    String attrName,
                                    String attrText);

  /**
   * gets the name of this bindingParser
   */
  public abstract String getName();

  public static final String EL_EXPRESSION_PARSER_NAME = "el";
  public static final String DATA_EXPRESSION_PARSER_NAME = "data";
}
