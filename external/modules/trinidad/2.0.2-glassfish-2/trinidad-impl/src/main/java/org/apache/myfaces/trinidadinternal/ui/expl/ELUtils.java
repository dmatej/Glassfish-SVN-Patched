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
package org.apache.myfaces.trinidadinternal.ui.expl;

import java.util.List;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/expl/ELUtils.java#0 $) $Date: 10-nov-2005.18:56:27 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
class ELUtils
{

  public static void getFunctions(List<FunctionToken> functions,
                                  String expression,
                                  int startIndex, int endIndex)
  {

    for(int index=startIndex; index<endIndex;)
    {
      int elStart = expression.indexOf("${", index);
      if (elStart >= 0)
      {
        // elStart is the index of "${" so we need to skip two characters:
        int i = elStart+2;
        while (i<endIndex)
        {
          // search for a open paren or the closing } of the EL expression:
          int parenStart = _searchFor(expression, "(}", i, endIndex);
          if (parenStart >= 0)
          {
            // see if what we have found is the end of the EL expression:
            if (expression.charAt(parenStart) == '}')
            {
              index = parenStart+1;
              break;
            }

            // we found a paren, so search for a function just before that:
            _getFunction(functions, expression, i, parenStart);
            // and continue searching after the paren:
            i = parenStart + 1;
          }
          else // we haven't found the end of the EL expression, this is an error:
            break;
        }
      }

      // we don't want to throw an exception, since this prevents the real
      // ELExpression parser from evaluating and producing an error. so we will
      // just return the largest index possible and force the introspection to
      // end:
      //throw new ELException("missing '}' brace in:"+expression);
      index = endIndex;
    }
  }

  // searches expression for the first occurrence of a character in
  // targets. The search will begin at the specified start index, and end
  // before the specified end index
  static private int _searchFor(String expression, String targets, 
                                int start, int end)
  {
    int state = 0; //initially we are not inside quotes.
    // this will store the current quote character when we are inside quotes:
    char quoteChar = 0;

    for(int i=start; i<end; i++)
    {
      char ch = expression.charAt(i);
      switch(state)
      {
      case 0: // not inside quotes
        // see if the current character matches what we are looking for:
        if (targets.indexOf(ch) >= 0)
          return i;

        // see if the current character starts a quotation:
        if (_QUOTES.indexOf(ch) >= 0)
        {
          quoteChar = ch;
          state = 1; // now we are in quote mode
        }
        break;

      case 1: // inside quotes
        // if the current character is a '\' (the escape character) then we
        // must skip it and the next one:
        if (ch == '\\')
          i++; //skip the next character
        else if (ch == quoteChar) // is this the closing quotation mark?
          state = 0; // no longer in quote mode
        break;

      default:
        throw new IllegalStateException();
      }
    }
    return -1;
  }

  // gets a function name and prefix in the expression. searches backwards
  // from the location of the paren until the start index.
  static private void _getFunction(List<FunctionToken> functions,
                                   String expression, 
                                   int start, 
                                   int parenIndex)
  {
    // first see if there is a name part:
    int qnameIndex = _getQNameStartIndex(expression, start, parenIndex);
    if (qnameIndex > 0)
    {
      String qname = expression.substring(qnameIndex, parenIndex);
      // figure out the index of the ':' between the name and prefix:
      int colonIndex = qname.indexOf(':');
      final String prefix, name;
      // make sure that there is infact a colon:
      if (colonIndex > 0)
      {
        prefix = qname.substring(0, colonIndex);
        name = qname.substring(colonIndex+1);
      }
      else
      {
        prefix = "";
        name = qname;
      }

      FunctionToken func = new FunctionToken(prefix, name,
                                             qnameIndex, parenIndex);
      functions.add(func);
    }
  }

  /**
   * gets a function name (or prefix) searching backwards from endIndex
   * @param endIndex this index is excluded from the search. The search begins
   * at the previous character. endIndex is the index of an open-paren
   * @param minIndex keep searching backwards until this minimum index is
   * reached
   * @return the index of the first character in the name, or -1 if there was
   * none
   */
  static private int _getQNameStartIndex(String expression,
                                         int minIndex, int endIndex)
  {
    //ystem.out.println("searching for text in:"+expression.substring(minIndex, endIndex));

    for(int i=endIndex - 1; i>=minIndex; i--)
    {
      char ch = expression.charAt(i);
      // is this a character that could be part of a function QName:
      if (!_isQNameChar(ch))
      {
        // if it isn't then the QName starts at the character in front
        // of i:
        i++;
        // check to make sure there are some characters to return:
        return (i < endIndex) ? i : -1;
      }
      // keep going back until we find a character that cannot be part of a
      // function name or prefix
    }

    // if we got upto this point, then the whole search area forms one text:
    return minIndex;
  }

  // is this a character that could be part of a function QName?
  static private boolean _isQNameChar(char ch)
  {
    return 
      ((ch >= 'a') && (ch <= 'z')) ||
      ((ch >= 'A') && (ch <= 'Z')) ||
      ((ch >= '0') && (ch <= '9')) ||
      (ch == '_') ||
      (ch == ':');
  }

  static final class FunctionToken
  {
    public final String prefix, name;
    public final int startIndex, endIndex;
    public FunctionToken(String prefix, String name, int start, int end)
    {
      this.prefix = prefix;
      this.name = name;
      startIndex = start;
      endIndex = end;
    }
  }

  private static final String _QUOTES = "\"\'";

}
