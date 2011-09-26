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
package org.apache.myfaces.trinidadinternal.el;

import java.util.Iterator;
import java.util.NoSuchElementException;


/**
 * converts a EL expression into tokens.
 */
public class Tokenizer implements Iterator<Tokenizer.Token>
{
  /**
   * Creates a new Tokenizer
   * @param expression the expression to tokenize
   */
  public Tokenizer(String expression)
  {
    _exp = expression;
    _curToken = _next();
  }

  public void remove()
  {
    throw new UnsupportedOperationException();
  }
    
  public boolean hasNext()
  {
    return _curToken != null;
  }
    
  public Tokenizer.Token next()
  {
    if (_curToken == null)
      throw new NoSuchElementException();
    Token tok = _curToken;
    _curToken = _next();
    return tok;
  }
    
  private Token _next()
  {
    int size = _exp.length();
    // if there are no more characters to consume then we cannot return any
    // tokens:
    if (_curIndex >= size)
      return null;
  
    final Token tok;
    // in the initial state everything is just text, and we are searching
    // for the beginning of an expression (searching for "#{" ):
    if (_state == _STATE_INIT)
    {
      int loc = _exp.indexOf("#{", _curIndex);
      if (loc < 0)
      {
        // if we do not find the beginning of an expression, then the entire
        // string is treated as one big text token:
        loc = size;
      }
      _state = _STATE_EXP_START;
      if (loc > _curIndex)
      {
        // there was some text before the beginning of the expression.
        // create a text token from that text:
        tok = new Token(TEXT_TYPE, _curIndex, loc);
        _curIndex = loc;
      }
      else
      {
        // there was no text before the beginning of the expression.
        // create a token for the expression-begin characters #{ :
        tok = _next();
      }
    }// _STATE_INIT
    else if (_state == _STATE_EXP_START)
    {
      // create a token for the expression-begin characters #{  :
      int end = _curIndex + 2; // skip over #{
      tok = new Token(EXP_START_TYPE, _curIndex, end);
      _curIndex = end;
      _state = _STATE_EXP;
    }
    else if (_state == _STATE_EXP)
    {
      // once we are inside an expression we can have:
      // 1. whitespace
      // 2. numbers
      // 3. alphas
      // 4. quotes
      // 5. operators
      // 6. end of expression character "}"
      int start = _curIndex;
      char ch = _exp.charAt(_curIndex++);
      if (Character.isWhitespace(ch))
       tok = _readWhiteSpace(start);
      else if (Character.isDigit(ch)) 
        tok = _readNumber(start);
      else if (Character.isJavaIdentifierStart(ch))
        tok = _readVar(start);
      else if ('}' == ch)
      {
        tok = new Token(EXP_END_TYPE, start, _curIndex);
        _state = _STATE_INIT;
      }
      else if (('"' == ch) || ('\'' == ch))
        tok = _readQuoted(start, ch);
      else // everything else is treated as an operator:
        tok = new Token(OPER_TYPE, start, _curIndex);
    } // _STATE_EXP
    else
      throw new AssertionError("invalid state:"+_state);
    
    
    return tok;
  }

  /**
   * creates a quote token.
   * @param start the start index of the quotation
   * @param quote the quote character that delimits this quotation.
   */
  private Token _readQuoted(int start, char quote)
  {
    // we start at start+1  since we want to skip over the starting
    // quotation delimiter:
    for(int end = start+1;;)
    {
      // search for the ending quotation character:
      end = _exp.indexOf(quote, end);
      if (end >= 0)
      {
        end++;
        // if the ending quotation character was preceded by a back-slash
        // then ignore it; it is escaped.
        if (_exp.charAt(end-2) == '\\')
        {
          continue;
        }
        _curIndex = end;
        return new Token(QUOTED_TYPE, start, end);
      }
      else
        throw _fatal("Cannot find matching quote", start);
    }
  }
  
  private RuntimeException _fatal(String message, int charPos)
  {
    return new IllegalArgumentException(message+". character position:"+charPos);
  }
    
  /**
   * Creates a token for variables
   * @param start the starting index of a variable usage
   */
  private Token _readVar(int start)
  {
    // we skip over the first character since we know that is an alpha already:
    for(int end=start+1;;)
    {
      // search for the end of this variable usage:
      end = _getLastLocOfAlpha(end);
      // if the variable was followed by a period, then append the next variable
      // as well, since this is part of a long dot separated variable usage like:
      // "a.b.c"
      if (_exp.charAt(end) == '.')
        end++;
      else
      {
        _curIndex = end;
        break;
      }
    }
    return new Token(VAR_TYPE, start, _curIndex);
  }
  
  private int _getLastLocOfAlpha(int start)
  {
    while(Character.isJavaIdentifierPart(_exp.charAt(start)))
      start++;
    return start;
  }
    
  /**
   * reads a number token.
   * @param start the starting index of a number
   */
  private Token _readNumber(int start)
  {
    // we can skip the first character since we know it is a number already:
    int end = _getLastLocOfDigit(start+1);
    // if this is a decimal number then append it to this token:
    if (_exp.charAt(end) == '.')
      end = _getLastLocOfDigit(end +1);
    _curIndex = end;
    return new Token(NUMBER_TYPE, start, end);
  }
    

  private int _getLastLocOfDigit(int start)
  {
    while(Character.isDigit(_exp.charAt(start)))
      start++;
    return start;
  }
    
  /**
   * creates a whitespace token
   * @param start the starting index of the first whitespace character
   */
  private Token _readWhiteSpace(int start)
  {
    while(Character.isWhitespace(_exp.charAt(_curIndex)))
    {
      _curIndex++;
    }
    return new Token(WHITE_SPACE_TYPE, start, _curIndex);
  }
    
  private final String _exp;
  private int _curIndex = 0;
  private int _state = _STATE_INIT;
  private Token _curToken = null;
  
  public final class Token
  {
    public final int type;
    private final int _start, _end;
    
    private Token(int type, int start, int end)
    {
      this.type = type;
      _start = start;
      _end = end;
    }
    
    public String getText()
    {
      return _exp.substring(_start, _end);
    }
  }
  
  
  private static final int _STATE_INIT = 100;
  private static final int _STATE_EXP_START = 200;
  private static final int _STATE_EXP = 300;
  
  
  /**
   * Identifies some plain text
   */
  public static final int TEXT_TYPE = 100;
  
  /**
   * Identifies the expression start string.
   * This string is two charaters long and is: "#{"
   */
  public static final int EXP_START_TYPE = 200;
  
  /**
   * Identifies the expression end character.
   * This is character is "}"
   */
  public static final int EXP_END_TYPE = 300;
  
  /**
   * Identifies a series of whitespace characters
   */
  public static final int WHITE_SPACE_TYPE = 400;
  
  /**
   * Identifies a quotation. 
   * Eg: "quotation", 'quotation', 'he said, "foo!"'  ,
   * "it's it". 
   * More examples:   "foo \" bar" ,  'foo \' bar'
   */
  public static final int QUOTED_TYPE = 500;
  
  /**
   * Identifies a variable usage.
   * Eg: "row", "row.name", "foo.bar.baz"
   */
  public static final int VAR_TYPE = 600;
  
  /**
   * identifies a number
   * eg:  "123", "321.321"
   */
  public static final int NUMBER_TYPE = 700;
  
  /**
   * Identifies an operator.
   * eg: ">", "[", "+", "?"
   */
  public static final int OPER_TYPE = 800;
  
}
