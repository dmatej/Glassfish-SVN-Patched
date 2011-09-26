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
package org.apache.myfaces.trinidadinternal.skin;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * This parses a skin css file into namespace map and
 * selector/properties.
 */
public class SkinCSSParser
{
  public SkinCSSParser()
  {
  }

  public void parseCSSDocument(
    Reader in,
    SkinCSSDocumentHandler documentHandler)
  {

    try
    {
      CSSScanner scanner = new CSSScanner(in);
      documentHandler.startDocument();
      List<String> selectorList = null;

      // start scanning the document
      // return comments /* xxx */
      // return @rules, which end with ';'
      // return selectors, which end with a {
      // return properties, which end with a }
      int currentType = _nextIgnoreSpaces(scanner);

      while (currentType != CSSLexicalUnits.EOF)
      {
        if (currentType == CSSLexicalUnits.COMMENT)
          documentHandler.comment(scanner.getStringValue());
        else if (currentType == CSSLexicalUnits.AT_KEYWORD)
          documentHandler.atRule(scanner.getStringValue());
        else if (currentType == CSSLexicalUnits.LEFT_CURLY_BRACE)
        {
          documentHandler.startSelector();
          selectorList = _parseSelectorString(scanner.getStringValue());
        }
        else if (currentType == CSSLexicalUnits.RIGHT_CURLY_BRACE)
        {
          String properties = scanner.getStringValue();
          _handlePropertiesString(documentHandler, properties);
          if (selectorList == null)
          {
            if (_LOG.isWarning())
            {
              _LOG.warning("IGNORING_PROPERTIES_WITHOUT_SELECTOR", properties);
            }
          }
          documentHandler.endSelector(selectorList);
        }
        currentType = _nextIgnoreSpaces(scanner);
      }
    }
    finally
    {
      documentHandler.endDocument();
    }
  }

  /**
   * given a string that denotes the selectors in a css file, parse this
   * further into a list of selectors.
   * (the selectors are deliminated by commas)
   */
  private List<String> _parseSelectorString(
    String selectors)
  {
    // give a list of selectors, deliminated by commas, parse into a List.
    // loop thru each character until I get to a left bracket.
    if (selectors == null)
      return null;

    List<String> selectorList = new ArrayList<String>();

    // pull apart by commas
    // don't skip whitespace since whitespace means descendant selectors in css
    String[] selector = _splitString(selectors, ',', false);

    String trimmedSelector;
    for (int i=0; i < selector.length; i++)
    {
      // the first selector might have extra }
      // this is a common typo, to have extra }s.
      if (i == 0)
      {
        trimmedSelector = _trimChar(selector[i].trim(), '}');
      }
      else
      {
        trimmedSelector = selector[i].trim();
      }
      // skip the selector if it is empty
      if ("".equals(trimmedSelector))
      {
        if (_LOG.isWarning())
        _LOG.warning("ERR_PARSING_SKIN_SELECTOR", selectors);
      }
      else
        selectorList.add(trimmedSelector);
    }
    
    return selectorList;
  }

  /**
   * given a string that denotes the properties of one or more selectors,
   * parse further into name/value pairs and call documentHandler's property
   * callback.
   */
  private void _handlePropertiesString(
    SkinCSSDocumentHandler documentHandler,
    String properties)
  {
    if (properties == null)
      return;

    // first, parse out any comments
    Matcher matcher = _COMMENT_PATTERN.matcher(properties);
    properties = matcher.replaceAll("");
    // split into name and value (don't skip whitespace since properties like padding: 0px 5px
    // need the spaces)
    String[] property = _splitString(properties, ';', false);
    
    for (int i=0; i < property.length; i++)
    {
      int indexOfColon = property[i].indexOf(':');
      if ((indexOfColon > -1) && (indexOfColon < property[i].length()))
      {
        String name = property[i].substring(0, indexOfColon);
        String value = property[i].substring(indexOfColon+1);
        documentHandler.property(name.trim(), value.trim());

      }
    }
  }

  /**
   * return the array of strings computed by splitting this string
   * around matches of the given character
   * @param in
   * @param charDelimiter
   * @param skipWhitespace if true, whitespace is skipped and not included in the return Strings
   * in the String array.
   * @return String[] The array of Strings computed by splitting the input String
   * around matches of the charDelimiter
   */
  private static String[] _splitString (
    String  in,
    char    charDelimiter,
    boolean skipWhitespace)
  {
    // return a String[] with each piece that is deliminated by the inChar.
    int length = in.length();
    StringBuffer buffer = new StringBuffer(length);
    List<String> splitList = new ArrayList<String>();
    
    for (int i=0; i < length; i++)
    {
      char c = in.charAt(i);
      if (c == charDelimiter)
      {
        // we hit the delimiter, so put it in the splitList and start a new buffer.
        splitList.add(buffer.toString());
        buffer = new StringBuffer(length);
      }
      else
      {
        // it's ok to put the character in the buffer if we don't want to skip whitespace
        // or if it isn't whitespace to begin with.
        if (!skipWhitespace || !(Character.isWhitespace(c)))
          buffer.append(c);
      }
      
    }
    // we are done with all the characters
    String lastString = buffer.toString();
    if (lastString.length() > 0)
      splitList.add(lastString);
    return splitList.toArray(_EMPTY_STRING_ARRAY);
  }
  
  private static String _trimChar (
    String in,
    char   c)
  {
    int len = in.length();
    char currentChar = in.charAt(0);
    if (currentChar != c)
      return in;

    for (int i=1; i < len; i++)
    {
      currentChar = in.charAt(i);
      if (currentChar != c)
      {

        return in.substring(i);
      }
    }
    return in;

  }

  // ignores spaces.
  private int _nextIgnoreSpaces(CSSScanner scanner)
  {
    int currentType = scanner.getNextToken();
    while (currentType == CSSLexicalUnits.SPACE)
    {
      currentType = scanner.getNextToken();
    }
    return currentType;
  }



  // This class builds up tokens for SPACE, COMMENT, AT_RULE,
  // LEFT_CURLY_BRACE (selectorList), and RIGHT_CURLY_BRACE (properties)
  // A token is stored in the _buffer object.
  private static class CSSScanner
  {
    public CSSScanner(Reader reader)
    {
      _reader = reader;
    }

    public String getStringValue()
    {
      if (_end <= 0)
        return null;
      else
        return new String(_buffer, 0, _end);
    }


    // get the next token in the buffer and return the type
    public int getNextToken()
    {
      _position = 0;
      _fillToken();

      _end = _position;

      // strip off the final brace if needed
      if (_type == CSSLexicalUnits.RIGHT_CURLY_BRACE ||
          _type == CSSLexicalUnits.LEFT_CURLY_BRACE)
        _end--;

      if (_currentChar == -1)
        return CSSLexicalUnits.EOF;
      return _type;
    }



    private void _fillToken()
    {
      while (true)
      {
        _nextChar();
        switch (_currentChar)
        {
          case -1:
            _type = CSSLexicalUnits.EOF;
            break;


          case ' ':
          case '\t':
          case '\n':
          case '\f':
          case '\r':
            if (_type != CSSLexicalUnits.LEFT_CURLY_BRACE)
            {
              _type = CSSLexicalUnits.SPACE;
              return;
            }
            // fall through to LEFT_CURLY_BRACE


          case '/':
            if (_type != CSSLexicalUnits.LEFT_CURLY_BRACE)
            {
              // check for comment. If it is a comment, set the type and return
              // if it isn't a comment, keep looping to get more characters.
              _nextChar();
              if (_currentChar == '*')
              {
                // WE ARE IN A COMMENT
                // loop and get characters into buffer until we get '*/'
  
                _nextChar();
                int prevChar;
                while (_currentChar != -1)
                {
  
                  prevChar = _currentChar;
                  _nextChar();
                  if ((prevChar == '*') && (_currentChar == '/'))
                    break;
                }
  
                _type = CSSLexicalUnits.COMMENT;
                return;
  
              }
              // wasn't a comment, so keep going on, filling the buffer with
              // each _nextChar call.
              break;
            }



          case '@':
            if (_type != CSSLexicalUnits.LEFT_CURLY_BRACE)
            {
              // found @.
              // @namespace is treated differently than other @rules.
              // These are the formats:
              // @namespace foo url(http://www.foo.com);
              // @agent {
              //    af|inputText::content{color:red; background-color:blue;}
              // }
              // @platform {...}
              // If @namespace, go 'til the semi-colon
              // Else, go until the start/end brace match.

              // found @. keep getting characters until we get a ; or end of file.
              /*
              _nextChar();

              while ((_currentChar != -1) && (_currentChar != ';'))
              {
                _nextChar();
              }
              */
              _nextChar();
              // go until ; or until {} match
              int openBraceCount = 0;
              boolean openBraceCountStarted = false;
              while ((_currentChar != -1))
              {

                if (_currentChar == '{')
                  openBraceCount++;
                if (openBraceCount == 1)
                  openBraceCountStarted = true;
                if (_currentChar == '}' && openBraceCountStarted)
                {
                  openBraceCount--;
                  if (openBraceCountStarted && openBraceCount == 0)
                  {
                    break;
                  }
                }
                if (_currentChar == ';' && openBraceCount==0)
                {
                  break;
                }
                _nextChar();

              }
             
  
              _type = CSSLexicalUnits.AT_KEYWORD;
              return;
            }

          default:
            if (_type == CSSLexicalUnits.LEFT_CURLY_BRACE)
            {
              // these are the properties,
              // keep going until we have all the properties
              while ((_currentChar != -1) && (_currentChar != '}'))
              {
                _nextChar();
              }
              _type = CSSLexicalUnits.RIGHT_CURLY_BRACE;
            }
            else
            {
              while ((_currentChar != -1) && (_currentChar != '{'))
              {
                _nextChar();
              }
              _type = CSSLexicalUnits.LEFT_CURLY_BRACE;
            }
            return;

        } // end switch



        if (_currentChar == -1)
        {
          _type = CSSLexicalUnits.EOF;
          return;
        }
      }

    }


    // fill buffer with one more character
    private void _nextChar()
    {
      try
      {
        _currentChar = _reader.read();
      }
      catch (IOException e)
      {
        if (_LOG.isSevere())
        {
          _LOG.severe("ERR_READING_SKIN_CSS_FILE", e);
        }
        _currentChar = -1;
        return;
      }
      // need to make sure buffer doesn't go over its size
      if (_buffer.length <= _position)
      {
        // increase buffer size by 50%
        char[] tmp = new char[_buffer.length + (_buffer.length/2)];
        // copy over buffer to new buffer
        for (int i=0; i < _buffer.length; i++)
          tmp[i] = _buffer[i];
        // pt _buffer to bigger buffer.
        _buffer = tmp;

      }

      _buffer[_position++] = (char)_currentChar;

    }

    private Reader _reader;
    // buffer parameters
    private char[] _buffer = new char[1024];
    private int    _position;
    private int    _end;
    // type of token (it will be a CSSLexicalUnits constant)
    private int _type;
    // current character. -1 means EOF
    private int _currentChar;
  }

  /**
   * constants that we use to keep track of what type of token of the css file
   * we have parsed.
   */
  private static class CSSLexicalUnits
  {
    public static final int EOF = 0;
    public static final int LEFT_CURLY_BRACE = 1;
    public static final int RIGHT_CURLY_BRACE = 2;
    public static final int SPACE = 3;
    public static final int COMMENT = 4;
    public static final int AT_KEYWORD = 5;
  }

  // this is the pattern for finding comments. We want to strip out
  // comments from the properties, and we use this pattern to do it.
  private static final Pattern  _COMMENT_PATTERN =
     Pattern.compile("(?s)/\\*.*?\\*/");
     
  private static final String[] _EMPTY_STRING_ARRAY = new String[0];

     
  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(SkinCSSParser.class);


}
