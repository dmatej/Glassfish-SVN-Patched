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
package org.apache.myfaces.trinidadinternal.agent.parse;

import java.util.ArrayList;
import java.util.Iterator;
import java.text.ParseException;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * An implementation that encapsulates "Name/Version Name/version ..." string and
 * supports a match method for matching other names and versions
 * <p/>
 * Name String can contain .*'s wild char
 * //TODO: Check if Name String *really* needs to support wild char
 * <p/>
 * Version string can contain +'s or *'s at the end
 * "*" at the end of version implies an 5xx.xx.xx version
 * "+" at the end implies greater than version 5
 * E.g "ie/5* ie/5.5+"
 * <p/>
 * <p/>
 * the name can have wild char charactes (at the end)
 * the version string may contain wild char or "+" (at the end)
 *
 */
class NameVersion
{

  NameVersion(String value) throws ParseException
  {
    _entries = _parseEntries(value);
  }

  public double match(String name, VersionId version)
  {
    if (_entries == null)
      return 0;

    if (name == null)
      return 0;

    double score = 0;
    for (int i = 0; i < _entries.length; i++)
    {
      Object[] entry = (Object[]) _entries[i];
      if (entry[0] != null)
      {
        double score1 = _matchName((NameEntry) entry[0], name);
        if (score1 > 0)
        {
          if ((entry[1] == null) & (score1 >= score))
          {
            score = score1;
          }
          else
          {
            double score2 = _matchVersion((VersionEntry) entry[1], version);
            //a match only if the score is > 0
            if ((score2 > 0) && (score1 + score2 >= score))
              score = score1 + score2;
          }
        }
      }
    }

    return score;
  }


  /**
   * Parse the condtions into an an array of [name, version] pairs
   * Right now the parse routine is very forgiving.
   * It will parse strings like "name/version/something", "name///version"
   *
   * @param value
   * @return
   */
  private Object[] _parseEntries(String value)
          throws ParseException
  {

    if (value == null)
      return null;

    int length = value.length();
    char[] data = new char[length + 1];


    value.getChars(0, length, data, 0);
    data[length] = (char) 0x1000000; //untype-able ascii char;

    boolean inQuote = false;
    int start = 0;
    int i = 0;
    char ch = data[i];

    ArrayList<Object[]> entries = new ArrayList<Object[]>(5);
    VersionEntry vEntry = null;
    NameEntry head, curr;
    head = curr = new NameEntry();
    curr.next = curr;

    while (i < length + 1)
    {
      switch (ch)
      {
        case '\'':
          if (!inQuote)
            inQuote = true;
          else
          {
            inQuote = false;
            if (start != i)
              throw new ParseException(_LOG.getMessage("UNEXPECTED_BACKSLASH"), i);
          }

          ch = data[++i];
          break;
        case '\\':
          //skip the next char
          //Since we only support '.*', throw error
          ch = data[++i];
          if ((ch != '.') || (ch != '\\'))
            throw new ParseException(_LOG.getMessage("EXPECTED_PERIOD_OR_BACKSLASH"), i);
          ch = data[++i];
          break;
        case '.':
          if (start != i)
          {
            curr.next.text = new String(data, start, i - start);
            curr = curr.next;
            curr.next = new NameEntry();
          }

          //We only support '.*'
          ch = data[++i];
          if (ch != '*')
            throw new ParseException(_LOG.getMessage("EXPECTED_ASTERISK"), i);

          curr.next.text = _WILD_CHAR_STRING;
          curr = curr.next;
          curr.next = new NameEntry();
          ch = data[++i];
          start = i;
          break;
        case '/':
          if (start != i)
          {
            curr.next.text = new String(data, start, i - start);
            curr = curr.next;
            curr.next = new NameEntry();
          }

          ch = data[++i];
          start = i;
          if (inQuote)
          {
            while ((ch != '\'') && (ch != (char) 0x1000000))
              ch = data[++i];
          }
          else
          {
            // 2006-08-02: -= Simon Lessard =-
            //while (!Character.isSpace(ch) && (ch != (char) 0x1000000))
            while (!Character.isWhitespace(ch) && (ch != (char) 0x1000000))
              ch = data[++i];
          }

          if (i - start <= 0)
            throw new ParseException(_LOG.getMessage(
              "EXPECTING_CHAR"), i);

          vEntry = _parseVersion(new String(data, start, i - start), i);
          start = i;
          break;
        case ((char) 0x1000000):
          if (inQuote)
            throw new ParseException(_LOG.getMessage(
              "UNTERMINATED_QUOTE"), i);

          if (start != i)
          {
            curr.next.text = new String(data, start, i - start);
            curr = curr.next;
            curr.next = new NameEntry();
          }

          if (head.text != null)
          {
            NameEntry temp = curr.next;
            curr.next = null;
            Object[] obj = new Object[]{head, vEntry};
            curr = head = temp;
            curr.next = curr;
            entries.add(obj);
            vEntry = null;
          }
          i++;
          break;
        default:
          // 2006-08-02: -= Simon Lessard =-
          //if ((!inQuote) && (Character.isSpace(ch)))
          if ((!inQuote) && (Character.isWhitespace(ch)))
          {
            if (start != i)
            {
              curr.next.text = new String(data, start, i - start);
              curr = curr.next;
              curr.next = new NameEntry();
            }

            if (head.text != null)
            {
              NameEntry temp = curr.next;
              curr.next = null;
              Object[] obj = new Object[]{head, vEntry};
              curr = head = temp;
              curr.next = curr;
              entries.add(obj);
              vEntry = null;
            }
            //Skip space
            ch = data[++i];
            // 2006-08-02: -= Simon Lessard =-
            //while (Character.isSpace(ch))
            while (Character.isWhitespace(ch))
              ch = data[++i];
            start = i;
          }

          ch = data[++i];
          break;
      }
    }

    return entries.toArray(new Object[entries.size()]);
  }

  /**
   * @param value
   * @return parse and return Entry for Name. '.' is delimiter for version numbers.
   *         '+' or '*' are treated as special chars and are allowed only at the end
   *         of the version string
   */
  private VersionEntry _parseVersion(String value,
                                     int offset) throws ParseException
  {

    int length = value.length();
    char[] data = new char[length + 1];
    int i = 0;

    value.getChars(0, length, data, 0);
    data[length] = (char) 0x1000000; //untype-able ascii char;

    char ch = data[i];
    VersionEntry vEntry = new VersionEntry();
    while (i < length + 1)
    {
      switch (ch)
      {
        case ((char) 0x1000000):
          String version = value.substring(0, i);
          //if the version string was simpley "*" or "+"
          if (version.length() == 0)
            return null;
          vEntry._versionId = new VersionId(version);
          vEntry._type = VersionEntry.EXACT_TYPE;
          i++;
          break;
        case '*':
        case '+':
          //After '*' anything other than space is invalid
          //After '+' anything other than space is invalid
          version = value.substring(0, i);
          for (++i; i < length; i++)
          {
            // 2006-08-02: -= Simon Lessard =-
            //if (!Character.isSpace(ch))
            if (!Character.isWhitespace(ch))
              throw new ParseException(_LOG.getMessage(
                "UNEXPECTED_CHAR"), offset + i);
          }
          //if the version string was simpley "*" or "+"
          if (version.length() == 0)
            return null;
          vEntry._versionId = new VersionId(version);
          if (ch == '*')
            vEntry._type = VersionEntry.STAR_TYPE;
          else
            vEntry._type = VersionEntry.PLUS_TYPE;

          ///exit the for loop, last char is 0x1000000
          i++;
          break;
        default:
          ch = data[++i];

      }
    }

    return vEntry;
  }


  /**
   * Match and score the name with the NameEntry in this condition
   *
   * @param entry
   * @param matchValue
   * @return
   */
  private double _matchName(NameEntry entry,
                            String matchValue)
  {
    if (matchValue == null)
      return NO_MATCH;

    int strLen = matchValue.length();

    int len = 0;
    if (entry.text != _WILD_CHAR_STRING)
    {
      if (matchValue.indexOf(entry.text, len) == 0)
      {
        len = entry.text.length();
        if ((entry = entry.next) == null)
          return NAME_EXACT_MATCH;
      }
      else
        return NO_MATCH;
    }

    while (entry.next != null)
    {
      if (entry.text != _WILD_CHAR_STRING)
      {
        int index = matchValue.indexOf(entry.text, len);
        if (index == -1)
          return NO_MATCH;
        len = index + entry.text.length();
      }
      entry = entry.next;
    }

    if (entry.text != _WILD_CHAR_STRING)
    {
      if (!matchValue.endsWith(entry.text))
        return NO_MATCH;
    }

    return (NAME_STAR_MATCH * (((double) len) / ((double) strLen)));
  }


  /**
   * Match and score the name with the VersionEntry in this condition
   *
   * @param entry
   * @param version
   * @return
   */
  private double _matchVersion(VersionEntry entry,
                               VersionId version)
  {
    if (version == null)
      return NO_MATCH; //request has no version info, but entry has version requirement. Hence no match

    if (entry._type == VersionEntry.STAR_TYPE)
    {
      Iterator<String> vIterator1 = version.iterator();
      Iterator<String> vIterator2 = entry._versionId.iterator();

      //Check upto where both have values
      int matchedParts = 0;
      while ((vIterator1.hasNext()) && (vIterator2.hasNext()))
      {
        String part1 = vIterator1.next();
        String part2 = vIterator2.next();
        if (!part1.equals(part2))
          return NO_MATCH;
        matchedParts++;
      }

      //So far they match. Now if version condition has more
      //parts (sub versions), then it must be .0's
      while (vIterator2.hasNext())
      {
        if (!"0".equals(vIterator2.next()))
          return NO_MATCH;
      }

      //now check how many parts the VersionId has
      int totNoOfParts = matchedParts;
      while (vIterator1.hasNext())
      {
        totNoOfParts++;
        vIterator1.next();
      }


      return (VERSION_STAR_MATCH * matchedParts / totNoOfParts);
    }
    else if (entry._type == VersionEntry.PLUS_TYPE)
    {
      Iterator<String> vIterator1 = version.iterator();
      Iterator<String> vIterator2 = entry._versionId.iterator();

      //Check upto where both have values
      while ((vIterator1.hasNext()) && (vIterator2.hasNext()))
      {
        try
        {
          int v1 = Integer.parseInt(vIterator1.next());
          int v2 = Integer.parseInt(vIterator2.next());
          if (v1 > v2)
            return (VERSION_GE_MATCH * (entry._versionId.getVersion() / version.getVersion()));
          else if (v1 < v2)
            return NO_MATCH;
        }
        catch (NumberFormatException npe)
        {
          return NO_MATCH;
        }
      }

      //if we reach here they two number are equal so far
      //Check if remaining are .0's
      while (vIterator2.hasNext())
      {
        try
        {
          if (Integer.parseInt(vIterator2.next()) != 0)
            return NO_MATCH;
        }
        catch (NumberFormatException npe)
        {
          return NO_MATCH;
        }
      }

      return (VERSION_GE_MATCH * (entry._versionId.getVersion() / version.getVersion()));
    }
    else
    {
      //exact match
      Iterator<String> vIterator1 = version.iterator();
      Iterator<String> vIterator2 = entry._versionId.iterator();

      //Check upto where both have values
      while ((vIterator1.hasNext()) && (vIterator2.hasNext()))
      {
        String part1 = vIterator1.next();
        String part2 = vIterator2.next();
        if (!part1.equals(part2))
          return NO_MATCH;
      }

      while (vIterator1.hasNext())
      {
        if (!"0".equals(vIterator1.next()))
          return NO_MATCH;
      }

      while (vIterator2.hasNext())
      {
        if (!"0".equals(vIterator2.next()))
          return NO_MATCH;
      }

      return VERSION_EXACT_MATCH;
    }
  }


  //structure to store the parsed parts of a name
  static private class NameEntry
  {
    String text;
    NameEntry next;
  }

  static private class VersionEntry
  {
    static final int EXACT_TYPE = 0;
    static final int PLUS_TYPE = 1;
    static final int STAR_TYPE = 2;

    /* type EXACT_TYPE, PLUS_TYPE, STAR_TYPE*/
    int _type;
    VersionId _versionId;
  }

  private Object[] _entries;

  private final static char _WILD_CHAR = 0x00002A;
  private final static String _WILD_CHAR_STRING = String.valueOf(_WILD_CHAR);


  /**
   * Name match has a much higher weightage.
   * Any string/version with wild char __matches gets a slightly lower score
   * so an input string "xxx" __matches "xxx" and "xxx.*", but a "xxx" match
   * gets an higher score
   */
  private final static double NAME_EXACT_MATCH = 1000;
  private final static double NAME_STAR_MATCH = NAME_EXACT_MATCH - 1;

  private final static double VERSION_EXACT_MATCH = 10;
  private final static double VERSION_GE_MATCH = 1;
  private final static double VERSION_STAR_MATCH = VERSION_EXACT_MATCH - 1;

  public final static double NO_MATCH = 0x0000;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    NameVersion.class);
}
