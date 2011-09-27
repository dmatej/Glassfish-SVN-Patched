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

// External variables used:
//  _df2DYS: Sets the two-digit year start.

var _AD_ERA = null;


function _getADEra()
{
  if (_AD_ERA == null)
  {
    _AD_ERA = new Date(0);
    _AD_ERA.setFullYear(1);
  }
  
  return _AD_ERA;
}


/**
 * Determine whether the parsed time is a strictly parsed value.
 */
function _isStrict(
  parseContext,
  parsedTime)
{
  var checks = ["FullYear", "Month", "Date", "Hours", "Minutes",
                "Seconds", "Milliseconds"];

  for (var i=0; i < checks.length; i++)
  {
    var parsed = "parsed" + checks[i];

    if (parseContext[parsed] != null &&
        parseContext[parsed] != parsedTime["get" + checks[i]]())
    {
      // failure for strict parsing
      return false;
    }
  }

  return true;
}


/**
 * Clump up similar runs of pattern characters from the format patter and
 * call the subfunction for each result.  Return whether the clumping
 * succeeded.
 */
function _doClumping(
  formatPattern,
  localeSymbols,
  locale,
  subFunction,
  param,
  outValue
  )
{  
  var formatLength = formatPattern.length;
  var inQuote      = false;
  var kindCount    = 0;
  var lastChar     = void 0;
  var startIndex   = 0;
  var quoteIndex = null;
  
  for (var i = 0; i < formatLength; i++)
  {
    var currChar = formatPattern.charAt(i);
    
    if (inQuote)
    {
      if (currChar == "\'")
      {
        // test strings to test escaping working properly
        // "'one '' two '' three''' 'four '' five'"   ->     "one ' two ' three' four ' five
        // "HH:mm:ss 'o''clock' z"                    ->     "[time] o'clock [timezone]"
        // "HH:mm:ss 'oclock' z"                      ->     "[time] oclock [timezone]"
        // "HH:mm:ss '' z"                            ->     "[time] ' [timezone]"
        
        inQuote = false;
        
        // handle to single quotes in a row as escaping the quote
        // by not skipping it when outputting
        if (kindCount != 1 && startIndex != quoteIndex)
        {
          startIndex++;
          kindCount--;
        }

        // output the quoted text
        if (!subFunction(formatPattern,
                         localeSymbols,
                         locale,
                         "\'",
                         startIndex,
                         kindCount,
                         param,
                         outValue))
        {
          // failure
          // alert("failure at " + startIndex + " with " + lastChar);
          return false;
        }
               
        var nextIndex = i + 1;
    
        if (nextIndex < formatLength)
        {
          var nextChar = formatPattern.charAt(nextIndex);
          
          if (nextChar == "\'")
          {
            quoteIndex = nextIndex;
          }
        } 
        
        kindCount = 0;
        lastChar  = void 0; 
      }
      else
      {
        // keep adding characters to the escaped String
        kindCount++;
      }
    }
    else
    {
      // the characters that we are collecting have changed
      if (currChar != lastChar)
      {
        if (kindCount != 0)
        {       
          // output the previously collected string
          if (!subFunction(formatPattern,
                           localeSymbols,
                           locale,
                           lastChar,
                           startIndex,
                           kindCount,
                           param,
                           outValue))
          {
            // failure
            //alert("failure at " + startIndex + " with " + lastChar);
            return false;
          }
          
          kindCount = 0;
          lastChar  = void 0;
        }
        
        if (currChar == '\'')
        {
          inQuote = true;
        }
  
        startIndex = i;      
        lastChar = currChar;
      }

      // keep collecting this kind of character together
      kindCount++;
    }   
  }
  
  // output any left over substring being collected
  if (kindCount != 0)
  {
    if (!subFunction(formatPattern,
                     localeSymbols,
                     locale,
                     lastChar,
                     startIndex,
                     kindCount,
                     param,
                     outValue))
    {
      // failure
      //alert("failure at " + startIndex + " with " + lastChar);
      return false;
    }
  }
  
  // success
  return true;
}


/**
 * Format a clump of pattern elements using the specified time.
 */
function _subformat(
  inString,
  localeSymbols,
  locale,
  formatType,
  startIndex,
  charCount,
  time,
  stringHolder
  )
{    

  // string to append to the toString
  var appendString = null;
  
  var amPMAdjust = false;
    
  if ((formatType >= 'A') && (formatType <= 'Z') ||
      (formatType >= 'a') && (formatType <= 'z'))
  {
    switch (formatType)
    {
      case 'D': // day in year
        appendString = "(Day in Year)";
        break;
      
      case 'E': // day in week
      {
        var dayOfWeek = time.getDay();
        
        appendString = (charCount <= 3)
                         ? localeSymbols.getShortWeekdays()[dayOfWeek]
                         : localeSymbols.getWeekdays()[dayOfWeek];
      }
      break;
      
      case 'F': // day of week in month
        appendString = "(Day of week in month)";
        break;
      
      case 'G': // era designator
      {
        var eras = localeSymbols.getEras();
        
        appendString = (time.getTime() < _getADEra().getTime())
                         ? eras[0]
                         : eras[1];
 
      }
      break;
                  
      case 'M': // month in year
      {
        var monthIndex = time.getMonth();
        
        if (charCount <= 2)
        {
          // use the month number
          appendString = _getPaddedNumber(monthIndex + 1, charCount);
        }
        else if (charCount == 3)
        {
          // use the month abbreviation
          appendString = localeSymbols.getShortMonths()[monthIndex];
        }
        else
        {
          // use the full month name
          appendString = localeSymbols.getMonths()[monthIndex];
        }
      }
      break;
      
      case 'S': // millisecond (0 - 999)
        appendString = _getPaddedNumber(time.getMilliseconds(), charCount);
        break;
      
      case 'W': // week in month
        appendString = "(Week in Month)";
        break;
      
      case 'a': // am/pm marker
      {
        var amPMs = localeSymbols.getAmPmStrings();
        
        appendString =  (_isPM(time.getHours()))
                          ? amPMs[1]
                          : amPMs[0];
      }
      break;
      
      case 'd': // day in month
        appendString = _getPaddedNumber(time.getDate(), charCount);
        break;
      
      case 'h': // hour in am/pm (1-12)
        hours = time.getHours();
        
        if (_isPM(hours))
          hours -= 12;
        
        if (hours == 0)
          hours = 12;

        appendString = _getPaddedNumber(hours, charCount);
        break;
      
      case 'K': // hour in am/pm (0-11)
         hours = time.getHours();
        
        if (_isPM(hours))
           hours -= 12;
         
        appendString = _getPaddedNumber(hours, charCount);
        break;

      case 'k': // hour in day (1-24)
        hours = time.getHours();
        if (hours == 0)
          hours = 24;
        appendString = _getPaddedNumber(hours, charCount);
        break;

      case 'H': // hour in day (0-23)
        appendString = _getPaddedNumber(time.getHours(), charCount);
        break;


      case 'm': // minute in hour 0 - 59)
        appendString = _getPaddedNumber(time.getMinutes(), charCount);
        break;
 
      case 's': // seconds in minute 0 - 59)
        appendString = _getPaddedNumber(time.getSeconds(), charCount);
        break;
   
      case 'w': // week in year
        appendString = "(Week in year)";
        break;
      
      case 'y': // year
      {
        var year = time.getFullYear();
        
        // Trinidad-2013: Thai Buddhist Calendar is offset by 543 years
        if (locale == "th_TH")
          year += 543;
        
        // truncate 2 and 1 digit years to that number of digits
        var maxDigits = (charCount <= 2)
                          ? charCount
                          : null;
                          
                          
        appendString = _getPaddedNumber(year, charCount, maxDigits);
      }
      break;
        
        
      case 'z': // GMT timezone - "GMT Sign Hours : Minutes"
      {
        appendString = "GMT";
        
        var tzString = _getTimeZoneOffsetString(time, false);
        if (tzString)
        {
          // +/-HH:mm
          appendString += tzString[0];
          appendString += ":"
          appendString += tzString[1];
        }
      }
      break;
      
      case 'Z': // RFC 822 timeZone - "Sign TwoDigitHours Minutes"
      { 
        var tzString = _getTimeZoneOffsetString(time, true);
        if (tzString)
        {
          // +/-HHmm
          appendString = tzString[0];
          appendString += tzString[1];
        }
        else
        {
          appendString = "";
        }
      }
      break;
    
      default:
        // do nothing rather than throw an exception
        appendString = "";
    }
  }
  else
  {
    // all other results are literal
    appendString = inString.substring(startIndex, startIndex + charCount);
  }
  
  stringHolder.value += appendString;
  
  // formatting should never fail
  return true;
}

/**
 * Returns the timeZone offset from GMT in the array:
 * [0] = "+/-HH", [1] = "mm".
 */
function _getTimeZoneOffsetString(time, rfcFormat)
{

  // timeZoneOffset in javascript gives
  // the wrong sign, so I am switching it.
  var timeZnOffset = -1* time.getTimezoneOffset();
  timeZnOffset += _getLocaleTimeZoneDifference();
  
  if(rfcFormat || timeZnOffset != 0)
  {
    var timeOffsetString = new Array(2);
    // sign
    if (timeZnOffset < 0)
    {
      timeOffsetString[0] = "-";
      // abs value
      timeZnOffset = -timeZnOffset
    }
    else
    {
      timeOffsetString[0] = "+";
    }
    
    // HH
    timeOffsetString[0] += _getPaddedNumber(Math.floor(timeZnOffset / 60), 2);
    
    // mm
    timeOffsetString[1] = _getPaddedNumber(timeZnOffset % 60, 2);
    
    return timeOffsetString;
  }
}

/**
 * compare the time zone that is on the client with the time zone that
 * came from the localeContext on the server, and return the difference
 * in hours.
 * This can be used to adjust the date/time value that will be displayed in
 * the date field to use the timezone set on the locale context on the 
 * server instead of the timezone we get from javascript's getTimezoneOffset.
 * see bug 3167883
 */
function _getLocaleTimeZoneDifference()
{
  var currentDate = new Date();
  // timeZoneOffset in javascript appears to give
  // the wrong sign, so I am switching it.
  // the timeZoneOffset is in minutes.
  var currentDateTzOffset = currentDate.getTimezoneOffset() * -1;
  var tzOffsetDiff = 0;
  
  return tzOffsetDiff - currentDateTzOffset;
}

/**
 * Parse a substring using a clump of format elements.
 */
function _subparse(
  inString,      // the pattern string, such as "yyMMdd"
  localeSymbols,
  locale,
  formatType,    // the current format char, such as 'y'
  startIndex,    // index into inString
  charCount,     // the number of chars of type formatType
  parseContext,  // information pertaining to the user input string
  parsedTime
  )
{
  // Start index of the string being parsed (as opposed
  // to startIndex, which is the index on the format mask)
  var inStartIndex = parseContext.currIndex;
    
  var nextFormatType = (startIndex + charCount < inString.length) ? 
    inString.charAt(startIndex + charCount) : null;
    
  // Consider the pattern "yyMMdd". Say that formatType is 'y' and nextFormatType is 'M'. Normally 
  // we would allow for leniency such that the user could input 2 or 4 digits for the year, but 
  // since this pattern contains no date separators and both the year and month can consist of 
  // digits, there's no easy way of telling whether the first 4 digits apply to just the year, or 
  // to both the year and month. Therefore, if nextFormatType is one of the reserved format types, 
  // then we go into strict parsing mode for formatType, where charCount represents the maximum 
  // number of user input characters that will be parsed when matching the current formatType.
  var isStrict = ("DFMSWdhkHKmswy".indexOf(nextFormatType) != -1);

  if ((formatType >= 'A') && (formatType <= 'Z') ||
      (formatType >= 'a') && (formatType <= 'z'))
  {
    switch (formatType)
    {
      case 'D': // day in year
        // skip this number
        if (_accumulateNumber(parseContext, !isStrict ? 3 : charCount) == null)
        {
          return false;
        }
        break;
      
      case 'E': // day in week
      {
        // extract the day but do nothing with it, as there is not setDay()
        // on Date
        var dayIndex = _matchArray(parseContext,
                                   (charCount <= 3)
                                     ? localeSymbols.getShortWeekdays()
                                     : localeSymbols.getWeekdays());
                                     
        if (dayIndex == null)
        {
          return false;
        }
      }
      break;
      
      case 'F': // day of week in month
        // skip this number
        if (_accumulateNumber(parseContext, !isStrict ? 2 : charCount) == null)
        {
          return false;
        }
        break;
      
      case 'G': // era designator
      {
        var eraIndex = _matchArray(parseContext, localeSymbols.getEras());
        
        if (eraIndex != null)
        {
          if (eraIndex == 0)
          {
            parseContext.parsedBC = true;
          }
        }
        else
        {
          return false;
        }
      }
      break;
                  
      case 'M': // month in year
      {
        var monthIndex;
        var monthOffset = 0;

        if (charCount <= 2)
        {
          // match month number
          monthIndex = _accumulateNumber(parseContext, !isStrict ? 2 : charCount);
          
          // subtract 1 from the monthIndex to make it 0-based
          monthOffset = -1;
        }
        else
        {
          var nameArray = (charCount == 3)
                            ? localeSymbols.getShortMonths()
                            : localeSymbols.getMonths();

          monthIndex = _matchArray(parseContext, nameArray);
        }

        if (monthIndex != null)
        {
          parseContext.parsedMonth = (monthIndex + monthOffset);
        }
        else
        {
          return false;
        }
      }
      break;

      case 'S': // millisecond (0 - 999)
      {
        var milliseconds = _accumulateNumber(parseContext, !isStrict ? 3 : charCount);

        if (milliseconds != null)
        {
          parseContext.parsedMilliseconds = milliseconds;
        }
        else
        {
          return false;
        }
      }
      break;
      
      case 'W': // week in month
        // skip this number
        if (_accumulateNumber(parseContext, !isStrict ? 2 : charCount) == null)
        {
          return false;
        }
        break;
      
      case 'a': // am/pm marker
      {
        var amPMIndex = _matchArray(parseContext,
                                    localeSymbols.getAmPmStrings());
        
        if (amPMIndex == null)
        {
          return false;
        }
        else
        {
          if (amPMIndex == 1)
          {
            parseContext.isPM = true;
          }
        }
      }
      break;
      
      case 'd': // day in month
      {
        var dayOfMonth = _accumulateNumber(parseContext, !isStrict ? 2 : charCount);
                
        if (dayOfMonth != null)
        {
          parseContext.parsedDate = dayOfMonth;
        }
        else
        {
          return false;
        }
      }
      break;
        
      case 'h': // hour in am/pm (1-12)
      case 'k': // hour in day (1-24)
      case 'H': // hour in day (0-23)
      case 'K': // hour in am/pm (0-11)
      {
        var hour = _accumulateNumber(parseContext, !isStrict ? 2 : charCount);
        
        if (hour != null)
        {
          if ((formatType == 'h') && (hour == 12))
            hour = 0;
          if ((formatType == 'k') && (hour == 24))
            hour = 0;

          parseContext.parsedHour = hour;
        }
        else
        {
          return false;
        }
      }
      break;


      case 'm': // minute in hour 0 - 59)
      {
        var minutes = _accumulateNumber(parseContext, !isStrict ? 2 : charCount);
        
        if (minutes != null)
        {
          parseContext.parsedMinutes = minutes;
        }
        else
        {
          return false;
        }
      }
      break;
      
      case 's': // seconds in minute 0 - 59)
      {
        var seconds = _accumulateNumber(parseContext, !isStrict ? 2 : charCount);

        if (seconds != null)
        {
          parseContext.parsedSeconds = seconds;
        }
        else
        {
          return false;
        }
      }
      break;

      case 'w': // week in year
        // skip this number
        if (_accumulateNumber(parseContext, !isStrict ? 2 : charCount) == null)
        {
          return false;
        }
        break;

      case 'y': // year
      {
        var year = _accumulateNumber(parseContext, !isStrict ? 4 : charCount);
        var enteredChars = parseContext.currIndex - inStartIndex;
        // if we have a 2-digit year, add in the default year
        if (year != null)
        {
          if ((enteredChars > 2) &&
              (charCount <= 2) &&
              (year <= 999))
          {
            // Block bonus characters;  if they've specified
            // a two-year mask, and there's more than two characters,
            // there might be a problem.  But allow four digits.
            return false;
          }
          else if ((charCount <= 2) && (year >= 0) && (year <= 100))
          {
            year = _fix2DYear(year);
          }
          else if (charCount == 4)
          {
            // Bug 2169562: For four-digit year formats, reject
            // three-year entries.  Fair enough!
            if (enteredChars == 3)
              return false;    
            if (enteredChars <= 2)
              year = _fix2DYear(year);
          }

          // There is no year "0"
          if (year == 0)
            return false;
            
          // Trinidad-2013: Thai Buddhist Calendar is offset by 543 years
          if (locale == "th_TH")
            year -= 543;
            
          parseContext.parsedFullYear = year;
        }
        else
        {
          return false;
        }
      }
      break;
        
      case 'z': // GMT timezone - "GMT Sign Hours : Minutes"
      {
        // consume the GMT portion
        if (!_matchText(parseContext, "GMT"))
        {
          // GMT is must for timeZone entry.
          return false;
        }
        
        // if we have any more chars then parse the remaining "+HH:mm" string.
        if( (parseContext.parseString.length - parseContext.currIndex) > 0)
        {
          // consume the plus or minus
          if(_matchArray(parseContext, ["-", "+"]) == null)
          {
            return false;
          }
          
          // accumulate the hour offset number
          var hourOffset = _accumulateNumber(parseContext, 2);
          if(hourOffset == null)
          {
            return false;
          }
          parseContext.hourOffset = hourOffset;
          
          // consume the separator between HH and mm
          if (!_matchText(parseContext, ":"))
          {
            return false;
          }
          
          // accumulate minute offset number (should have 2 digits)
          var minOffset;
          if(((parseContext.parseString.length - parseContext.currIndex) < 2) ||
             (minOffset = _accumulateNumber(parseContext, 2)) == null)
          {
            return false;
          }
          parseContext.minOffset = minOffset;
        }
      }
      break;
      
      case 'Z': // RFC 822 timezone - "Sign TwoDigitHours Minutes"
      {
        // RFC 822 TimeZone format should have 5 chars (+/-HHmm)
        if ((parseContext.parseString.length - parseContext.currIndex) < 5)
        {
          return false;
        }
        
        // consume the plus or minus
        if(_matchArray(parseContext, ["-", "+"]) == null)
        {
          return false;
        }
          
        // accumulate the hour offset number
        var hourOffset = _accumulateNumber(parseContext, 2)
        if(hourOffset == null)
        {
          return false;
        }
        parseContext.hourOffset = hourOffset;
        
        // accumulate the minute offset number
        var minOffset = _accumulateNumber(parseContext, 2)
        if(minOffset == null)
        {
          return false;
        }
        parseContext.minOffset = null;
      }
      break;
    
      default:
    }
  }
  else
  {
    // consume constants
    return _matchText(parseContext,
                      inString.substring(startIndex, startIndex + charCount));
  }
  
  // match succeeded
  return true;
}


/**
 * Fix two-digit years.
 */
function _fix2DYear(year)
{
  var defaultCentury;

  if (_df2DYS != null)
  {
    // year               51    01
    // offsetYear       1950  1950
    // defaultCentury   1900  1900
    // year             1951  1901
    // year             1951  2001
    var offsetYear = _df2DYS;
    defaultCentury = offsetYear - (offsetYear % 100);

    year += defaultCentury;
    if (year < offsetYear)
      year += 100;
  }
  else
  {
    var currentYear = new Date().getFullYear();
    defaultCentury = currentYear - (currentYear % 100) - 100;

    year += defaultCentury;
 
    // if the new year is now more than 80 years in the past,
    // then it is actually a date in the future, so add the 100 years
    // back in.  The 80 years rule, matches Java's spec
    if (year + 80 < currentYear)
    {
      year += 100;
    }
  }

  return year;
}


/**
 * Match the current text against an array of possibilities, returning
 * the index of the succesful match, or undefined if no match succeeded.
 */
function _matchArray(
  parseContext,
  matchArray
  )
{
  for (var i = 0; i < matchArray.length; i++)
  {
    if (_matchText(parseContext, matchArray[i]))
    {
      return i;
    }
  }
  
  // no match
  return null;
}


/**
 * Match the specified text in a case insensitive manner,
 * returning true and updating the
 * <code>parseContext</code> if the match succeeded.
 */
function _matchText(
  parseContext,
  text
  )
{
  // if no text to match then match will fail
  if (!text)
    return false;

  // get the length of the text to match
  var textLength  = text.length;

  var currIndex   = parseContext.currIndex;
  var parseString = parseContext.parseString;
  
  // determine whether we have enough of the parseString left to match
  if (textLength > parseString.length - currIndex)
  {
    return false;
  }

  //
  // Convert to lowercase for case insensitive match
  //
  // =-= bts Maybe toLocaleLowerCase would be better, but that would cause
  //         problems if the browser locale were different from the application
  //         locale.
  //  
  var parseText  = parseString.substring(currIndex, currIndex + textLength);
  var parseMatch = parseText.toLowerCase();
  var textMatch  = text.toLowerCase();
  
  if (parseMatch != textMatch)
    return false;
    
  // update the current parseContext
  parseContext.currIndex += textLength;
  
  return true;
}
 

/**
 * Accumlates and returns a number at this location or undefined, if
 * there is no number.
 */
function _accumulateNumber(
  parseContext,
  maxLength
  )
{
  var startIndex  = parseContext.currIndex;
  var currIndex   = startIndex;
  var parseString = parseContext.parseString;
  var parseLength = parseString.length;
  if (parseLength > currIndex + maxLength)
    parseLength = currIndex + maxLength;

  var currValue = 0;

  // gather up all of the digits
  while (currIndex < parseLength)
  {
    var currDigit = parseDigit(parseString.charAt(currIndex));

    if (!isNaN(currDigit))
    {
      // add on the digit and shift over the results
      currValue *= 10;
      currValue += currDigit;

      currIndex++;
    }
    else
    {
      break;
    }
  }

  if (startIndex != currIndex)
  {
    // update the current parseContext
    parseContext.currIndex = currIndex;

    // return the numeric version
    return currValue;
  }
  else
  {
    // no number at this location
    return null;
  }
}


/**
 * Returns true if the hour index is considered PM.
 */
function _isPM(
  hours
  )
{
  return (hours >= 12);
}


/**
 * Pad out a number with leading 0's to meet the minDigits digits or
 * truncate to meet the minDigits.
 */
function _getPaddedNumber(
  number,
  minDigits,
  maxDigits
  )
{  
  var stringNumber = number.toString();
  
  //
  // pad out any number strings that are too short
  //
  if (minDigits != null)
  {    
    var addedDigits = minDigits - stringNumber.length;
  
    while (addedDigits > 0)
    {
      stringNumber = "0" + stringNumber;
      addedDigits--;
    }
  }
  
  //
  // truncate any number strings that are too long
  //
  if (maxDigits != null)
  {
    var extraDigits = stringNumber.length - maxDigits;
    
    if (extraDigits > 0)
    {
      stringNumber = stringNumber.substring(extraDigits,
                                            extraDigits + maxDigits);
    }
  }
  
  return stringNumber;
}


/**
 * External variable for TrDateTimeConverter. Maps locales to lists of 
 * convenience patterns.
 */
var _CONVENIENCE_PATTERNS = null;

/**
 * Construct a TrDateTimeConverter with the specifed date pattern for
 * the specified locale.
 */
function TrDateTimeConverter(
  pattern,  
  locale,
  exampleString,
  type,
  messages
  )
{

  // for debugging
  this._class = "TrDateTimeConverter";
  this._exampleString = exampleString;
  this._type = type;
  this._messages = messages;
  this._offset = null;
  
  // save the Locale elements for the specified locale, or client locale
  // if no locale is specified
  this._localeSymbols = getLocaleSymbols(locale);

  // =-= bts need to change default pattern to match JDK
  if (pattern == null)
    pattern = this._localeSymbols.getShortDatePatternString();

  var patterns = this._initPatterns(pattern, locale);

  // Stash away the patterns for later use.
  this._pattern = patterns;
  
  this._locale = (locale != null) ? locale : getJavaLanguage(locale);
}

TrDateTimeConverter.prototype = new TrConverter();

TrDateTimeConverter.prototype.getFormatHint = function()
{
	//customized hint
	if(this._messages && this._messages["hint"])
	{
    return TrMessageFactory.createCustomMessage(
      this._messages["hint"],
      ""+this._exampleString);
		
	}
	else
	{
		//no customized hint
		var key = "org.apache.myfaces.trinidad.convert.DateTimeConverter." + this._type + "_HINT";
    return TrMessageFactory.createMessage(
      key,
      ""+this._exampleString);
	}
}

TrDateTimeConverter.prototype.getAsString = function(
  formatTime
  )
{

  //correct Date Time ?
  if(this._offset)
  {
    var min = formatTime.getMinutes();
    formatTime.setMinutes((+min) - parseInt(this._offset));
  }
  var stringHolder = new Object();
  stringHolder.value ="";
  
  var pattern = this._pattern;
  if (typeof pattern != "string")
    pattern = pattern[0];
    
  _doClumping(pattern,
              this._localeSymbols,
              this._locale,
              _subformat,
              formatTime,
              stringHolder);

  if(this._offset)
  {
  	var gmtDiff = (((this._offset + formatTime.getTimezoneOffset()) * -1) / 60);
  	if(parseInt(gmtDiff) > 0)
  	{
  		stringHolder.value = stringHolder.value + "+"
  	}
  	stringHolder.value = stringHolder.value + gmtDiff + ":00";
  }
  return stringHolder.value;
}

TrDateTimeConverter.prototype.setDiffInMins = function(
  offset
  )
{ 
  this._offset = offset;
}

TrDateTimeConverter.prototype.getDiffInMins = function()
{
  return this._offset;
}

TrDateTimeConverter.prototype.getLocaleSymbols = function()
{
  return this._localeSymbols;
}


/**
 * Parses a String into a Date using the current object's pattern.  If the
 * parsing fails, undefined will be returned.
 */
TrDateTimeConverter.prototype.getAsObject  = function(
  parseString,
  label
  )
{
  // The following are from the javadoc for DateTimeConverter
  // If the specified String is null, return a null. Otherwise, trim leading and trailing whitespace before proceeding.
  // If the specified String - after trimming - has a zero length, return null.
  if (parseString == null)
    return null;

  parseString = TrFormatUtils.trim(parseString);
  if (parseString.length == 0)
    return null;

  var pattern = this._pattern;
  
  var invalidFormatMsg;
  var key = "org.apache.myfaces.trinidad.convert.DateTimeConverter.CONVERT_"+this._type;
  if(this._messages && this._messages["detail"])
  {
    invalidFormatMsg = _createCustomFacesMessage(TrMessageFactory.getSummaryString(key),
                                          this._messages["detail"],
                                          label,
                                          parseString,
                                          this._exampleString);
  }
  else
  {
    invalidFormatMsg = _createFacesMessage( key,
                                          label,
                                          parseString,
                                          this._exampleString);
  }
  
  var invalidDateMsg = _createFacesMessage ("org.apache.myfaces.trinidad.convert.DateTimeConverter.CONVERT_DATE_INVALID_DATE", 
                                            label, 
                                            parseString);
  
  if (typeof pattern == "string")
  {
    return this._simpleDateParseImpl(parseString,
                                pattern,
                                this._localeSymbols,
                                this._locale,
                                invalidFormatMsg,
                                invalidDateMsg);
  }
  else
  { 
    var i;
    for (i = 0; i < pattern.length; i++)
    {
      try{
        var date = this._simpleDateParseImpl(parseString,
                                        pattern[i],
                                        this._localeSymbols,
                                        this._locale,
                                        invalidFormatMsg,
                                        invalidDateMsg);
        return date;
      }
      catch (e)
      {
        // Trinidad-1634: If the format is valid, but the date is invalid,
        // return that error instead of trying other formats.
        if (e.isDateInvalid)
          throw e;
          
        // if we're not on the last pattern try the next one, 
        // but if we're on the last pattern, throw the exception
        if ( i == pattern.length-1 )
          throw e;
      }
    }
  }
}

TrDateTimeConverter.prototype._endsWith = function(
  value,
  suffix
  )
{
  // TODO: add to a String utils class ?
  var startPos = value.length - suffix.length;
  if (startPos < 0)
    return false;
  return (value.lastIndexOf(suffix, startPos) == startPos);
}

TrDateTimeConverter.prototype._initPatterns  = function(
  pattern, locale)
{
  // We need to build up an Array of all acceptable patterns,
  // which we'll stash away for later use.  If we do lenient
  // parsing, then we may end up supporting a variety of patterns
  // that weren't passed in via the "pattern" arg.  Previously,
  // if the "pattern" arg is itself an Array, we just tacked
  // any additional lentient patterns right into the "pattern"
  // Array.  However, the "pattern" Array is actually referenced
  // from other locations, so we should avoid modifying this
  // array directly.  Instead, we create our own "patterns"
  // Array and append all supported patterns into this Array.
  var patterns = new Array();

  // Array from which the patterns array will be constructed.
  var tmpPatterns = new Array();
  
  // If pattern is non-null, append it to the tmpPatterns array.
  if (pattern)
    tmpPatterns = tmpPatterns.concat(pattern);

  // At this point 'locale' is the value of the locale attribute; if 'locale' is 
  // null, we should make sure to grab the same locale that was grabbed by getLocaleSymbols() (i.e.,getJavaLanguage)
  if (!locale)
    locale = getJavaLanguage(locale);
  
  // Make sure the static map of convenience patterns has been initialized.
  if (!_CONVENIENCE_PATTERNS)
    this._initConveniencePatterns();
  
  // see TRINIDAD-859
  var convPatterns = _CONVENIENCE_PATTERNS[locale];
  if (convPatterns)
    tmpPatterns = tmpPatterns.concat(convPatterns);
  
  // Add the tmp patterns and all their lenient pattern variants.
  var len = tmpPatterns.length;
  for (var c = 0; c < len; c++)
  {
    var convPattern = tmpPatterns[c];
    patterns[patterns.length] = convPattern;
    var baseCount = 1;
    
    // Bug 2002065: 
    // Be forgiving of users who prefer a different separator and alternative 
    // month styles. We are to be lenient by default with ADF Faces.
    
    // We should add all the leniency patterns for this default pattern first.
    // First add in replacements for month parsing.
    if (convPattern.indexOf('MMM') != -1)
    {
      patterns[patterns.length] = convPattern.replace(/MMM/g, 'MM');
      patterns[patterns.length] = convPattern.replace(/MMM/g, 'M');
      baseCount = 3;
    }
    
    // Now add support for all of the above with any of the separators below. 
    // The separator is the same for all patterns since we only replaced month.
    var idx = patterns.length - baseCount;
    if (convPattern.indexOf('/') !=  - 1)
    {
      for (var i = idx; i < idx + baseCount; i++)
        patterns[patterns.length] = patterns[i].replace(/\//g, '-');
      
      for (var i = idx; i < idx + baseCount; i++)
        patterns[patterns.length] = patterns[i].replace(/\//g, '.');
    }
    else if (convPattern.indexOf('-') !=  - 1)
    {
      for (var i = idx; i < idx + baseCount; i++)
        patterns[patterns.length] = patterns[i].replace(/-/g, '/');
      
      for (var i = idx; i < idx + baseCount; i++)
        patterns[patterns.length] = patterns[i].replace(/-/g, '.');
    }
    else if (convPattern.indexOf('.') !=  - 1)
    {
      for (var i = idx; i < idx + baseCount; i++)
        patterns[patterns.length] = patterns[i].replace(/\./g, '-');
      
      for (var i = idx; i < idx + baseCount; i++)
        patterns[patterns.length] = patterns[i].replace(/\./g, '/');
    }
  }
  
  return patterns;
}

/**
 * Initialize the static map of convenience patterns. This should only be called 
 * if _CONVENIENCE_PATTERNS is null (so that this map is recreated only when the 
 * page is reloaded). All map entries MUST match those of the server map:
 * trinidad-api\src\main\java\org\apache\myfaces\trinidad\convert\DateTimeConverter.java->_CONVENIENCE_PATTERNS
 */
TrDateTimeConverter.prototype._initConveniencePatterns = function() 
{
  _CONVENIENCE_PATTERNS = new Object();
  
  // All map entries added here MUST match the entries added to the server map:
  // trinidad-api\src\main\java\org\apache\myfaces\trinidad\convert\DateTimeConverter.java->_CONVENIENCE_PATTERNS
  _CONVENIENCE_PATTERNS.en_US = ["MMMM dd, yy", "MMMM/dd/yy", "dd-MMMM-yy"];  
}

TrDateTimeConverter.prototype._simpleDateParseImpl = function(
  parseString,
  parsePattern,
  localeSymbols,
  locale,
  invalidFormatMsg,
  invalidDateMsg)
{
  // When a pattern (e.g. dd.MM.yyyy HH:mm' Uhr ') requires a whitespace
  // at the end, we should honor that. As the JSF spec (see http://bit.ly/kTelf)
  // wants the converter to trim leading/trailing whitespace, we have to append
  // one, if the pattern requires it at the end...
  if(this._endsWith(parsePattern, " '"))
  {
    parseString += " ";
  }
	
  var parseContext = new Object();
  parseContext.currIndex = 0;
  parseContext.parseString = parseString;
  parseContext.parsedHour = null;
  parseContext.parsedMinutes = null;
  parseContext.parsedSeconds = null;
  parseContext.parsedMilliseconds = null;
  parseContext.isPM = false;
  parseContext.parsedBC = false;
  parseContext.parsedFullYear = null;
  parseContext.parsedMonth = null;
  parseContext.parsedDate = null;
  parseContext.hourOffset = null;
  parseContext.minOffset = null;

  var parsedTime = new Date(0);
  parsedTime.setDate(1);

  // parse the time
  if (_doClumping(parsePattern,
                  localeSymbols,
                  locale,
                  _subparse,
                  parseContext,
                  parsedTime))
  {
    if (parseString.length != parseContext.currIndex)
    {
      parseContext.parseException = new TrConverterException (invalidFormatMsg);
      throw parseContext.parseException;
    }

    // give up instantly if we encounter timezone because
    // the client can never correctly convert to a milliseconds
    // value accurately due to lack of timezone and Daylight savings
    // rules in Javascript
    // Undefined is used in _multiValidate as a flag to skip
    // validation and avoid required errors (which returning null would trigger)
    if ((parseContext.hourOffset != null) || 
       (parseContext.minOffset != null))
      return undefined;

    // Set the parsed year, if any;  adjust for AD vs. BC
    var year = parseContext.parsedFullYear;
    if (year != null)
    {
      // convert year to BC
      if (parseContext.parsedBC)
      {
        year = _getADEra().getFullYear() - year;
      }

      parsedTime.setFullYear(year);
      parseContext.parsedFullYear = year;
    }

    // Set the parsed month, if any
    var month = parseContext.parsedMonth;
    if (month != null)
      parsedTime.setMonth(month);

    // Set the parsed day-of-month, if any
    var date = parseContext.parsedDate;
    if (date != null)
      parsedTime.setDate(date);

    // Set the parsed hour, if any.  Adjust for AM vs. PM
    var hour = parseContext.parsedHour;
    if (hour != null)
    {
      if (parseContext.isPM && (hour < 12))
      {
        hour += 12;
      }

      parsedTime.setHours(hour);
      parseContext.parsedHour = hour;
    }

    // Set the parsed minutes, if any
    var minutes = parseContext.parsedMinutes;
    if (minutes != null)
      parsedTime.setMinutes(minutes);

    // Set the parsed seconds, if any
    var seconds = parseContext.parsedSeconds;
    if (seconds != null)
      parsedTime.setSeconds(seconds);

    // Set the parsed milliseconds, if any
    var milliseconds = parseContext.parsedMilliseconds;
    if (milliseconds != null)
      parsedTime.setMilliseconds(milliseconds);

    // so far we have done a lenient parse
    // now we check for strictness
    if (!_isStrict(parseContext, parsedTime))
    {
      // Trinidad-1634: If the format is correct, but the date doesn't 
      // match, throw a different error.
      parseContext.parseException = new TrConverterException (invalidDateMsg);
      parseContext.parseException.isDateInvalid = true;
      throw parseContext.parseException;
    }
      
    //correct Date Time ?
    if(this._offset)
    {
      var min = parsedTime.getMinutes();
      parsedTime.setMinutes((+min) + parseInt(this._offset));
    }

    return parsedTime;
  }
  else
  {
    // failure
     parseContext.parseException = new TrConverterException (invalidFormatMsg);
     throw parseContext.parseException;
  }
}
