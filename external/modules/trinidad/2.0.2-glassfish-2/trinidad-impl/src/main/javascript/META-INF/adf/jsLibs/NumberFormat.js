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

/**
 * constructor for TrNumberFormat.
 */
 function TrNumberFormat(type, locale)
{
  if(!type)
    alert("type for TrNumberFormat not defined!");
  this._type = type;
  
  this._localeSymbols = getLocaleSymbols(locale);
  this._pPre = this._localeSymbols.getPositivePrefix();
  this._pSuf = this._localeSymbols.getPositiveSuffix();
  this._nPre = this._localeSymbols.getNegativePrefix();
  this._nSuf = this._localeSymbols.getNegativeSuffix();

  //default values, similar to JDK (values from Apache Harmony)
  this._maxFractionDigits = 3;
  this._maxIntegerDigits  = 40;
  if(this._type=="currency")
  {
    this._minFractionDigits = 2;
  }
  else
  {
    this._minFractionDigits = 0;
  }
  this._minIntegerDigits  = 1;
  this._groupingUsed = true;
  
}
//***********************
// static
//***********************

/**
 * Returns a number formater.
 */
TrNumberFormat.getNumberInstance = function(locale)
{
  return new TrNumberFormat("number", locale);
}

/**
 * Returns a currency formater.
 */
TrNumberFormat.getCurrencyInstance = function(locale)
{
  return new TrNumberFormat("currency", locale);
}

/**
 * Returns a percent formater.
 */
TrNumberFormat.getPercentInstance = function(locale)
{
  return new TrNumberFormat("percent", locale);
}

/**
 * Sets whether this NumberFormat formats and parses numbers using a
 * grouping separator.
 * 
 * @param value true when a grouping separator is used, false otherwise
 */
TrNumberFormat.prototype.setGroupingUsed = function(groupingUsed)
{
  this._groupingUsed = groupingUsed;
}

/**
 * Answers whether this NumberFormat formats and parses numbers using a
 * grouping separator.
 * 
 * @return true when a grouping separator is used, false otherwise
 */
TrNumberFormat.prototype.isGroupingUsed = function()
{
  return this._groupingUsed;
}

/**
 * Used to specify the new maximum count of integer digits that are printed
 * when formatting. If the maximum is less than the number of integer
 * digits, the most significant digits are truncated.
 * 
 * @param value the new maximum number of integer numerals for display
 */
TrNumberFormat.prototype.setMaximumIntegerDigits = function(number)
{
  //taken from the Apache Harmony project
  if(number)
  {
    this._maxIntegerDigits = number < 0 ? 0 : number;
    if (this._minIntegerDigits > this._maxIntegerDigits)
    {
      this._minIntegerDigits = this._maxIntegerDigits;
    }
  }
} 

/**
 * Answers the maximum number of integer digits that are printed when
 * formatting. If the maximum is less than the number of integer digits, the
 * most significant digits are truncated.
 * 
 * @return the maximum number of integer digits
 */
TrNumberFormat.prototype.getMaximumIntegerDigits = function()
{
  //taken from the Apache Harmony project
  return this._maxIntegerDigits;
}

/**
 * Sets the maximum number of fraction digits that are printed when
 * formatting. If the maximum is less than the number of fraction digits,
 * the least significant digits are truncated.
 * 
 * @param value the maximum number of fraction digits
 */
TrNumberFormat.prototype.setMaximumFractionDigits = function(number)
{
  //taken from the Apache Harmony project
  if(number)
  {
    this._maxFractionDigits = number < 0 ? 0 : number;
    if (this._maxFractionDigits < this._minFractionDigits)
    {
      this._minFractionDigits = this._maxFractionDigits;
    }
  }
} 

/**
 * Answers the maximum number of fraction digits that are printed when
 * formatting. If the maximum is less than the number of fraction digits,
 * the least significant digits are truncated.
 * 
 * @return the maximum number of fraction digits
 */
TrNumberFormat.prototype.getMaximumFractionDigits = function()
{
  //taken from the Apache Harmony project
  return this._maxFractionDigits;
}

/**
 * Sets the minimum number of integer digits that are printed when
 * formatting.
 * 
 * @param value the minimum number of integer digits
 */
TrNumberFormat.prototype.setMinimumIntegerDigits = function(number)
{
  //taken from the Apache Harmony project
  if(number)
  {
    this._minIntegerDigits = number < 0 ? 0 : number;
    if(this._minIntegerDigits > this._maxIntegerDigits)
    {
      this._maxIntegerDigits = this._minIntegerDigits;
    }
  }
}

/**
 * Answers the minimum number of integer digits that are printed when
 * formatting.
 * 
 * @return the minimum number of integer digits
 */
TrNumberFormat.prototype.getMinimumIntegerDigits = function()
{
  //taken from the Apache Harmony project
  return this._minIntegerDigits;
}

/**
 * Sets the minimum number of fraction digits that are printed when
 * formatting.
 * 
 * @param value the minimum number of fraction digits
 */
TrNumberFormat.prototype.setMinimumFractionDigits = function(number)
{
  //taken from the Apache Harmony project
  if(number)
  {
    this._minFractionDigits = number < 0 ? 0 : number;
    if (this._maxFractionDigits < this._minFractionDigits)
    {
      this._maxFractionDigits = this._minFractionDigits;
    }
  }
}

/**
 * Answers the minimum number of fraction digits that are printed when
 * formatting.
 * 
 * @return the minimum number of fraction digits
 */
TrNumberFormat.prototype.getMinimumFractionDigits = function()
{
  //taken from the Apache Harmony project
  return this._minFractionDigits;
}

/**
 * Based on the type this func returns a percentage, currency or number string.
 */
TrNumberFormat.prototype.format = function(number)
{
  if(this._type=="percent")
    return this.percentageToString(number);
  else if (this._type=="currency")
    return this.currencyToString(number);
  else if (this._type=="number")
    return this.numberToString(number);
}

/**
 * Based on the type this func returns a number result, from the given formatted string.
 */
TrNumberFormat.prototype.parse = function(string)
{
  if(this._type=="percent")
    return this.stringToPercentage(string);
  else if (this._type=="currency")
    return this.stringToCurrency(string);
    
  // ELSE: assume this._type=="number"
  return this.stringToNumber(string);
}

/**
 * Formats a number string into a number.
 */
TrNumberFormat.prototype.stringToNumber = function(numberString)
{
  // parseFloat("123abc45") returns 123, but 123abc45 is considered an invalid number on the server, 
  // so check for a valid number first. Exclude non-numbers and disallow exponential notation.
  if (isNaN(numberString) || numberString.indexOf('e') != -1 || numberString.indexOf('E') != -1)
  {
    throw new TrParseException("not able to parse number");
  }
  return parseFloat(numberString);
}

/**
 * Formats a currency string into a number.
 */
TrNumberFormat.prototype.stringToCurrency = function(numberString)
{
  //is the string negative ?
  var negP = numberString.indexOf(this._nPre);
  var nSufNoSpace = this._nSuf;
  if (nSufNoSpace.charAt(0) == ' ' || nSufNoSpace.charAt(0) == '\xa0')
    nSufNoSpace = nSufNoSpace.substring(1);
  var negS = numberString.indexOf(nSufNoSpace);


  // TRINIDAD-1958: In Arabic the values for negPrefix and posPrefix are the same, so it is insufficient to test for
  // the presence of (only) negPrefix to determine if the number is negative. 
  if(negP != -1 && negS != -1)
  {
    numberString = numberString.substr(this._nPre.length, numberString.length - (this._nPre.length + nSufNoSpace.length));
    return (this.stringToNumber(numberString) * -1);
  }
  else
  {
    var posP = numberString.indexOf(this._pPre);
    var pSufNoSpace = this._pSuf;
    if (pSufNoSpace.charAt(0) == ' ' || pSufNoSpace.charAt(0) == '\xa0')
      pSufNoSpace = pSufNoSpace.substring(1);
    var posS = numberString.indexOf(pSufNoSpace);

    if(posP != -1 && posS != -1)
    {
      numberString = numberString.substr (this._pPre.length, numberString.length - (this._pPre.length + pSufNoSpace.length));
      numberString = this.stringToNumber(numberString);
      return numberString;
    }
    else
    {
       throw new TrParseException("not able to parse number");
    }//end-if we could not find a positive or negative prefix/suffix pair
  }//end-if not negative
}

/**
 * Formats a percent string into a number.
 */
TrNumberFormat.prototype.stringToPercentage = function(percentString)
{
  var isPercentage = (percentString.indexOf('%') != -1);
  if (!isPercentage)
  {
    throw new TrParseException("not able to parse number");
  }
  
  var numberString = percentString.replace(/\%/g, '');
  return this.stringToNumber(numberString);
}

/**
 * Formats a number into a a formatted string.
 */
TrNumberFormat.prototype.numberToString = function(number)
{
  //negative ?
  var negative = number<0;
  if(negative)
    number = (number*-1);

  var numberString = number + "";
  
  // check for scientific notation
  numberString = TrNumberFormat.scientificToExpanded(numberString);
  
  var index = numberString.indexOf(".");
  var numberStringLength = numberString.length;
  var ints;
  var fracs;
  if(index != -1)
  {
    ints = numberString.substring(0, index);
    fracs = numberString.substring(index+1, numberStringLength);
  }
  else
  {
    ints = numberString;
    fracs = "";
  }

  ints  = this._formatIntegers(ints);
  fracs = this._formatFractions(fracs)
  
  var decimalSeparator = this._localeSymbols.getDecimalSeparator();

  if(fracs!="")
    numberString = (ints+decimalSeparator+fracs);
  else
    numberString = (ints);
  
  if(negative)
    numberString = "-" + numberString;
  
  return numberString;
  
}

/**
 * Formats a number into a a formatted currency string.
 */
TrNumberFormat.prototype.currencyToString = function(number)
{
  //negative ?
  if(number<0)
  {
    number = (number*-1)+"";
    number = this.numberToString(number);
    return this._nPre + number + this._nSuf;
  }
  else
  {
    number = this.numberToString(number);
    return this._pPre + number + this._pSuf;
  }
}

/**
 * Formats a number into a a formatted percent string.
 */
TrNumberFormat.prototype.percentageToString = function(number)
{
  number = number * 100;
  number = this.getRounded(number);
  if (isNaN(number))
  {
    throw new TrParseException("not able to parse number");
  } 
  
  // Get the percent suffix. I.e. in French the suffix is " %", not just "%". The following assumes 
  // the percent suffix is the same when the number is positive or negative. It also assumes that 
  // the locale indeed uses a percent suffix (and not a percent prefix); if that assumption is 
  // wrong, we should be notified by the following exception. If any changes need to be made, you 
  // should start by looking at _getPercentData() in:
  // maven-i18n-plugin\src\main\java\org\apache\myfaces\trinidadbuild\plugin\i18n\uixtools\JSLocaleElementsGenerator.java
  var suffix = this._localeSymbols.getPercentSuffix();
  if (!suffix || suffix == "")
  {
    throw new TrParseException("percent suffix undefined or empty");
  }
  
  number = this.numberToString(number);
  return number + suffix;
}

/**
 * Static utility function.
 * Converts a number string from scientific notation to standard expanded notation.
 */
TrNumberFormat.scientificToExpanded = function(numberString)
{
  // check for scientific notation
  var expIndex = numberString.indexOf('e');
  if (expIndex == -1)
    return numberString;
    
  var prefix = "";
  if (numberString.charAt(0) == '-')
  {
    prefix = "-";
    numberString = numberString.substring(1);
    expIndex -= 1;
  }
  
  var isPosExp = numberString.charAt(expIndex + 1) == '+';
  var exp = parseInt(numberString.substring(expIndex + 2));
  var nFractionDigits = expIndex - 2;
  var zeroes = "";
  
  // The exponent should always be greater than the number of fraction digits.
  if (isPosExp)
  {
    for (var i = 0; i < exp - nFractionDigits; ++i)
      zeroes += "0";
      
    return prefix + numberString.charAt(0) + numberString.substring(2, expIndex) + zeroes;
  }
  
  // ELSE: negative exponent
  for (var i = 0; i < exp - 1; ++i)
    zeroes += "0";
    
  return prefix + "0." + zeroes + numberString.charAt(0) + numberString.substring(2, expIndex);
}

/**
 * Static utility function.
 * Trims extraneous leading zeroes.
 */
TrNumberFormat.trimLeadingZeroes = function(numberString)
{
  var strbuf = [];
  var i, ch;
  for (i = 0; i < numberString.length; ++i)
  {
    ch = numberString.charAt(i);
    
    if ((ch >= '1' && ch <= '9') || ch == '.')
      break;
      
    if (ch == '0' && i+1 < numberString.length && numberString.charAt(i+1) != '.')
      continue;
      
    strbuf.push(ch);
  }
  
  return strbuf.join('') + numberString.substring(i);
}

/**
 * helper for rounding values
 */
TrNumberFormat.prototype.getRounded = function(val)
{
  val = this.moveDecimalRight(val);
  val = Math.round(val);
  val = this.moveDecimalLeft(val);
  return val;
}

/**
 * helper for rounding values
 */
TrNumberFormat.prototype.moveDecimalRight = function(val)
{
  var newVal = '';
  newVal = this.moveDecimal(val, false);
  return newVal;
}

/**
 * helper for rounding values
 */
TrNumberFormat.prototype.moveDecimalLeft = function (val)
{
  var newVal = '';
  newVal = this.moveDecimal(val, true);
  return newVal;
}

/**
 * helper for rounding values
 */
TrNumberFormat.prototype.moveDecimal = function(val, left)
{
  var newVal = '';
  newVal = this.moveDecimalAsString(val, left);
  return parseFloat(newVal);
}

/**
 * helper for rounding values
 */
TrNumberFormat.prototype.moveDecimalAsString = function(val, left)
{
  //TODO: matzew make it nicer....
  var spaces = 2;
  if (spaces <= 0)
    return val; 
  var newVal = val + '';
  var extraZ = this.getZeros(spaces);
  var re1 = new RegExp('([0-9.]+)');
  if (left)
  {
    newVal = newVal.replace(re1, extraZ + '$1');
    var re2 = new RegExp('(-?)([0-9]*)([0-9]{' + spaces + '})(\\.?)');
    newVal = newVal.replace(re2, '$1$2.$3');
  }
  else
  {
    var reArray = re1.exec(newVal); 
    if (reArray != null)
    {
      newVal = newVal.substring(0,reArray.index) + reArray[1] + extraZ + newVal.substring(reArray.index + reArray[0].length); 
    }
    var re2 = new RegExp('(-?)([0-9]*)(\\.?)([0-9]{' + spaces + '})');
    newVal = newVal.replace(re2, '$1$2$4.');
  }
  newVal = newVal.replace(/\.$/, ''); 
  return newVal;
}

/**
 * 
 */
TrNumberFormat.prototype.getZeros = function(places)
{
  var extraZ = '';
  var i;
  for (i=0; i<places; i++) {
    extraZ += '0';
  }
  return extraZ;
}

//***********************
// PRIVATE
//***********************

/**
 * Formats the integer part of a number
 */
TrNumberFormat.prototype._formatIntegers = function(ints)
{
  var intsLength = ints.length;
  var maxInt = this.getMaximumIntegerDigits();
  var minInt = this.getMinimumIntegerDigits();

  var gap;
  if(intsLength>maxInt)
  {
    gap = intsLength-maxInt;
    ints = ints.substring(gap, intsLength);
  }
  else if(intsLength<minInt)
  {
    gap = minInt-intsLength;
    var leadingZeros = "";
    
    //we need some more leadingZeros
    while(gap>0)
    {
      leadingZeros = "0"+leadingZeros;
      --gap;
    }
    
    ints = leadingZeros + ints;
  }
  
  if(this.isGroupingUsed())
  {
    ints = this._addGroupingSeparators(ints);
  }

  return ints;
}

/**
 * Formats the fraction part of a number
 */
TrNumberFormat.prototype._formatFractions = function(fracs)
{
  var fracsLength = fracs.length;
  var maxFra = this.getMaximumFractionDigits();
  var minFra = this.getMinimumFractionDigits();

  if(fracsLength > maxFra && maxFra >= minFra)
  {
    fracs = fracs.substring(0, maxFra);
  }
  if(fracsLength <minFra)
  {
    var gap = minFra-fracsLength;
    
    //we need to add some zeros
    while(gap>0)
    {
      fracs = fracs + "0";
      --gap;
    }
  }
  return fracs;
}

/**
 * Adds localized grouping separators to a number string.
 */
TrNumberFormat.prototype._addGroupingSeparators = function(ints)
{
  var counter = ints.length;
  var toMuch = counter%3;
  var balance;
  var toFormat;
  var formatted = "";
  var groupingSeparator = this._localeSymbols.getGroupingSeparator();

  if(toMuch>0)
  {
    balance = (counter < 4) ? ints.substring(0, toMuch) : ints.substring(0, toMuch) + groupingSeparator;
    toFormat = ints.substring(toMuch, counter);
  }
  else
  {
    balance = "";
    toFormat = ints;
  }

  for(i=0; i < toFormat.length; i++)
  {
    if(i%3==0 && i!=0)
    {
      formatted += groupingSeparator;
    }
    formatted += toFormat.charAt(i);
  }
  ints = balance + formatted;
  return ints;
}
/** 
 * TrParseException is an exception thrown by the TrNumberFormater.
 * TODO: loclized messages ?
 */
function TrParseException(
  message
  )
{
  this._message = message;
}
TrParseException.prototype.getMessage = function()
{
  return this._message;
}
