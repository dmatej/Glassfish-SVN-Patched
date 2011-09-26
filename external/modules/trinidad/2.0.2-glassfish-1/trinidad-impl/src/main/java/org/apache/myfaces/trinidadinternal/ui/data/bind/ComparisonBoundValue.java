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
package org.apache.myfaces.trinidadinternal.ui.data.bind;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * Booolean BoundValue that compares either two BoundValues or a
 * BoundValues and an Object with a comparison operator and
 * returns the Boolean result.
 * <p>
 * <STRONG>
 * Only BoundValues that return <CODE>java.lang.Numbers</CODE> or
 * <CODE>java.lang.Numbers</CODE> can be used with comparisons other than
 * <CODE>COMPARISON_EQUALS</CODE> and <CODE>COMPARISON_NOT_EQUALS</CODE>
 * </STRONG>
 * <p>
 *@version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bind/ComparisonBoundValue.java#0 $) $Date: 10-nov-2005.18:56:36 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ComparisonBoundValue implements BoundValue
{
  /**
   * True if the left and right sides are equivalent.
   */
  public static final int COMPARISON_EQUALS     = 1;

  /**
   * True if the left and right sides are not equivalent.
   */
  public static final int COMPARISON_NOT_EQUALS = ~COMPARISON_EQUALS;

  /**
   * True if the left side is greater than the right side.
   * <P>
   * <STRONG>This requires that both sides be or return
   * <CODE>java.lang.Number</CODE>s.
   */
  public static final int COMPARISON_GREATER_THAN = 2;

  /**
   * True if the left side is greater than or equal to the right side.
   * <P>
   * <STRONG>This requires that both sides be or return
   * <CODE>java.lang.Number</CODE>s.
   */
  public static final int COMPARISON_GREATER_THAN_OR_EQUALS =
                                             COMPARISON_GREATER_THAN +
                                             COMPARISON_EQUALS;

  /**
   * True if the left side is less than the right side.
   * <P>
   * <STRONG>This requires that both sides be or return
   * <CODE>java.lang.Number</CODE>s.
   */
  public static final int COMPARISON_LESS_THAN =
                                           ~COMPARISON_GREATER_THAN_OR_EQUALS;

  /**
   * True if the left side is less than or equal to the right side.
   * <P>
   * <STRONG>This requires that both sides be or return
   * <CODE>java.lang.Number</CODE>s.
   */
  public static final int COMPARISON_LESS_THAN_OR_EQUALS =
                                           ~COMPARISON_GREATER_THAN;

  public ComparisonBoundValue(
    int        comparison,
    BoundValue leftSideValue,
    BoundValue rightSideValue
    )
  {
    if (leftSideValue == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "NULL_LEFTSIDEVALUE"));
    if (rightSideValue == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "NULL_RIGHTSIDEVALUE"));

    if ((comparison < COMPARISON_LESS_THAN) ||
        (comparison > COMPARISON_GREATER_THAN_OR_EQUALS))
      throw new IllegalArgumentException(_LOG.getMessage(
        "UNKNOWN_COMPARISON"));

    _comparison = comparison;

    _leftSideValue  = leftSideValue;
    _rightSideValue = rightSideValue;
  }

  public ComparisonBoundValue(
    int        comparison,
    BoundValue leftSideValue,
    Object     rightSide
    )
  {
    this(comparison, leftSideValue, new FixedBoundValue(rightSide));
  }


  public static ComparisonBoundValue createExistsValue(
    BoundValue existenceValue
    )
  {
    return new ComparisonBoundValue(COMPARISON_NOT_EQUALS,
                                    existenceValue,
                                    FixedBoundValue.NULL_VALUE);
  }


  /**
   * Calculates the current state of the model.
   */
  public Object getValue(
    UIXRenderingContext context
    )
  {
    Object leftSide  = _leftSideValue.getValue(context);
    Object rightSide = _rightSideValue.getValue(context);

    //
    // determine the comparison to attempt
    //
    boolean isNot = false;

    int comparison = _comparison;

    // make sure that we can always attempt an equivalence comparison,
    // by inverting the comparison if necessary
    if ((comparison & COMPARISON_EQUALS) == 0)
    {
      comparison = ~comparison;
      isNot      = true;
    }

    boolean areBothNumbers = (leftSide instanceof Number) &&
                             (rightSide instanceof Number);
    boolean newResult;
    // attempt equivalence comparison
    if (leftSide == rightSide)
    {
      newResult = true;
    }
    else if (leftSide != null)
    {
      if (areBothNumbers)
        newResult = _equalsForNumbers((Number) leftSide,
                                      (Number) rightSide);
      else
        newResult = leftSide.equals(rightSide);
    }
    else
    {
      newResult = false;
    }


    // if we have a greter than / less than comparison and the equivalence
    // comparison failed, tried the other comparison
    if (!newResult && (comparison != COMPARISON_EQUALS))
    {
      // numeric comparisons against null always fail
      if ((leftSide == null) || (rightSide == null))
        return Boolean.FALSE;

      if (!areBothNumbers)
      {
        if (_LOG.isSevere())
          _LOG.severe(new IllegalArgumentException(
                                "Numeric comparisons only allowed on numbers"));

        return Boolean.FALSE;
      }

      Number leftNumber  = (Number)leftSide;
      Number rightNumber = (Number)rightSide;

      //
      // at this point the comparison will be either less than or equals
      // or greater than or equals.  Since we know that the equals portion
      // must have failed, we can simply treat all comparisons as greater
      // than and invert the result for less than
      //
      if (comparison == COMPARISON_LESS_THAN_OR_EQUALS)
      {
        isNot = !isNot;
      }

      if (leftNumber instanceof Long)
      {
        newResult = (leftNumber.longValue() > rightNumber.longValue());
      }
      else
      {
        newResult = (leftNumber.doubleValue() > rightNumber.doubleValue());
      }
    }

    if (isNot)
      newResult = !newResult;

    return (newResult) ? Boolean.TRUE : Boolean.FALSE;
  }

  static private boolean _equalsForNumbers(Number a, Number b)
  {
    if ((a == null) || (b == null))
      return (a == b);

    Class<?> ac = a.getClass();
    Class<?> bc = b.getClass();
    if (ac == bc)
      return a.equals(b);

    if ((ac == Long.class) ||
        (ac == Integer.class) ||
        (ac == Short.class) ||
        (ac == Byte.class))
      return _equalsForLong(a.longValue(), b, bc);

    if ((ac == Double.class) || (ac == Float.class))
      return _equalsForDouble(a.doubleValue(), b, bc);

    if (ac == BigInteger.class)
      return _equalsForBigInteger((BigInteger) a, b, bc);

    if (ac == BigDecimal.class)
      return _equalsForBigDecimal((BigDecimal) a, b, bc);

    return a.equals(b);
  }

  static private boolean _equalsForLong(long a, Number b, Class<?> bc)
  {
    if ((bc == Double.class) || (bc == Float.class))
    {
      return (b.doubleValue() == a);
    }

    if (bc == BigDecimal.class)
    {
      return BigDecimal.valueOf(a).equals(b);
    }

    if (bc == BigInteger.class)
    {
      return BigInteger.valueOf(a).equals(b);
    }

    return a == b.longValue();
  }


  static private boolean _equalsForDouble(double a, Number b, Class<?> bc)
  {
    if (bc == BigDecimal.class)
    {
      return new BigDecimal(a).equals(b);
    }

    if (bc == BigInteger.class)
    {
      return new BigDecimal(a).equals(new BigDecimal((BigInteger) b));
    }

    return a == b.doubleValue();
  }


  static private boolean _equalsForBigInteger(BigInteger a, Number b, Class<?> bc)
  {
    if (bc == BigDecimal.class)
    {
      return new BigDecimal(a).equals(b);
    }

    if ((bc == Double.class) || (bc == Float.class))
    {
      return new BigDecimal(a).equals(new BigDecimal(b.doubleValue()));
    }

    return BigInteger.valueOf(b.longValue()).equals(a);
  }


  static private boolean _equalsForBigDecimal(BigDecimal a, Number b, Class<?> bc)
  {
    if (bc == BigInteger.class)
    {
      return a.equals(new BigDecimal((BigInteger) b));
    }

    if ((bc == Double.class) || (bc == Float.class))
    {
      return a.equals(new BigDecimal(b.doubleValue()));
    }

    return a.equals(BigDecimal.valueOf(b.longValue()));
  }

  private BoundValue _leftSideValue;
  private BoundValue _rightSideValue;
  private int        _comparison;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ComparisonBoundValue.class);
}
