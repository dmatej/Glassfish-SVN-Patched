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
package org.apache.myfaces.trinidadinternal.image.painter;

import java.awt.Insets;


/**
 * Immutable version of Insets.
 *
 * @see java.awt.Insets
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/ImmInsets.java#0 $) $Date: 10-nov-2005.19:04:58 $
 */
public class ImmInsets implements Cloneable
{
  /**
   * The inset from the top.
   */
  public final int top;

  /**
   * The inset from the left.
   */
  public final int left;

  /**
   * The inset from the bottom.
   */
  public final int bottom;

  /**
   * The inset from the right.
   */
  public final int right;


  /**
   * Constructs and initializes a new ImmInsets with the specified top,
   * left, bottom, and right insets.
   * @param top the inset from the top
   * @param left the inset from the left
   * @param bottom the inset from the bottom
   * @param right the inset from the right
   */
  public ImmInsets(
    int top,
    int left,
    int bottom,
    int right
    )
  {
    this.top    = top;
    this.left   = left;
    this.bottom = bottom;
    this.right  = right;
  }


  /**
   * Constructs and initializes a new ImmInsets, copying
   * the values from an insets object.
   * @param insets the insets object
   */
  public ImmInsets(
    Insets insets
    )
  {
    top    = insets.top;
    left   = insets.left;
    bottom = insets.bottom;
    right  = insets.right;
  }


  /**
   * Returns the shared, empty insets object.
   */
  public static ImmInsets getEmptyInsets()
  {
    return _sEmptyInsets;
  }


  /**
   * Creates an Insets object based on this ImmInsets.
   */
  public Insets toInsets()
  {
    return new Insets(top, left, bottom, right);
  }


  /**
   * Checks whether two insets objects are equal.
   */
  @Override
  public boolean equals(Object obj)
  {
    if (obj instanceof ImmInsets)
    {
      ImmInsets insets = (ImmInsets)obj;

      return ((top    == insets.top)    &&
              (left   == insets.left)   &&
              (bottom == insets.bottom) &&
              (right  == insets.right));
    }
    else
    {
      if (obj instanceof Insets)
      {
        Insets insets = (Insets)obj;

        return ((top    == insets.top)    &&
                (left   == insets.left)   &&
                (bottom == insets.bottom) &&
                (right  == insets.right));
      }
      else
      {
        return false;
      }
    }
  }

  /**
   * Override of Object.hashCode()
   */
  @Override
  public int hashCode()
  {
    // We need to override hashCode since we override equals().
    // I don't think we ever actually use ImmInsets as a hash
    // key, so any old hash code should do.
    return (top           ^
            (left   << 1) ^
            (bottom << 2) ^
            (right  << 3));
  }


  /**
   * Returns a String object representing this Inset's values.
   */
  @Override
  public String toString()
  {
    return getClass().getName() +
           "[top="    + top     +
           ",left="   + left    +
           ",bottom=" + bottom  +
           ",right="  + right + "]";
  }


  @Override
  public Object clone()
  {
    try
    {
      return super.clone();
    }
    catch (CloneNotSupportedException e)
    {
      // this shouldn't happen, since we are Cloneable
      throw new InternalError();
    }
  }

  private static final ImmInsets _sEmptyInsets = new ImmInsets(0, 0, 0, 0);
}
