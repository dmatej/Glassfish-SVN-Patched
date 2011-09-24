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
package org.apache.myfaces.trinidad.model;

import java.io.Serializable;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Default BoundedRangeModel implementation. Defaults 'maximum' and 'value' to
 *  be -1. Validates that value <= maximum.
 * This model implementation can conveniently be used by the 'progress' 
 *  components.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/model/DefaultBoundedRangeModel.java#0 $) $Date: 10-nov-2005.19:08:52 $
 */
public class DefaultBoundedRangeModel extends BoundedRangeModel implements Serializable {
  /**
   * Construct a new DefaultBoundedRangeModel with defaults.
   */
  public DefaultBoundedRangeModel()
  {
    _maximum = -1;
    _value = -1;
  }
 
  /**
   * Constructs a new DefaultBoundedRangeModel with specified 'maximum' and 
   *  'value'.
   * value should be in the range -1 <-> maximum.
   * @param value - the value for this model.
   * @param maximum - the maximum for this bounded range.
   * @exception IllegalArgumentException if the value is not in the valid range.
   */
  public DefaultBoundedRangeModel(long value, long maximum)
  {
    if (value > maximum || value < -1)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "SETTING_ILLEGAL_VALUE"));
    }
    _maximum = maximum;
    _value = value;
  }

  @Override
  public long getMaximum()
  {
    return _maximum;
  }
  
  @Override
  public long getValue()
  {
    return _value;
  }
  
  /**
   * Sets the maximum value for this model.
   * Defaults to -1 which indicates that maximum is unknown.
   * @param maximum the maximum value to be set
   */
  public void setMaximum(long maximum)
  {
    _maximum = maximum;    
  }
  
  /**
   * Sets the current value for this model.
   * Ensure that the maximum is set before the value is being set.
   * Value should be in the range -1 <-> maximum. Defaults to -1.
   * @param value the current value to be set.
   * @exception IllegalArgumentException if the value is not in the valid range.
   */
  public void setValue(long value)
  {
    if (value > _maximum || value < -1)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "SETTING_ILLEGAL_VALUE"));
    }
    _value = value;
  }
  
  private long _value;
  private long _maximum;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    DefaultBoundedRangeModel.class);

  private static final long serialVersionUID = 1L;
}
