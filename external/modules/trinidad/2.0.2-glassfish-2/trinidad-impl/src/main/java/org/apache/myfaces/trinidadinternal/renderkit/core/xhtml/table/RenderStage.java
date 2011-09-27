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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/table/RenderStage.java#0 $) $Date: 10-nov-2005.19:02:36 $
 */
public final class RenderStage
{
  /**
   * @return the current table render stage
   * @see #setStage(int)
   */
  public int getStage()
  {
    return _stage;
  }

  /**
   * @see #INITIAL_STAGE
   * @see #UPPER_CONTROL_BAR_STAGE      
   * @see #LOWER_CONTROL_BAR_STAGE      
   * @see #COLUMN_HEADER_STAGE    
   * @see #DATA_STAGE             
   * @see #START_ROW_STAGE             
   * @see #DETAIL_CELL_STAGE
   * @see #DETAIL_ROW_STAGE
   * @see #TREE_NODE_STAGE
   * @see #COLUMN_FOOTER_STAGE   
   * @see #END_STAGE              
   * @return the previous stage
   */
  public int setStage(int stage)
  {
    int ps = _stage;
    _stage = stage;
    return ps;
  }

  @Override
  public String toString()
  {
    return "RenderStage:"+_stage;
  }

  public RenderStage()
  {
    _stage = INITIAL_STAGE;
  }

  private int _stage;

  public static final int INITIAL_STAGE              = 0;
  public static final int UPPER_CONTROL_BAR_STAGE    = 10;  
  public static final int SUB_CONTROL_BAR_STAGE      = 12;
  public static final int LOWER_CONTROL_BAR_STAGE    = 15;

  /**
   * This is the stage when a column header cell is rendered
   */
  public static final int COLUMN_HEADER_STAGE        = 20;

  /**
   * This is the stage when a column data cell is rendered
   */
  public static final int DATA_STAGE                 = 30;

  /**
   * This is an initialization stage for a particular row
   */
  public static final int START_ROW_STAGE            = 35;

  /**
   * This is the stage when a detail cell (the cell with the hideShow)
   * is rendered
   */
  public static final int DETAIL_CELL_STAGE          = 40;

  /**
   * This is the stage when a disclosed detail row is rendered
   */
  public static final int DETAIL_ROW_STAGE           = 45;

  /**
   * This is the stage when the object hierarchy column cell is rendered
   * in an HGrid
   */
  public static final int TREE_NODE_STAGE            = 50;

  /**
   * This is the stage when the table footer is rendered
   */
  public static final int COLUMN_FOOTER_STAGE        = 60;

  public static final int END_STAGE                  = 100;
}
