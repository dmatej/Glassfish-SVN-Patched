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

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.CollectionComponent;
import org.apache.myfaces.trinidad.component.UIXTable;

/**
 */
public final class TableUtils 
{
  /**
   * implements a loop that loops through all the rows in the visible range
   */
  public static abstract class RowLoop
  {
    public RowLoop()
    {
    }

    /**
     * saves the state of the table that might be changed by the loop
     * @param fc
     * @param table
     */
    private void _saveOldState(CollectionComponent table)
    {
      _oldIndex = table.getRowKey();
    }

    /**
     * restores the state of the table that was changed by this loop
     * @param fc
     * @param table
     */
    private void _restoreOldState(CollectionComponent table)
    {
      table.setRowKey(_oldIndex);
    }

    /**
     * calls {@link #saveOldState} to save any state. Then calls {@link #loop} to
     * run through the loop. Finally, calls {@link #restoreOldState} to
     * restore any state.
     * @param fc
     * @param table
     * @throws java.io.IOException
     */
    public final void run(FacesContext fc, CollectionComponent table) throws IOException
    {
      _saveOldState(table);
      try
      {
        loop(fc, table);
      }
      finally
      {
        _restoreOldState(table);
      }
    }

    /**
     * selects each index in the table that is in the current range 
     * and calls {@link #processRow}
     * @param fc
     * @param table
     * @throws java.io.IOException
     */
    protected void loop(FacesContext fc, CollectionComponent table) throws IOException
    {
      UIComponent comp = (UIComponent) table;
      boolean showAll = 
        Boolean.TRUE.equals(comp.getAttributes().get(UIXTable.SHOW_ALL_KEY.getName()));
      
      int rangeStart = table.getFirst();
      int rangeEnd = showAll  
        ? table.getRowCount() - 1
        : org.apache.myfaces.trinidad.component.TableUtils.getLast(table);
      for(int i=rangeStart; i<=rangeEnd; i++)
      {
        table.setRowIndex(i);
        processRow(fc, table);
      }
    }
  
    /**
     * this method sets up the 'var' variable and calls
     * {@link #processRowImpl}
     * @param fc
     * @param table
     * @throws java.io.IOException
     */
    protected final void processRow(FacesContext fc, CollectionComponent table)
      throws IOException
    {
       processRowImpl(fc, table);
    }
  
    /**
     * this method will be called for each visible row.
     * the rowIndex of table will be set to the corresponding row.
     * the 'var' variable will be setup before this method is called.
     * @param table
     */
    protected abstract void processRowImpl(FacesContext fc, CollectionComponent table)
      throws IOException;
      
    private Object _oldIndex = null;
  }


  /**
   * gets the number of visible rows in the table, taking the range start and
   * range size into account.
   * @param table
   * @return 
   */
  public static int getVisibleRowCount(CollectionComponent table)
  {
    int rangeStart = table.getFirst();
    int rangeEnd = org.apache.myfaces.trinidad.component.TableUtils.getLast(table) + 1;
    return rangeEnd - rangeStart;
  }
    
  private TableUtils()
  {
  }
}
