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

/**
 * Identifies a collection that is indexed by rowKeys.
 * Data is accessed by setting a rowKey, and then accessing
 * {@link #getRowData}.
 */
public interface RowKeyIndex
{
  /**
   * Gets the number of values in this collection
   * @return -1 if the number of values is not known.
   */
  public int getRowCount();
  
  /**
   * Gets the index of the current value.
   * The current value is returned by calling {link #getRowData}
   * @return the zero-based index of the current value, or -1 if there
   * is no current value
   */
  public int getRowIndex();
  
  /**
   * Sets up a value at a particular index to be the current value.
   * The current value is returned by calling {link #getRowData}
   * @param rowIndex the zero-based index of the value to make current.
   * Use -1 to clear the current value
   */
  public void setRowIndex(int rowIndex);

  /**
   * Gets the rowKey of the current value.
   * The current value is returned by calling {link #getRowData}
   * @return the rowKey of the current value, or null if there
   * is no current value
   */
  public Object getRowKey();
  
  /**
   * Sets up a value at a particular rowKey to be the current value.
   * The current value is returned by calling {link #getRowData}
   * @param rowKey the rowKey of the value to make current.
   * Use null to clear the current value
   */
  public void setRowKey(Object rowKey);
  
  /**
   * Checks to make sure a value exists for the current index or rowKey.
   * This is useful if the number of values in this collection is not known
   * (See {@link #getRowCount}).
   * @see #getRowKey
   * @see #getRowIndex
   * @return true if a value exists; false otherwise.
   */
  public boolean isRowAvailable();
  
  /**
   * Gets the current value identified by the current index or rowKey.
   * @see #getRowKey
   * @see #getRowIndex
   * @return null if the current value has been cleared.
   */
  public Object getRowData();

  /**
   * Checks to make sure a value exists for the given index.
   * @param rowIndex the index of the row to check.
   * @return true if a value exists; false otherwise.
   */
  public boolean isRowAvailable(int rowIndex);

  /**
   * Gets the row value at the given index.
   * @param rowIndex the index of the row to get data from.
   * @return null if the current value has been cleared.
   */
  public Object getRowData(int rowIndex);


  /**
   * Check for an available row by row key. 
   * @param rowKey the row key for the row to check.
   * @return true if a value exists; false otherwise.
   */
  public boolean isRowAvailable(Object rowKey);


  /**
   * Get row data by row key. 
   * @param rowKey the row key for the row to get data.
   * @return row data
   */
  public Object getRowData(Object rowKey); 


  /**
   * Check if a range of rows is available starting from the current position 
   * @param rowsToCheck number of rows to check
   * @return true if all rows in range are available
   */
  public boolean areRowsAvailable(int rowsToCheck);

  /**
   * Check if a range of rows is available from a starting index without 
   * requiring the client to iterate over the rows
   * @param startIndex the starting index for the range
   * @param rowsToCheck number of rows to check
   * @return true if all rows in range are available
   */
  public boolean areRowsAvailable(int startIndex, int rowsToCheck) ;


  /**
   * Check if a range of rows is available from a starting row key without 
   * requiring the client to iterate over the rows
   * @param startRowKey the starting row key for the range
   * @param rowsToCheck number of rows to check
   * @return true if all rows in range are available
   */
  public boolean areRowsAvailable(Object startRowKey, int rowsToCheck) ;  
}
