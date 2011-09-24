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
 * Listener for RowKeyChangeEvent.
 */
public interface RowKeyChangeListener
{
  /**
   * Called by CollectionModel to inform a RowKeyChangeListener that a RowKey has changed
   * The old row key or the new row key can be null, it is expected that listener implementation
   * to handle these situations with best efforts, for example, clear and rebuild component
   * state cache to recover.
   */
  public void onRowKeyChange(RowKeyChangeEvent event); 
}
