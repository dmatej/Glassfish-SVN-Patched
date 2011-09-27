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
package org.apache.myfaces.trinidadinternal.share.config;

/**
 * Exception thrown when a directory is entirely unavailable.
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/config/DirectoryUnavailableException.java#0 $) $Date: 10-nov-2005.19:00:20 $
 */
public class DirectoryUnavailableException extends RuntimeException
{
  /**
   * Creates a DirectoryUnavailableException.
   */
  public DirectoryUnavailableException(String message, Object directoryKey)
  {
    super(message);
    _directoryKey = directoryKey;
  }

  /**
   * Returns the directory that was being searched for.
   */
  public Object getDirectory()
  {
    return _directoryKey;
  }

  private final Object _directoryKey;
  private static final long serialVersionUID = 1L;
}
