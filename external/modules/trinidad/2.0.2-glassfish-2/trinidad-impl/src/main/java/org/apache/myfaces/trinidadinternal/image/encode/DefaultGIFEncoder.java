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
package org.apache.myfaces.trinidadinternal.image.encode;

/**
 * This class only exists so that we can do deferred instantiation of
 * the default OctreeEncoder/OracleGIFEncoder - it provides the no-arg
 * constructor that we need for deferred instantiation.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/DefaultGIFEncoder.java#0 $) $Date: 10-nov-2005.19:05:15 $
 */
public class DefaultGIFEncoder extends AlphaMultiplyEncoder
{
  public DefaultGIFEncoder()
  {
    super(new OracleGIFEncoder());
  }
}
