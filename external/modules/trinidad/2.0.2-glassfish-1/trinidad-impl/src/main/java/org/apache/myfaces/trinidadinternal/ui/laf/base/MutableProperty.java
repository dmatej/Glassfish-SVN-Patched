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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

/**
 * Interface that should be implemented on objects set as properties that
 * need to be mutated.  This is used by TryRenderer to clone the
 * objects to protect changed values from 'escaping' out of the try block.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/MutableProperty.java#0 $) $Date: 10-nov-2005.18:53:04 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
// =-=AEW This interface might move outside of laf.base if it is deemed
// necessary.  Keep it here unless there's an important reason to expose
// it publicly.
@Deprecated
public interface MutableProperty extends Cloneable
{
  // =-=AEW Not sure this should be called clone, because the
  // behavior of this method may actually differ from the behavior
  // of a real clone()
  public Object clone();
}
