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
package org.apache.myfaces.trinidadinternal.io;

import javax.faces.context.ResponseWriter;
import javax.faces.context.ResponseWriterWrapper;

/**
 * ResponseWriter abstract base used to decorate another ResponseWriter.
 *
 * @Deprecated Use ResponseWriterWrapper direcly
 */
abstract public class ResponseWriterDecorator extends ResponseWriterWrapper
{
  /**
   * Create a ResponseWriterDecorator.
   * 
   * @param decorated the decorated ResponseWriter
   */
  public ResponseWriterDecorator(ResponseWriter decorated)
  {
    _decorated = decorated;
  }

  /**
   * Return the decorated ResponseWriter.
   */
  protected ResponseWriter getResponseWriter()
  {
    return _decorated;
  }

  public String toString()
  {
    return super.toString() + "[" + _decorated.toString() + "]";
    
  }
  
  @Override
  public ResponseWriter getWrapped()
  {
    return getResponseWriter();
  }  

  private final ResponseWriter _decorated;

}
