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
package org.apache.myfaces.trinidadinternal.agent.parse;

/**
 * XML Names used in the capabilties file
 */
interface XMLConstants
{
  public static final String NS_URI = "http://myfaces.apache.org/trinidad/agent/capabilities";
  public static final String ELEMENT_ROOT = "capabilitiesDocument";
  public static final String ELEMENT_AGENT_CAPABILITIES = "agentCapabilities";
  public static final String ELEMENT_CAPABILITIES = "capabilities";
  public static final String ELEMENT_INCLUDE = "include";
  public static final String ELEMENT_DEVICES = "devices";
  public static final String ELEMENT_DEVICE = "device";
  public static final String ELEMENT_COMPONENT = "component";
  public static final String ATTRIBUTE_ID = "id";
  public static final String ATTRIBUTE_REFID = "refid";
  public static final String ATTRIBUTE_SRC = "src";
  public static final String ATTRIBUTE_AGENTS = "agents";
  public static final String ATTRIBUTE_PLATFORMS = "platforms";
  public static final String ATTRIBUTE_DEFAULT = "default";
  public static final String ATTRIBUTE_MODEL = "model";
  public static final String ATTRIBUTE_EXTENDS = "extends";
  public static final String ATTRIBUTE_TYPE = "type";

  public static final String ELEMENT_CAPABILITY_DATA = "capabilityData";
  public static final String ELEMENT_CAPABILITY = "capability";
  public static final String ATTRIBUTE_NAME = "name";
  public static final String ATTRIBUTE_VALUE = "value";

}
