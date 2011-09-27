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
package org.apache.myfaces.trinidadinternal.context;

import java.util.Locale;
import java.util.TimeZone;

import org.apache.myfaces.trinidad.bean.FacesBeanImpl;
import org.apache.myfaces.trinidad.bean.PropertyKey;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.RequestContext;

public class RequestContextBean extends FacesBeanImpl
{
  static public final Type TYPE = new Type();
  static public final PropertyKey PAGE_FLOW_SCOPE_LIFETIME_KEY =
    TYPE.registerKey("page-flow-scope-lifetime",
                     Integer.class,
                     PropertyKey.CAP_NOT_BOUND);
  static public final PropertyKey DEBUG_OUTPUT_KEY =
    TYPE.registerKey("debug-output", Boolean.class);
  static public final PropertyKey CLIENT_VALIDATION_KEY =
    TYPE.registerKey("client-validation", RequestContext.ClientValidation.class);
  static public final PropertyKey CLIENT_VALIDATION_DISABLED_KEY =
    TYPE.registerKey("client-validation-disabled",
                     Boolean.class,
                     PropertyKey.CAP_NOT_BOUND);
  static public final PropertyKey OUTPUT_MODE_KEY =
    TYPE.registerKey("output-mode");
  static public final PropertyKey LOOK_AND_FEEL_KEY =
    TYPE.registerKey("look-and-feel");
  static public final PropertyKey ACCESSIBILITY_MODE_KEY =
    TYPE.registerKey("accessibility-mode");
  static public final PropertyKey ACCESSIBILITY_PROFILE_KEY =
    TYPE.registerKey("accessibility-profile",
                     AccessibilityProfile.class);
  static public final PropertyKey RIGHT_TO_LEFT_KEY =
    TYPE.registerKey("right-to-left", Boolean.class);
  static public final PropertyKey FORMATTING_LOCALE_KEY =
    TYPE.registerKey("formatting-locale", Locale.class);
  static public final PropertyKey NUMBER_GROUPING_SEPARATOR_KEY =
    TYPE.registerKey("number-grouping-separator", Character.class);
  static public final PropertyKey DECIMAL_SEPARATOR_KEY =
    TYPE.registerKey("decimal-separator", Character.class);
  static public final PropertyKey CURRENCY_CODE_KEY =
    TYPE.registerKey("currency-code");
  static public final PropertyKey TIME_ZONE_KEY =
    TYPE.registerKey("time-zone", TimeZone.class);
  static public final PropertyKey ORACLE_HELP_SERVLET_URL_KEY =
    TYPE.registerKey("oracle-help-servlet-url");  
  static public final PropertyKey TWO_DIGIT_YEAR_START =
    TYPE.registerKey("two-digit-year-start", Integer.class);
  static public final PropertyKey SKIN_FAMILY_KEY =
    TYPE.registerKey("skin-family");
  static public final PropertyKey SKIN_VERSION_KEY =
    TYPE.registerKey("skin-version");   
  static public final PropertyKey UPLOADED_FILE_PROCESSOR_KEY = 
    TYPE.registerKey("uploaded-file-processor",
                     PropertyKey.CAP_NOT_BOUND);
  static public final PropertyKey UPLOADED_FILE_MAX_MEMORY_KEY = 
    TYPE.registerKey("uploaded-file-max-memory", Long.class);
  static public final PropertyKey UPLOADED_FILE_MAX_DISK_SPACE_KEY = 
    TYPE.registerKey("uploaded-file-max-disk-space", Long.class);
  static public final PropertyKey UPLOADED_FILE_TEMP_DIR_KEY = 
    TYPE.registerKey("uploaded-file-temp-dir");
  static public final PropertyKey REMOTE_DEVICE_REPOSITORY_URI =
      TYPE.registerKey("remote-device-repository-uri");                       
  static public final PropertyKey ANIMATION_ENABLED_KEY =
    TYPE.registerKey("animation-enabled", Boolean.class, true);
  static
  {
    TYPE.lock();
  }

  @Override
  public Type getType()
  {
    return TYPE;
  }

}
