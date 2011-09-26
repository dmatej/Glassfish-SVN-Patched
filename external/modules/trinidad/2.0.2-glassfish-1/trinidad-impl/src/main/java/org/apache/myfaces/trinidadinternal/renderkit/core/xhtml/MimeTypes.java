/*
 * MimeTypes.java
 *
 * Created 15-Mar-07 10:21:33 PM
 * 
 * Copyright  2004-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

/**
 * This class exposes known MIME types as constants to prevent typos and make 
 * it easier to lookup for MIME type.
 */
public final class MimeTypes
{
  public static final String CSS = "text/css";
  public static final String HTML = "text/html";
  public static final String JAVASCRIPT = "text/javascript";
  public static final String X_JAVASCRIPT = "text/x-javascript";
  public static final String XML = "text/xml";
  
  private MimeTypes(){}
}
