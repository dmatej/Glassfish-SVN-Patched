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

import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadinternal.image.cache.FileSystemImageCache;

/**
 * Configuration class that works off of a RequestContext.
 */
public class ContextBasedConfiguration extends ConfigurationImpl
{
  public ContextBasedConfiguration(FacesContext fContext,
                                   RequestContext context)
  {
    super();
    putProperty(SKIN_FAMILY,
                context.getSkinFamily());

    setDebug(context.isDebugOutput());

    String disableStandardsMode = 
      fContext.getExternalContext().getInitParameter(_DISABLE_STANDARDS_MODE);
    if ((disableStandardsMode != null) &&
        disableStandardsMode.equalsIgnoreCase("true"))
    {
      putProperty(DISABLE_STANDARDS_MODE, Boolean.TRUE);
    }

    // Backdoor hack to completely disable image generation for
    // testing purposes
    String blockImageGeneration = 
      fContext.getExternalContext().getInitParameter(_BLOCK_IMAGE_GENERATION);
    if ((blockImageGeneration != null) &&
        blockImageGeneration.equalsIgnoreCase("true"))
    {
      putProperty(FileSystemImageCache.BLOCK_IMAGE_GENERATION, Boolean.TRUE);
    }
  }

  static private final String _DISABLE_STANDARDS_MODE= "org.apache.myfaces.trinidad.ENABLE_QUIRKS_MODE";
  static private final String _BLOCK_IMAGE_GENERATION= "org.apache.myfaces.trinidadinternal.BLOCK_IMAGE_GENERATION";
}
