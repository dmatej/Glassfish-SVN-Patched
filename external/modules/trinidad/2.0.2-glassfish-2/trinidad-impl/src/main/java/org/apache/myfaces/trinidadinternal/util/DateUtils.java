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
package org.apache.myfaces.trinidadinternal.util;

import java.util.HashMap;
import java.util.TimeZone;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.config.ConfigParser;

/**
 * Utility class for Date related functions. 
 */
public final class DateUtils
{

    //empty, private constructor
    private DateUtils()
    {
    }

    public static TimeZone getSupportedTimeZone (String id) 
    {
      // If the timezone was specified using String, it may
      //not be in the correct case and TimeZone.getTimeZone(..) doesn't give
      //any warning, just returning GMT (on Unix)
      String officialId = _supportedTimeZoneIdsMap.get(id.toLowerCase());
      
      if (officialId != null)
      {
        // The ID was found in the map of TimeZone.getAvailableIDs(), so 
        // TimeZone.getTimeZone(id) should succeed
        return (TimeZone.getTimeZone(officialId));        
      }
      
      _LOG.warning("CANNOT_FIND_TIMEZONE", id);
      return null;
    }
    
    static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(DateUtils.class);
    private static HashMap<String, String> _supportedTimeZoneIdsMap;

    static{
        String[] supportedIds = TimeZone.getAvailableIDs();
        _supportedTimeZoneIdsMap = new HashMap<String, String> (supportedIds.length);
        for (int i = 0; i < supportedIds.length; i++)
        {
            String id = supportedIds[i];
            _supportedTimeZoneIdsMap.put (id.toLowerCase(), id);
        }      
    }
}
