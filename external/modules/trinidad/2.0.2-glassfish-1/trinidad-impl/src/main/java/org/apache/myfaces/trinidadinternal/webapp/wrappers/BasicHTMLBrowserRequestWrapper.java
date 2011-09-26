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

package org.apache.myfaces.trinidadinternal.webapp.wrappers;

import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;

/*
 * This class decodes the encoded parameter names and values for Non-JavaScript  
 * browsers. It also includes decoded parameter values into the HttpServletRequest.
 */

public class BasicHTMLBrowserRequestWrapper extends HttpServletRequestWrapper
{

  public BasicHTMLBrowserRequestWrapper(HttpServletRequest request)
  {
    super(request);
    initializeDecodedParamterMap();
    mergeParameterMap();  
  }
    
  /* 
   * This method decodes encoded parameters and initializes a hash map. 
   * Renderer encodes necessary data in the name attribute of submitting 
   * elements. There is only one successful submitting element in a request,
   * and thus there will be only one paramter name that contains encoded 
   * parameters per payload. 
   * It searches for such paramter name and it decodes it into an array
   * that is an alternating sequece of names and values.
   * Finally, it populates the hash map of name-value pairs from the array.
   */
     
  void initializeDecodedParamterMap()
  {
    Enumeration enumeration = super.getParameterNames();
    String paramName;
    String paramDetail[] = null;
    boolean findSubmit = true;

    while (enumeration.hasMoreElements() && findSubmit)
    {
      paramName = (String)enumeration.nextElement();
      
      // Search for name attribute that contains encoded parameters 
      if (paramName.indexOf(XhtmlConstants.NO_JS_PARAMETER_KEY) != -1)
      {
        encodedParameterName = paramName;
        paramDetail = paramName.split(XhtmlConstants.NO_JS_PARAMETER_KEY);
        findSubmit = false;
      }
      
      // If a page is submitted by an input element of type 'image', browser 
      // will generate two parameter names from the element's name attribute  
      // that contains encoded parameters. Each of the parameter name generated 
      // represents the coordinate of the image that was activated. Example,
      // if the name attribute of the element is 'paramName', browser will 
      // create two parameter names like 'paramName.x' and 'paramName.y'.
      else if (paramName.indexOf(XhtmlConstants.NO_JS_INPUT_IMAGE_KEY) != -1)
      {
        encodedParameterName = paramName;
        
        // Remove '.x' or '.y' from the parameter name before decoding
        paramName = paramName.substring(0, paramName.length()-2);  
        paramDetail = paramName.split(XhtmlConstants.NO_JS_INPUT_IMAGE_KEY);
        findSubmit = false;
      }
    }
    
    decodedParamMap = new HashMap<String, String[]>();
      
    //  paramDetail array contains parameter name and value in the following  
    //  order {name1, value1, name2, value2, ... nameN, valueN}
    for (int i = 0; i < paramDetail.length; i = i + 2)
    {
      decodedParamMap.put(paramDetail[i], new String[]{ paramDetail[i + 1]});
    }
  
    if(decodedParamMap.containsKey(XhtmlConstants.MULTIPLE_VALUE_PARAM)) 
    {
      paramName  = decodedParamMap.
                                 get(XhtmlConstants.MULTIPLE_VALUE_PARAM)[0];
                                 
      if (!decodedParamMap.containsKey(XhtmlConstants.SOURCE_PARAM)) 
      {
        decodedParamMap.put(XhtmlConstants.SOURCE_PARAM, 
                          new String[]{ super.getParameter(paramName)} );
      }
      else 
      {
        decodedParamMap.put(XhtmlConstants.VALUE_PARAM,
                            new String[]{ super.getParameter(paramName)} );
      }
    }
  }   
    
  /*
   * This methods merges payLoad's parameterMap with the decoded parameter names  
   * and values
   */
   
  void mergeParameterMap()
  {
    Map<String, String[]> originalMap = super.getParameterMap();
    modifiableParameterMap = new HashMap<String, String[]>(originalMap);
   
    // Since not needed anymore, remove the parameter name containing 
    // the encoded parameters 
    modifiableParameterMap.remove(encodedParameterName);
   
    //Update with decoded parameter map
    modifiableParameterMap.putAll(decodedParamMap);
  }
        
  /**
   * Returns an array of <code>String</code> objects containing
   * all the values of a given parameter.
   *
   * @param param a <code>String</code> containing the name of
   *  the parameter whose value is requested
   *
   * @return an array of <code>String</code> objects
   *  containing the parameter's values
   */      
        
  @Override
  public String[] getParameterValues(String param)
  { 
    return modifiableParameterMap.get(param);
  }
  
  /**
   * @param param  a <code>String</code> specifying the
   *  name of the parameter
   *
   * @return a <code>String</code> representing the
   *  single value of the parameter
   */
  
  @Override
  public String getParameter(String param)
  {
    String[] paramValue = getParameterValues(param);
        
    if (paramValue == null)
    { 
      return null;
    }
    
    return paramValue[0];
  }
  
  /**
   * @return a <code>Map</code> containing parameter names
   * as <code>String</code> objects and modified parameter values as 
   * <code>String[]</code arrays
   */

  @Override
  public Map<String, String[]> getParameterMap()
  { 
    return Collections.unmodifiableMap(modifiableParameterMap);
  }
  
  /**
   * Returns a <code>Enumeration</code> of <code>String</code>
   * objects containing the names of the parameters contained
   * in this request. 
   *
   * @return a <code>Enumeration</code> of <code>String</code>
   * objects, each <code>String</code> represent
   * the parameter
   * name of this request; 
   */

  @Override
  public Enumeration<String> getParameterNames()
  {
    return Collections.enumeration(modifiableParameterMap.keySet());
  }
  
  // The parameter name in the payLoad that contains the encoded parameter name 
  // and value pair for Non-JavaScript browsers
  private String encodedParameterName;
  
  // This map stores the decoded parameter name and value pair.
  private Map<String, String[]> decodedParamMap;
  
  // This map is the integration of decodedParamMap and payLoad's parameterMap
  private Map<String, String[]> modifiableParameterMap;  
}
