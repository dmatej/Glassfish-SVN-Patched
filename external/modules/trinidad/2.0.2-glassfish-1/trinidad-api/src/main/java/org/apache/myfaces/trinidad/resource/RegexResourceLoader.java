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
package org.apache.myfaces.trinidad.resource;

import java.io.IOException;
import java.net.URL;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * A resource loader implementation which loads resources
 * by pattern matching the requested resource path to a
 * registered resource loader.
 * 
 * Change history
 * 2006-08-01: -= Simon Lessard =-
 *             Changed to use a list of entry rather than a dual typed
 *             one to add more type safety with minimal memory overhaul
 *             and get a really small performance gain. 
 *
 */
public class RegexResourceLoader extends ResourceLoader
{
  /**
   * Creates a new RegexResourceLoader.
   */
  public RegexResourceLoader()
  {
    _loaders = new ArrayList<RegexResourceNode>();
  }
  
  @Override
  protected URL findResource(
    String path) throws IOException
  {

    // a RegexResourceNode contains a ResourceLoader 
    // (e.g., a CoreClassLoaderResourceLoader) and a Pattern.
    // loop through all the RegExResourceNodes that have been registered
    // with the RegexResourceLoader's register(regex, loader) method, 
    // and find the node where the path fits the node's pattern.
    // Once the node is found, return the resource with the given name
    for(RegexResourceNode node : _loaders)
    {
      Matcher matcher = node.getPattern().matcher(path);
      if (matcher.matches())
      {
        return node.getResourceLoader().getResource(matcher.group(1));
      }
    }
    
    return null;
  }
  
  /**
   * Registers a resource loader by regular expression.
   * 
   * @param regex  the regular expression to match
   * @param loader  the resource loader to use for matching paths
   */
  protected void register(
    String         regex,
    ResourceLoader loader)
  {
    Pattern pattern = Pattern.compile(regex);
    _checkPathRegex(regex);
    _loaders.add(new RegexResourceNode(pattern,loader));
  }
  
  /**
   * Deregisters a resource loader by regular expression.
   * 
   * @param regex  the regular expression to remove
   */
  protected void deregister(
    String regex)
  {
    // -= Simon Lessard =- 
    // Regex compilation can be expensive and the variable 
    // is only used for equals purpose, so use the other way 
    // around instead, that is get the expression out of the 
    // compiled patterns through Pattern.pattern().
    // Pattern pattern = Pattern.compile(regex);

    Iterator<RegexResourceNode> nodeIterator = _loaders.iterator();
    while(nodeIterator.hasNext())
    {
      if(regex.equals(nodeIterator.next().getPattern().pattern()))
      {
        nodeIterator.remove();
        return;
      }
    }
  }

  /**
   * Verify that the regular expression will match only paths with a 
   * leading slash.
   * 
   * @param regex  the regular expression to verify
   */
  private void _checkPathRegex(
    String regex)
  {
    if (!regex.startsWith("/")  &&
        !regex.startsWith("(/"))
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "RESOURCE_PATH_REGULAR_EXPRESSION_HAS_NO_LEADING_SLASH", regex));
    }
  }
  
  private static class RegexResourceNode
  {
    public RegexResourceNode(Pattern pattern, ResourceLoader loader)
    {
      _pattern = pattern;
      _loader  = loader;
    }
    
    public Pattern getPattern()
    {
      return _pattern;
    }
    
    public ResourceLoader getResourceLoader()
    {
      return _loader;
    }

    private ResourceLoader _loader;
    private Pattern _pattern;
  }

  private final List<RegexResourceNode> _loaders;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    RegexResourceLoader.class);
}
