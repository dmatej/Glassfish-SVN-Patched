package org.apache.myfaces.trinidad.resource;

import java.net.URL;

import java.util.Collections;
import java.util.Iterator;

import javax.faces.context.ExternalContext;

/**
 * Non-Trinidad skin resource loader implementations should extend this class and override
 * findResources to specify where from where to load skin resources.
 * We will find all overridden classes by calling
 * <pre>
 * List&lt;SkinResourceLoader> urlProviders = ClassLoaderUtils.getServices(
 *                                 "org.apache.myfaces.trinidad.resource.SkinResourceLoader");
 * </pre>
 */
public class SkinResourceLoader
{

  /**
   * Returns an iterator of URL objects representing all the resources with the given name.
   * @param context The ExternalContext
   * @param filename The filename of the resource to find, e.g., "trinidad-skins.xml"
   * @return An iterator of URL objects for the resources.
   *  Returns an empty iterator if no resources were found.
   */
  public Iterator<URL> findResources(ExternalContext context, String filename)
  {
      return Collections.<URL>emptyList().iterator();
  }
}