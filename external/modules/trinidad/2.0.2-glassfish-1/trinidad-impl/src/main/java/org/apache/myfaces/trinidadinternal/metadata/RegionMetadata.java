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
package org.apache.myfaces.trinidadinternal.metadata;

import java.io.IOException;
import java.io.InputStream;

import java.io.StringReader;
import java.net.URL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.context.FacesContext;

import javax.servlet.ServletContext;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidad.config.RegionConfig;
import org.apache.myfaces.trinidad.config.RegionManager;
import org.apache.myfaces.trinidadinternal.ui.data.bind.ConvertBoundValue;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * This class contains metadata for region components.
 */
public final class RegionMetadata extends RegionManager
{
  private RegionMetadata()
  {
    _map = Collections.emptyMap();
  }
  
  @Override
  public RegionConfig getRegionConfig(String regionType)
  {
    return _map.get(regionType);
  }

  @Override
  public Map<String, RegionConfig> getRegionConfigs()
  {
    return Collections.unmodifiableMap(_map);
  }

  /**
   * Gets the RegionMetadata.
   * Initially this will parse the region metadata files to create and cache
   * an instance of this metadata object.
   * All resources with the name "/META-INF/region-metadata.xml" will be searched.
   * /WEB-INF/region-metadata.xml will also be searched.
   */
  @SuppressWarnings("unchecked")
  public static synchronized RegionMetadata getRegionMetadata(
    FacesContext context)
  {
    Map<String, Object> appMap = context.getExternalContext().getApplicationMap();
    RegionMetadata bean = (RegionMetadata) appMap.get(_KEY);
    if (bean == null)
    {
      bean = new RegionMetadata();
      appMap.put(_KEY, bean);
      ClassLoader loader = Thread.currentThread().getContextClassLoader();
      _readAllRegionMDFiles(loader, bean, __CONFIG_FILE_OTHER);
      _readAllRegionMDFiles(loader, bean, _CONFIG_FILE);

      Object sContext = context.getExternalContext().getContext();
      if (sContext instanceof ServletContext)
      {
        ServletContext servletContext = (ServletContext) sContext;
        InputStream in = servletContext.getResourceAsStream(_CONFIG_FILE_2);
        if (in != null)
        {
          _readRegionMetadata(bean, in, _CONFIG_FILE_2);
          try
          {
            in.close();
          }
          catch (IOException e)
          {
            _LOG.severe("ERR_CLOSING_FILE",_CONFIG_FILE_2);
            _LOG.severe(e);
          }
        }
      }
    }
    return bean;
  }

  /**
   * Invalidates the cached RegionMetadata.
   * The next time {@link #getRegionMetadata} is called, it
   * will reparse all the region-metadata.xml files.
   */
  @SuppressWarnings("unchecked")
  public static synchronized void invalidate(FacesContext context)
  {
    Map<String, Object> appMap = context.getExternalContext().getApplicationMap();
    appMap.remove(_KEY);
  }

  private static void _readAllRegionMDFiles(
    ClassLoader loader,
    RegionMetadata bean,
    String regionMDFile)
  {
    if (_LOG.isFinest())
    {
      _LOG.finest("searching for region-metadata with resource:{0}", regionMDFile);
    }
    try
    {
      Enumeration<URL> files = loader.getResources(regionMDFile);
      while(files.hasMoreElements())
      {
        URL url = files.nextElement();
        String publicId = url.toString();
        try
        {
          InputStream in = url.openStream();
          _readRegionMetadata(bean, in, publicId);
          in.close();
        }
        catch (IOException e)
        {
          _error(publicId, e);
        }
      }
    }
    catch (IOException e)
    {
      _LOG.warning("ERR_GET_REGION_METADATA_FILE",__CONFIG_FILE_OTHER);
      _LOG.warning(e);
    }
  }

  private static void _error(String publicId, Throwable t)
  {
    _LOG.warning("ERR_READ_REGION_METADATA_FILE", publicId);
    _LOG.warning(t);
  }

  private static void _readRegionMetadata(
    RegionMetadata bean, InputStream in, String publicId)
  {
    if (_LOG.isFiner())
    {
      _LOG.finer("Loading region-metadata from file:{0}", publicId);
    }
    try
    {
      InputSource input = new InputSource();
      input.setByteStream(in);
      input.setPublicId(publicId);

      DefaultHandler handler = new Handler(bean);
      _SAX_PARSER_FACTORY.newSAXParser().parse(input, handler);
    }
    catch (IOException ioe)
    {
      _error(publicId, ioe);
    }
    catch (ParserConfigurationException pce)
    {
      _error(publicId, pce);
    }
    catch (SAXException saxe)
    {
      _error(publicId, saxe);
    }
  }


  private void _put(String componentType, ComponentMetaData comp)
  {
    if (_LOG.isFine())
    {
      _LOG.fine("Associating jspUri {0} with componentType:{1}",
        new Object[] { comp.getJspUIDef(), componentType});
    }
    
    Map<String, RegionConfig> comparant = Collections.emptyMap();
    if (_map == comparant)
    {
      _map = new HashMap<String, RegionConfig>(5);
    }
    RegionConfig old = _map.put(componentType, comp);
    if (old != null)
    {
      _LOG.warning("REPLACE_COMPONENTTYPE_JSPURI", new Object[] {old.getJspUIDef(), comp.getJspUIDef(), componentType});
    }
  }


  static private class Handler extends DefaultHandler
  {
    public Handler(RegionMetadata bean)
    {
      _bean = bean;
    }

    @Override
    public InputSource resolveEntity (String publicId, String systemId)
    	throws SAXException
    {
      // at RT we don't care about DTDs. We must return something to prevent
      // the parser from making a network connection to resolve DTDs.
      return new InputSource(new StringReader(""));
    }

    @Override
    public void setDocumentLocator(Locator locator)
    {
      super.setDocumentLocator(locator);
      _loc = locator;
    }

    @Override
    public void startElement(String uri,
                             String localName,
                             String qName,
                             Attributes atts)
    {
      switch(_state)
      {
        case _STATE_INIT:
          if (!_changeState(localName, "faces-config", _STATE_FACES_CONFIG))
          {
            _unexpected(localName);
          }
          break;
        case _STATE_FACES_CONFIG:
          if (_changeState(localName, "component", _STATE_COMP))
          {
            assert _comp == null;
            _comp = new ComponentMetaData();
          }
          else
          {
            _skip(localName);
          }
          break;
        case _STATE_COMP:
          if (_changeState(localName, "attribute", _STATE_ATTR))
          {
            assert _attr == null;
            _attr = new AttributeMetaData();
          }
          else if (!(_changeState(localName, _COMP_TYPE_TAG, _STATE_COMP_TYPE) ||
                     _changeState(localName, "component-extension", _STATE_COMP_EXT)))
          {
            _skip(localName);
          }
          break;
        case _STATE_COMP_TYPE:
        case _STATE_ATTR_CLASS:
        case _STATE_ATTR_NAME:
        case _STATE_REQUIRED:
        case _STATE_DEFAULT_VALUE:
        case _STATE_REGION_DEF:
          _unexpected(localName);
          break;
        case _STATE_COMP_EXT:
          if (!_changeState(localName, _REGION_DEF_JSP_TAG, _STATE_REGION_DEF))
            _skip(localName);
          break;
        case _STATE_ATTR:
          if (!(_changeState(localName, "attribute-name", _STATE_ATTR_NAME)     ||
                _changeState(localName, "attribute-class", _STATE_ATTR_CLASS)   ||
                _changeState(localName, "attribute-extension", _STATE_ATTR_EXT) ||
                _changeState(localName, "default-value", _STATE_DEFAULT_VALUE)))
          {
            _skip(localName);
          }
          break;
        case _STATE_ATTR_EXT:
          _changeState(localName, "required", _STATE_REQUIRED);
          break;
        case _STATE_SKIP:
          if (localName.equals(_skipElement))
          {
            throw new UnsupportedOperationException(_LOG.getMessage(
              "REGION_METADATA_CANNOT_NEST",localName));
          }
          break;
      }
    }

    /**
     * Checks if the localName matches the tag. If they match the state of
     * the fsm is changed, and true is returned. otherwise false is returned.
     */
    private boolean _changeState(String localName, String tag, int newState)
    {
      if (tag.equals(localName))
      {
        _state = newState;
        return true;
      }
      return false;
    }

    @Override
    public void characters(char[] ch, int start, int length)
    {
      switch(_state)
      {
        case _STATE_COMP_TYPE:
        case _STATE_ATTR_CLASS:
        case _STATE_ATTR_NAME:
        case _STATE_REQUIRED:
        case _STATE_DEFAULT_VALUE:
        case _STATE_REGION_DEF:
          _currentText.append(ch, start, length);
          break;
      }
    }

    @Override
    public void endElement(String uri,
                           String localName,
                           String qName)
    {
      switch(_state)
      {
        case _STATE_SKIP:
          if (localName.equals(_skipElement))
          {
            _skipElement = null;
            _state = _returnState;
          }
          break;
        case _STATE_COMP_TYPE:
          _comp._regionType = _getCurrentText(_STATE_COMP);
          break;
        case _STATE_REGION_DEF:
          _comp._jsp = _getCurrentText(_STATE_COMP_EXT);
          break;
        case _STATE_COMP_EXT:
          _state = _STATE_COMP;
          break;
        case _STATE_COMP:
          _addComponentMetadata();
          _comp = null;
          _state = _STATE_FACES_CONFIG;
          break;
        case _STATE_ATTR:
          _comp._addAttribute(_attr);
          _attr = null;
          _state = _STATE_COMP;
          break;
        case _STATE_REQUIRED:
          _attr._required = _getCurrentText().equals("true");
          _state = _STATE_ATTR_EXT;
          break;
        case _STATE_ATTR_CLASS:
          String javaType = _getCurrentText(_STATE_ATTR);
          _attr._class = _getClass(javaType);
          break;
        case _STATE_ATTR_NAME:
          _attr._name = _getCurrentText(_STATE_ATTR);
          break;
        case _STATE_DEFAULT_VALUE:
          _attr._default = _getCurrentText(_STATE_ATTR);
          break;
        case _STATE_ATTR_EXT:
          _state = _STATE_ATTR;
          break;
      }
    }

    private void _addComponentMetadata()
    {
      if (_comp._regionType == null)
      {
        _missing(_COMP_TYPE_TAG);
        return;
      }
      if (_comp.getJspUIDef() == null)
      {
        _missing(_REGION_DEF_JSP_TAG);
        return;
      }
      _bean._put(_comp._regionType, _comp);
      _comp._trim(); // no more attributes will be added, so trim-to-size
    }

    private Class<?> _getClass(String javaType)
    {
      String className = ConvertBoundValue.getClassName(javaType);
      try
      {
        return Thread.currentThread().getContextClassLoader().loadClass(className);
      }
      catch (ClassNotFoundException e)
      {
        _log(e);

      }
      return Object.class;
    }

    private String _getCurrentText(int newState)
    {
      _state = newState;
      return _getCurrentText();
    }

    private String _getCurrentText()
    {
      String result = _currentText.toString();
      result = result.trim();
      _currentText.setLength(0);
      return result;
    }

    private void _unexpected(String localName)
    {
      if (_LOG.isWarning())
      {
        _LOG.warning("UNKNOWN_ELEMENT", new Object[] {localName, _getLocation()});
      }
      _skip(localName);
    }

    private void _missing(String tag)
    {
      _LOG.severe("MISSING_AT", new Object[] {tag,_getLocation()});
    }

    private void _log(Throwable t)
    {
      _LOG.severe("EXCEPTION_AT",_getLocation());
      _LOG.severe(t);
    }

    private String _getLocation()
    {
      return "line:"+_loc.getLineNumber()+
        " column:"+_loc.getColumnNumber() +
        " file:" + _loc.getPublicId();
    }

    private void _skip(String localName)
    {
      _skipElement = localName;
      _returnState = _state;
      _state = _STATE_SKIP;
    }


    private StringBuffer _currentText = new StringBuffer(50);
    private String _skipElement = null;
    private int _returnState = _STATE_INIT;
    private int _state = _STATE_INIT;
    private AttributeMetaData _attr = null;
    private ComponentMetaData _comp = null;
    private final RegionMetadata _bean;
    private Locator _loc = null;
  }

  public static final class AttributeMetaData
  {
    private String _default;
    private String _name;
    private Class<?> _class;
    private boolean _required = false;

    public String getAttrName()
    {
      return _name;
    }

    public Class<?> getAttrClass()
    {
      return _class;
    }

    public String getDefaultValue()
    {
      return _default;
    }

    public boolean isRequired()
    {
      return _required;
    }
  }

  public static final class ComponentMetaData extends RegionConfig
  {
    public ComponentMetaData()
    {
      _attrs = Collections.emptyList();
    }

    @Override
    public String getDescription()
    {
      return null;
    }

    @Override
    public String getDisplayName()
    {
      return null;
    }

    @Override
    public String getComponentType()
    {
      return _regionType;
    }

    @Override
    public String getJspUIDef()
    {
      return _jsp;
    }

    public List<AttributeMetaData> getAttributes()
    {
      return _attrs;
    }

    private void _addAttribute(AttributeMetaData attr)
    {
      List<AttributeMetaData> comparant = Collections.emptyList();
      if (_attrs == comparant)
      {
        _attrs = new ArrayList<AttributeMetaData>(5);
      }
      _attrs.add(attr);
    }

    public void _trim()
    {
      if (_attrs instanceof ArrayList)
        ((ArrayList) _attrs).trimToSize();
    }
    
    private String _jsp;
    private String _regionType;
    private List<AttributeMetaData> _attrs;
  }

  private Map<String, RegionConfig> _map;

  private static final int _STATE_INIT = 0;
  private static final int _STATE_SKIP = 10;
  private static final int _STATE_COMP = 20;
  private static final int _STATE_COMP_TYPE = 30;
  private static final int _STATE_COMP_EXT = 40;
  private static final int _STATE_REGION_DEF = 50;
  private static final int _STATE_ATTR = 60;
  private static final int _STATE_ATTR_NAME = 70;
  private static final int _STATE_ATTR_CLASS = 80;
  private static final int _STATE_ATTR_EXT = 90;
  private static final int _STATE_FACES_CONFIG = 100;
  private static final int _STATE_DEFAULT_VALUE = 110;
  private static final int _STATE_REQUIRED = 120;

  private static final String _REGION_DEF_JSP_TAG = "region-jsp-ui-def";
  private static final String _COMP_TYPE_TAG = "component-type";

  // when classpath points to a file system path,
  // having a leading "/" causes problems:
  private static final String _CONFIG_FILE = "META-INF/region-metadata.xml";

  private static final String _CONFIG_FILE_2 = "/WEB-INF/region-metadata.xml";
  // package private for testing purposes:
  // the leading "/" is needed when classpath points to a jar file:
  static final String __CONFIG_FILE_OTHER = "/META-INF/region-metadata.xml";
  
  private static final SAXParserFactory _SAX_PARSER_FACTORY;
  static
  {
      _SAX_PARSER_FACTORY = SAXParserFactory.newInstance();
      _SAX_PARSER_FACTORY.setNamespaceAware(true);
  } 
  
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(RegionMetadata.class);
  private static final String _KEY = RegionMetadata.class.getName();
}
