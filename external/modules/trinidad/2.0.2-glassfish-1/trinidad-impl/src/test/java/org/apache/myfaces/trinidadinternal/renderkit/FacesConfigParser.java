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
package org.apache.myfaces.trinidadinternal.renderkit;

import javax.faces.render.Renderer;
import javax.faces.render.RenderKit;

import java.lang.reflect.Array;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.apache.myfaces.trinidadinternal.share.expl.Coercions;
import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.StringParser;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

class FacesConfigParser extends BaseNodeParser
{
  public FacesConfigParser(FacesConfigInfo info)
  {
    _info = info;
  }
  
  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    if ("render-kit".equals(localName))
      return new RenderKitParser(_info);
    if ("component".equals(localName))
      return new ComponentParser();
    if ("converter".equals(localName))
      return new ConverterParser();
    if ("factory".equals(localName))
      return new FactoryParser(_info);

    return BaseNodeParser.getIgnoreParser();
  }

  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child) throws SAXParseException
  {
    if ("component".equals(localName))
    {
      FacesConfigInfo.ComponentInfo info = (FacesConfigInfo.ComponentInfo) child;
      _info.addComponent(info);
    }
  }
  

  private class RenderKitParser extends BaseNodeParser
  {
    public RenderKitParser(FacesConfigInfo info)
    {
      _info = info;
    }

    @Override
    public NodeParser startChildElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      if ("render-kit-id".equals(localName))
        return new StringParser();
      if ("render-kit-class".equals(localName))
        return new StringParser();
      if ("renderer".equals(localName))
        return new RendererParser();

      return BaseNodeParser.getIgnoreParser();
    }
    
    @Override
    public void addCompletedChild(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Object       child) throws SAXParseException
    {
      if ("render-kit-id".equals(localName))
        _renderKitId = (String) child;
      else if ("render-kit-class".equals(localName))
      {
        try
        {
          _kit = (RenderKit) Class.forName((String) child).newInstance();
        }
        catch (Exception e)
        {
          throw new SAXParseException("Couldn't create RenderKit", context.getLocator(), e);
        }
      }
      else if ("renderer".equals(localName))
      {
        RendererInfo info = (RendererInfo) child;
        try
        {
          Renderer renderer = (Renderer)
            Class.forName(info.rendererClass).newInstance();
          _kit.addRenderer(info.componentFamily, info.rendererType, renderer);
        }
        catch (Exception e)
        {
          System.err.println("Couldn't create renderer " + info.rendererClass);
          //          throw new SAXParseException("Couldn't create Renderer", context.getLocator(), e);
        }
      }
    }
    
    @Override
    public Object endElement(
      ParseContext context,
      String       namespaceURI,
      String       localName) throws SAXParseException
    {
      _info.getRenderKits().put(_renderKitId, _kit);
      return null;
    }

    private FacesConfigInfo _info;
    private String    _renderKitId;
    private RenderKit _kit;
  }

  private class FactoryParser extends BaseNodeParser
  {
    public FactoryParser(FacesConfigInfo info)
    {
      _info = info;
    }

    @Override
    public NodeParser startChildElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      if (FacesConfigInfo.isFactorySupported(localName))
        return new StringParser();

      return BaseNodeParser.getIgnoreParser();
    }
    
    @Override
    public void addCompletedChild(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Object       child) throws SAXParseException
    {
      if (FacesConfigInfo.isFactorySupported(localName))
      {
        String factoryName = (String) child;
        if (factoryName != null)
        {
          factoryName = factoryName.trim();
          if (!"".equals(factoryName))
            _info.getFactoriesByName(localName).add(factoryName);
        }
      }
    }
    
    private FacesConfigInfo _info;
  }


  private class ComponentParser extends BaseNodeParser
  {
    @Override
    public NodeParser startChildElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      if ("component-type".equals(localName) ||
          "component-class".equals(localName))
        return new StringParser();
      else if ("property".equals(localName))
      {
        return new PropertyParser(_info);
      }

      return BaseNodeParser.getIgnoreParser();
    }
    
    @Override
    public void addCompletedChild(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Object       child) throws SAXParseException
    {
      if (child instanceof String)
      {
        String s = (String) child;
        if ("component-type".equals(localName))
          _info.componentType = s;
        else if ("component-class".equals(localName))
          _info.componentClass = s;
      }
    }

    @Override
    public Object endElement(
      ParseContext context,
      String       namespaceURI,
      String       localName) throws SAXParseException
    {
      return _info;
    }
    
    private FacesConfigInfo.ComponentInfo _info = new FacesConfigInfo.ComponentInfo();
  }


  private class ConverterParser extends BaseNodeParser
  {
    public ConverterParser()
    {
    }

    @Override
    public NodeParser startChildElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      if ("converter-id".equals(localName) ||
          "converter-for-class".equals(localName) ||
          "converter-class".equals(localName))
        return new StringParser();

      return BaseNodeParser.getIgnoreParser();
    }
    
    @Override
    public void addCompletedChild(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Object       child) throws SAXParseException
    {
      if (child instanceof String)
      {
        String s = (String) child;
        if ("converter-id".equals(localName))
          _converterId = s;
        else if ("converter-for-class".equals(localName))
          _converterForClass = s;
        else if ("converter-class".equals(localName))
          _converterClass = s;
      }
    }

    @Override
    public Object endElement(
      ParseContext context,
      String       namespaceURI,
      String       localName) throws SAXParseException
    {
      if (_converterId != null)
        _info.addConverterById(_converterId, _converterClass);
      if (_converterForClass != null)
        _info.addConverterByType(_converterForClass, _converterClass);
      return null;
    }
    
    private String _converterId;
    private String _converterForClass;
    private String _converterClass;
  }

  private class PropertyParser extends BaseNodeParser
  {
    public PropertyParser(FacesConfigInfo.ComponentInfo info)
    {
      _info = info;
      _property = new FacesConfigInfo.PropertyInfo();
    }

    @Override
    public NodeParser startChildElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      if ("property-name".equals(localName) ||
          "property-class".equals(localName) ||
          "default-value".equals(localName) ||
          "property-values".equals(localName))
        return new StringParser();
      else if ("property-extension".equals(localName) ||
               "property-metadata".equals(localName))
      {
        return this;
      }

      return BaseNodeParser.getIgnoreParser();
    }
    
    @Override
    public void addCompletedChild(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Object       child) throws SAXParseException
    {
      if (child instanceof String)
      {
        String s = (String) child;
        if ("property-name".equals(localName))
        {
          _info.properties.put(s, _property);
        }
        else if ("property-class".equals(localName))
        {
          Class<?> c = _sPrimitiveTypes.get(s);
          if (c == null)
          {
            boolean isArray = s.endsWith("[]");
            if (isArray)
              s = s.substring(0, s.length() - 2);
              
            try
            {
              c = _loadClass(s);
              if (isArray)
                c = _getArrayType(c);
            }
            catch (Exception e)
            {
              logError(context, "Couldn't find class " + s, e);
            }
          }
          
          _property.type = c;
        }
        else if ("default-value".equals(localName))
        {
          Class<?> c = _property.type;
          if (c == null)
            c = String.class;
          _property.defaultValue = Coercions.coerce(null, s, c);
        }
        else if ("property-values".equals(localName))
        {
          _property.enumValues = new ArrayList<Object>();
          String[] values = XMLUtils.parseNameTokens(s);
          _property.enumValues.addAll(Arrays.asList(values));
        }
      }
    }

    @Override
    public Object endElement(
      ParseContext context,
      String       namespaceURI,
      String       localName) throws SAXParseException
    {
      return _info;
    }
    
    private FacesConfigInfo.ComponentInfo _info;
    private FacesConfigInfo.PropertyInfo  _property;
  }

  /**
   * Load a class, "trying harder" - supports "." syntax
   * to access inner classes
   */
  static private Class _loadClass(String s) throws ClassNotFoundException
  {
    try
    {
      return Class.forName(s);
    }
    catch (ClassNotFoundException cnfe)
    {
      // If the inner class works, great
      int lastPeriod = s.lastIndexOf('.');
      String ifInnerClass =
        s.substring(0, lastPeriod) + "$" + s.substring(lastPeriod + 1);
      try
      {
        return Class.forName(ifInnerClass);
      }
      catch (ClassNotFoundException cnfe2)
      {
        // Otherwise, throw the original exception
        throw cnfe;
      }
    }
  }

  private class RendererParser extends BaseNodeParser
  {
    @Override
    public NodeParser startChildElement(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Attributes   attrs) throws SAXParseException
    {
      if ("component-family".equals(localName) ||
          "renderer-type".equals(localName) ||
          "renderer-class".equals(localName))
        return new StringParser();
      return BaseNodeParser.getIgnoreParser();
    }
   
    @Override
    public void addCompletedChild(
      ParseContext context,
      String       namespaceURI,
      String       localName,
      Object       child) throws SAXParseException
    {
      if (child instanceof String)
      {
        String s = (String) child;
        if ("component-family".equals(localName))
          _info.componentFamily = s;
        else if ("renderer-type".equals(localName))
          _info.rendererType = s;
        else if ("renderer-class".equals(localName))
          _info.rendererClass = s;
      }
    }

    @Override
    public Object endElement(
      ParseContext context,
      String       namespaceURI,
      String       localName) throws SAXParseException
    {
      return _info;
    }

    private RendererInfo _info = new RendererInfo();
  }
  
  static class RendererInfo
  {
    public String componentFamily;
    public String rendererType;
    public String rendererClass;
  }

  private FacesConfigInfo _info;

  static private Class<?> _getArrayType(Class<?> baseClass)
  {
    return Array.newInstance(baseClass, 0).getClass();
  }

  static private final HashMap<String, Class<?>> _sPrimitiveTypes;
  static
  {
    _sPrimitiveTypes = new HashMap<String, Class<?>>();
    _sPrimitiveTypes.put("int[]",     _getArrayType(Integer.TYPE));
    _sPrimitiveTypes.put("boolean[]", _getArrayType(Boolean.TYPE));
    _sPrimitiveTypes.put("char[]",    _getArrayType(Character.TYPE));
    _sPrimitiveTypes.put("long[]",    _getArrayType(Long.TYPE));
    _sPrimitiveTypes.put("short[]",   _getArrayType(Short.TYPE));
    _sPrimitiveTypes.put("float[]",   _getArrayType(Float.TYPE));
    _sPrimitiveTypes.put("double[]",  _getArrayType(Double.TYPE));
    _sPrimitiveTypes.put("int",     Integer.TYPE);
    _sPrimitiveTypes.put("boolean", Boolean.TYPE);
    _sPrimitiveTypes.put("char",    Character.TYPE);
    _sPrimitiveTypes.put("long",    Long.TYPE);
    _sPrimitiveTypes.put("short",   Short.TYPE);
    _sPrimitiveTypes.put("float",   Float.TYPE);
    _sPrimitiveTypes.put("double",  Double.TYPE);
  }
}
