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
package org.apache.myfaces.trinidadinternal.image.encode;

/**
 * The node elements that make up an OctreeQuantizer.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/OctreeNode.java#0 $) $Date: 15-nov-2005.17:46:01 $
 * @since 0.1.4
 * @see OctreeQuantizer
 */
class OctreeNode
{

  /**
   * Create a new node, which will be the root of the octree.
   */
  public OctreeNode(OctreeQuantizer tree)
  {
    _tree = tree;
    _leaf = new OctreeNode[8];
    _level = 0;
    _maxLevel = 8;
    _children = 0;
    _colorSet = false;
  }

  /**
   * Put a color into the OctreeQuantizer, reducing as necessary
   * @param rgb The color, in AARRGGBB format.
   */
  public void addColor(int rgb)
  {


    int index = _setColorVals(rgb, true);
    // as long as we haven't reached a leaf, we keep on descending
    if(_getLevel() < _maxLevel && _getLevel() < _tree._getMaxDepth())
    {
      if (_leaf[index]==null)
      {
        OctreeNode newNode = new OctreeNode(this);
        _leaf[index] = newNode; // the new child
        if (newNode._getLevel() == _tree._getMaxDepth())
        {
          _tree._incColors();
        }

        // is there a list of nodes at this level?
        if (_tree._getListHead(_getLevel())==null)
        {
          // create the list of nodes at this level
          _tree._setListHead(_getLevel(), newNode);
        }
        else
        {
          // add to the list of nodes at this level
          _tree._setListEnd(_getLevel(), newNode);
        }
      }
      _leaf[index].addColor(rgb);
    }
    else
    {
      if (!_colorSet)
        _computeColor();
    }
  }

  /**
   * Map one color onto another from a limited palette.
   * @param rgb The original color
   * @return The palette-limited color
   */
  public int mapColor(int rgb)
  {
    // once the leaf level is hit
    if (_colorSet)
    {
      // keep transparency from old color, only rgb from new color
      return (rgb&_TRANSPARENCY_MASK)|(_color&_RGB_MASK);
    }

    // traversing the tree
    int index = _setColorVals(rgb, false);

    // if the color isn't in the map, send it back
    if (_leaf[index] == null)
      return rgb;

    return _leaf[index].mapColor(rgb);
  }


  int _getChildren()
  {
    return _children;
  }

  int _getLevel()
  {
    return _level;
  }

  OctreeNode _getNext()
  {
    return _next;
  }

  void _setChildren(int i)
  {
    if(i <= 8) // don't want to be a nontree :-)
      _children = i;
  }

  // using maxColor values, picks a viable color
  void _computeColor()
  {
    // the color is simply the sum of the color amounts divided
    // by the number of pixels contributing to these amounts.
    _color = (int)(((_totalRed/_pixels)<<16) +
                   ((_totalGreen/_pixels)<<8) +
                   (_totalBlue/_pixels));
    _colorSet = true;
  }

  void _setMaxLevel(int i)
  {
    // you can only go lower
    if (i < _maxLevel)
      _maxLevel = i;
  }
  void _setNext(OctreeNode n)
  {
    _next = n;
  }


  // used to create all nodes in the tree other than the root
  private OctreeNode(OctreeNode p)
  {
    this(p._getTree());
    _level = p._getLevel()+1;
    p._incChildren();
  }

  private OctreeQuantizer _getTree()
  {
    return _tree;
  }

  private  void _incChildren()
  {
    _children++;
  }

  // incrementor functions for values that only go up.

  private int _setColorVals(int rgb, boolean isNew)
  {
    int red = (rgb >> 16)&_BYTE_MASK;
    int green = (rgb >> 8)&_BYTE_MASK;
    int blue = rgb&_BYTE_MASK;
    int index = 0;

    if(isNew) // increment color values when adding color
    {
      _totalBlue += blue;
      _totalGreen += green;
      _totalRed += red;
      _pixels++;
    }
    // choose which leaf to follow
    red <<= _getLevel();
    green <<= _getLevel();
    blue <<= _getLevel();
    // 0-7 determines all values of 3-digit binary RGB
    if((byte)red < 0)
      index += 4;
    if((byte)green < 0)
      index += 2;
    if((byte)blue < 0)
      index += 1;
    return index;
  }


  // and this with a color to preserve transparency
  private static final int _TRANSPARENCY_MASK = 0xff000000;
  // and this with a color to preserve rgb
  private static final int _RGB_MASK = 0x00ffffff;
  // and this with an int to keep only the lower 8 bits
  private static final int _BYTE_MASK = 0xff;

  private OctreeQuantizer _tree; // the tree from whence this node was spawned
  private int _children;  // how many leaves are filled
  private OctreeNode[] _leaf; // 8 children of node
  private int _pixels; // how many pixels have passed through this node
  private int _level; // where does this node lie in the tree?
  private OctreeNode _next; // next node at this level
  private int _maxLevel; // lowest permissible depth at this node

  // sum of the (r/g/b) values of every node beneath this, incremented
  // for every pixel that passes through.

  private long _totalRed;
  private long _totalGreen;
  private long _totalBlue;

  // color value at this node. If _colorSet is false,
  // this value is meaningless (and a color is to be found further down
  // the tree)
  private int _color;
  private boolean _colorSet; // is the color set?
}
