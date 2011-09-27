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

import java.awt.Image;
import java.awt.image.ImageObserver;
import java.awt.image.PixelGrabber;
import org.apache.myfaces.trinidadinternal.image.painter.ImageLoader;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Reduces an image's color palette by means of an octree quantization method.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/OctreeQuantizer.java#0 $) $Date: 10-nov-2005.19:05:19 $
 * @since 0.1.4
 * @see OctreeNode
 */

class OctreeQuantizer
{

  // The octree quantization method reduces a set of colors in an
  // image to a desired color palette size while maintaining as much
  // image quality as possible. The principle structure employed here
  // is an octree, or tree with a branching factor of 8. The tree is
  // also 8 nodes deep, as each color may be represented by an 8 bit
  // number.
  //
  // The image is scanned and pixel colors are read into the tree and
  // stored one at a time. Branch selection is as follows: at depth n,
  // the nth most significant r, g, and b values form rgb, a binary
  // number from 0 to 7. The red, green, and blue values of the entire
  // color are added to red, green, and blue fields at every node
  // along the way, so every node has the sum of color value for all
  // of its children. The number of unique colors in the tree is
  // noted, though red, green, and blue value totals per node as
  // described above are done for *each* pixel.
  // 
  // Once the number of unique colors exceeds the desired threshhold,
  // from 2 to 8 colors are combined into a single color. A search is
  // done from the bottom of the tree, since colors with a common
  // node share all of that nodes's parental hierarchy. Nodes in
  // deeper levels are combined first, however which node is combined
  // first within a row results in slightly different outcomes, which
  // are not discussed here. In this implementation, the first viable
  // node is reduced. 
  //
  // Once a node has been selected, its children are removed and it
  // takes a color equal to a weighted average of its children. Since
  // color sums are kept in each node, all that needs be done is
  // divide the total red, green, and blue values by the number of
  // pixels passing through the node.
  //
  // Note that further colors that would have been added to pruned
  // nodes are not added further than the prune. The image color is
  // not changed either, though red, green, and blue totals are still
  // updated as before.
  //
  // Once the image has been scanned in, it is trivial to map a color
  // to its equivalent from the new color table. Simply traverse the
  // tree as before, stopping when a node with no children is reached
  // and return that node's color value.
  //
  // This algorithm is made more efficient by the use of color lists
  // to more quickly find nodes to reduce. As nodes are created they
  // are added to the appropriate list for their level. When a reduce
  // is performed, the lists are scanned, one at a time, for a viable
  // node. If the end of a list is reached, the next highest list is
  // scanned, and no further node will ever be created in the lower
  // list. This thus makes the adding nodes operation progressively
  // faster.

  /**
   * Create a new octree and insert image data. This constructor is mostly
   * used by OctreeFilter.
   * @param im The image that provides pixel data to construct the tree
   */
  public OctreeQuantizer(Image im)
  {
    this();

    // first retrieve the pixels
    ImageLoader il = new ImageLoader(im);
    il.start();
    if(!il.waitFor()){
      throw new IllegalArgumentException(_LOG.getMessage(
        "PROBLEM_LOADING"));
    }
    int width = im.getWidth(il);
    int height = im.getHeight(il);
    int[] pixels = new int[width*height]; // going to hold all
    // the image's pixels
    
    PixelGrabber grabber = new PixelGrabber(im.getSource(), 0, 0,
                                            width, height,
                                            pixels, 0, width);
    try // get the pixels
    { 
      grabber.grabPixels();
    } 
    catch (InterruptedException e) 
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "GRABBING_PIXELS"));
    } 
    if ((grabber.getStatus() & ImageObserver.ABORT) != 0) 
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ERROR_FETCHING_IMAGE", new Object[]{pixels.length,width,height}));
    }

    // add all pixels to the tree.
    for (int i = 0; i < pixels.length; i++)
      addColor(pixels[i]);

  }


  /**
   * Map an array of color values into an octree to reduce the colors.
   * Modify the array to reflect the color changes. This method is used
   * to do all the creation and mapping at once.
   * @param pixels The image data.
   */
  public static void mapColors(int[] pixels)
  {
    OctreeQuantizer o = new OctreeQuantizer();

    // add all the colors to the tree
    for (int i = 0; i < pixels.length; i++)
      o.addColor(pixels[i]);

    // modify the array
    for (int i = 0; i < pixels.length; i++)
      pixels[i] = o.mapColor(pixels[i]);
    
  }

  /**
   * Add a color to the tree. This is a wrapper for a call to the root 
   * OctreeNode.
   * @param rgb the color to be added
   */
  public void addColor(int rgb)
  {
    _root.addColor(rgb);
  }

  /**
   * Map a color to the tree. This is a wrapper for a call to the root
   * OctreeNode.
   * @param rgb the color to be mapped
   * @return the mapped color
   */
  public int mapColor(int rgb)
  {
    return _root.mapColor(rgb);
  }
  
  OctreeNode _getListHead(int i)
  {
    return _listHead[i];
  }
  int _getMaxDepth()
  {
    return _maxDepth;
  }

  // increment the number of colors, and if this goes over the mark, reduce
  void _incColors()
  {
    if(++_colors > _MAX_COLORS)
      _reduce();
  }

  // define the beginning of a list
  void _setListHead(int i, OctreeNode n)
  {
    _listHead[i] = n;
    _listEnd[i] = n;
  }

  // add to a list
  void _setListEnd(int i, OctreeNode n)
  {
    _listEnd[i]._setNext(n); // continue the chain
    _listEnd[i] = n;
  }

  // create a root
  private OctreeQuantizer()
  {
    _root = new OctreeNode(this);
    _listHead = new OctreeNode[8];
    _listEnd = new OctreeNode[8];
    _maxDepth = 8;
  }

  // pick the first viable node (lowest level that will remove at least
  // one color
  private  OctreeNode _pickNode()
  {
    // apparently, once you've risen a level, you don't go back. I 
    // question apple's claim, but it works okay nonetheless.
    // why _maxDepth-2? -1 since _maxDepth starts at 8 and the array is 0-based.
    // -2 because we're only concerned with the seventh level and above - leaves
    // can't be pruned.
    for (int i = _maxDepth-2; i >=0; i--)
    {
      // traverse list i for a good node
      OctreeNode n = _getListHead(i);
      OctreeNode p = null; // previously selected node
      
      // picking specific node
      OctreeNode saveNode = null;
      OctreeNode savePrev = null;
      // traverse the entire level looking for the best node, which is placed in saveNode
      while (n != null)
      {
        
        // usable nodes have siblings
        if (n._getChildren() > 1)
        {
          // picking specific node
          if ((saveNode == null) || 
              (n._getChildren() < saveNode._getChildren()))
          {
            saveNode = n;
            savePrev = p;
          }
        }
        // still looking
        p = n;
        n = n._getNext();
      }
      if (saveNode != null)
      {
      
        // if we picked the first node, then we have to change the value
        // in the list of heads.
        if (savePrev == null)
        {
          _listHead[i] = saveNode._getNext();
        }   
        else
        {
          OctreeNode nextNode = saveNode._getNext();
          // if we picked the last node, then we have to change the value
          // in the list of ends
          if (nextNode == null)
          {
            _listEnd[i] = savePrev;
          }
          savePrev._setNext(nextNode);   // otherwise preserve the chain
          // if nextNode is null, we still want to erase n from p.next
          
        }
        return saveNode;
      }
      // we've exhausted this level
      _maxDepth--;
    }
    return null;
  }

 // get rid of some nodes
  private void _reduce()
  {
    // which node to reduce?
    OctreeNode n = _pickNode();

    // now no colors will be created under this node
    n._setMaxLevel(n._getLevel());
    
    // determine color based on weighted average
    n._computeColor();
    
    _colors -= (n._getChildren()-1); // children are pruned
    
    // now children will never be used by _pickNode
    n._setChildren(0);
  }

  // the size of the desired color table
  private static final int _MAX_COLORS = 255;  

  private int _colors; // how many colors currently in the tree
  
  // heads of lists of available nodes. Where all _pickNode calls look from
  private OctreeNode[] _listHead; 

  // tails of lists of available nodes. Where new nodes are added
  private OctreeNode[] _listEnd; 

  private int _maxDepth; // max. relevant depth of the tree


  private OctreeNode _root;   // root node of the tree
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    OctreeQuantizer.class);
}




