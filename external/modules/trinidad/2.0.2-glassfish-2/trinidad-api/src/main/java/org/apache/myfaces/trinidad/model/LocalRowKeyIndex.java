package org.apache.myfaces.trinidad.model;

/**
 * Defines a set of "local" APIs for a CollectionModel.
 * The "local" APIs allow a client to query the model and determine if a 
 * set of rows are locally available. "Locally available" can mean the 
 * model has the given set of rows in a local cache and can honor a fetch request
 * efficiently (for example, without performing a SQL query).
 */
public interface LocalRowKeyIndex
{

  /**
   * Given a row index, check if a row is locally available
   * @param rowIndex index of row to check 
   * @return <code>true</code> if row is locally available <code>flase</code> otherwise
   */
  public boolean isRowLocallyAvailable(int rowIndex);

  /**
   * Given a row key, check if a row is locally available
   * @param rowKey row key for the row to check 
   * @return <code>true</code> if row is locally available <code>flase</code> otherwise
   */
  public boolean isRowLocallyAvailable(Object rowKey);

  /**
   * Check if a range of rows is locally available starting from a row index
   * @param startIndex staring index for the range  
   * @param rowCount number of rows in the range
   * @return <code>true</code> if range of rows is locally available <code>flase</code> otherwise
   */
  public boolean areRowsLocallyAvailable(int startIndex, int rowCount);

  /**
   * Check if a range of rows is locally available starting from a row key
   * @param startRowKey staring row key for the range  
   * @param rowCount number of rows in the range
   * @return <code>true</code> if range of rows is locally available <code>flase</code> otherwise
   */
  public boolean areRowsLocallyAvailable(Object startRowKey, int rowCount);
  
  /**
   * Check if a range of rows is locally available starting from the current row
   * @param rowCount number of rows in the range
   * @return <code>true</code> if range of rows is locally available <code>flase</code> otherwise
   */
  public boolean areRowsLocallyAvailable(int rowCount);
  
  /**
   * Convenient API to return a row count estimate.  This method can be optimized 
   * to avoid a data fetch which may be required to return an exact row count.
   * <p>
   * This method can return -1 or a row count estimate if determining
   * exact row count requires a data fetch.  When dealing with estimated row counts,
   * the caller needs to gracefully handle the case where <code>isRowAvailable</code>
   * returns <code>false</code> for a row index or a row key.
   * @return estimated row count
   */
  public int getEstimatedRowCount();


  /**
   * Helper API to determine if the row count returned from {@link #getEstimatedRowCount} 
   * is EXACT, or an ESTIMATE
   */
  public Confidence getEstimatedRowCountConfidence();

  
  /**
   * Enum used in the {@link #getEstimatedRowCountConfidence} API to determine 
   * if the row count is exact or an estimate
   */
  public enum Confidence
  {
    /**
     * The row count returned by {@link #getEstimatedRowCount} is exact
     */
    EXACT,

    /**
     * The row count returned by {@link #getEstimatedRowCount} is an estimate
     */
    ESTIMATE,
        
    /**
     * The row count returned by {@link #getEstimatedRowCount} is unknown (-1)
     */
    UNKNOWN    
  }
  
  
  //
  // Local Cache management APIs
  //
  
  /**
   * clear all rows from the local cache
   */
  public void clearLocalCache();
  
  /**
   * Clear the requested range of rows from the local cache
   * @param startingIndex starting row index for the range to clear
   * @param rowsToClear number of rows to clear from the cache
   */
  public void clearCachedRows(int startingIndex,  int rowsToClear);
  
  /**
   * Clear the requested range of rows from the local cache
   * @param startingRowKey starting row key for the range to clear
   * @param rowsToClear number of rows to clear from the cache
   */
  public void clearCachedRows(Object startingRowKey, int rowsToClear);
  
  /**
   * Clear a row from the local cache by row index
   * @param index row index for the row to clear from the cache
   */
  public void clearCachedRow(int index);
  
  /**
   * Clear a row from the local cache by row key
   * @param rowKey row key for the row to clear from the cache
   */
  public void clearCachedRow(Object rowKey);
  
  /**
   * Indicates the caching strategy supported by the model
   * @see LocalCachingStrategy
   * @return caching strategy supported by the model
   */
  public LocalCachingStrategy getCachingStrategy();
  
  /**
   * Enum used to indicate the type of caching supported by the model
   * @see #getCachingStrategy()
   */
  public enum LocalCachingStrategy
  {
    /**
     * Caching is not supported
     */
    NONE,
    
    /**
     * Supports caching certain ranges of rows
     */
    PARTIAL,
    
    /**
     * Caches all rows
     */
    ALL
  }
}
