package org.apache.myfaces.trinidad.util;

/**
 * Interface for exceptions that tells whether the exception should be reported.
 * The implementation of this interface can be thrown to notify an exception without reporting it.
 */
public interface Reportable
{

  /**
   * Return false if this exception should not be reported, otherwise true.
   */
  public boolean shouldReportMessage();

}