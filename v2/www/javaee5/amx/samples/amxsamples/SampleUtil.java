/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package amxsamples;

import java.util.Map;

import com.sun.appserv.management.util.stringifier.SmartStringifier;


/**
	Utility methods for the samples.
 */
public final class SampleUtil
{
	private	SampleUtil()	{}
	
		public static void
	println( final Object... items )
	{
	    String s    = null;
	    
	    if ( items != null && items.length == 1 &&
	        items[0] instanceof String )
	    {
	        s   = (String)items[ 0 ];
	    }
	    else
	    {
		    s   = arrayToString( items, "", "" );
		}
		System.out.println( s );
	}
	

		public static <T> String
	arrayToString(
		final T[] a,
		final String prefix,
		final String suffix )
	{
		final StringBuilder	buf	= new StringBuilder();
		
		for( int i = 0; i < a.length; ++i )
		{
			if ( prefix != null )
			{
				buf.append( prefix );
			}
			
			buf.append( SmartStringifier.toString( a[ i ] ) );
			
			if ( suffix != null && i < a.length )
			{
				buf.append( suffix );
			}
		}
		
		return( buf.toString() );
	}


		public static <K,V> String
	mapToString(
		final Map<K,V> m,
		final String prefix,
		final String suffix )
	{
		final StringBuilder	buf	= new StringBuilder();
		
		for( final K key : m.keySet() )
		{
		    final V value   = m.get( key );
		    
			if ( prefix != null )
			{
				buf.append( prefix );
			}
			
			buf.append( SmartStringifier.toString( key ) + "=" +
			    SmartStringifier.toString( value ) );
			
			buf.append( suffix );
		}
		
		return( buf.toString() );
	}
	
		
}









