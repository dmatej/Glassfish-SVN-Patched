package org.codehaus.stax2;

import javax.xml.stream.Location;

/**
 * Extension of {@link Location} that adds accessor to retrieve nested
 * location information.
 */
public interface XMLStreamLocation2
    extends Location
{
    /**
     * Method that can be used to traverse nested locations, like ones
     * created when expanding entities (especially external entities).
     * If so, single location object only contains information about
     * specific offsets and ids, and a link to its context. Outermost
     * location will return null to indicate there is no more information
     * to retrieve.
     *
     * @return Location in the context (parent input source), if any;
     *    null for locations in the outermost known context
     */
    public XMLStreamLocation2 getContext();
}
