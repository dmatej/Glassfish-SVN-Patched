// $Id: Selection.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import javax.persistence.TupleElement;
import java.util.List;

/**
 * The Selection interface defines an item that to be
 * returned in the query result.
 * @param <X> the type of the selection item
 */
public interface Selection<X> extends TupleElement<X> {

    /**
     * Return a selection item with the assigned alias.
     * @param name  alias
     * @return selection item
     */
    Selection<X> alias(String name);

    /**
     * Whether the selection item is a compound selection
     * @return boolean
     */
    boolean isCompoundSelection();

    /**
     * Return selection items composing a compound selection
     * @return list of selection items
     * @throws IllegalStateException if selection is not a compound
     *           selection
     */
    List<Selection<?>> getCompoundSelectionItems();

}

