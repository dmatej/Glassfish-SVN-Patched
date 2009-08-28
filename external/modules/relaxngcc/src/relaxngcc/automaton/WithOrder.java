package relaxngcc.automaton;
import java.util.Comparator;

/**
 * Implemented by those alphabets that have orders.
 */
public interface WithOrder {
    public int getOrder();

    /**
     * Comparator that can be used to sort ordered alphabets into
     * descending orders (larger numbers first.)
     */
    public static Comparator orderComparator = new Comparator() {
        public int compare( Object o1, Object o2 ) {
            return ((WithOrder)o2).getOrder()-((WithOrder)o1).getOrder();
        }
    };
}
    
