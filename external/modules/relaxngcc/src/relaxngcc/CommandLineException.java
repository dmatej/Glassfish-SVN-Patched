package relaxngcc;

/**
 * Signals an error in command line arguments.
 */
public class CommandLineException extends Exception {
    public CommandLineException( String msg ) {
        super(msg);
    }
}
