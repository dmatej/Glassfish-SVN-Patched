import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;

/**
 * Splits the document into a localized version.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class DocGen {

	public static void main(String[] args) throws Exception {
        if(args.length<2) {
            System.out.println("Usage: DocGen <language> <source files>");
            return;
        }
        
        String lang = args[0];
        
        for( int i=1; i<args.length; i++ ) {
            File src = new File(args[i]);
            File dst = new File(
                new File(src.getParentFile(),lang),
                src.getName() );
            System.out.println("Writing "+dst.getPath());
            convert( src, dst, lang );
        }
	}
    
    private static void convert( File src, File dst, String lang ) throws Exception {
        BufferedReader in = new BufferedReader(new InputStreamReader(
            new FileInputStream(src)));
        Writer out = new OutputStreamWriter(new FileOutputStream(dst));
        
        StringBuffer contents = new StringBuffer();
        
        String line;
        while((line=in.readLine())!=null) {
            contents.append(line);
            contents.append('\n');
        }
        
        String body = contents.toString();
        int idx;
        while((idx=body.indexOf("<?"))!=-1) {
            out.write(body,0,idx);
            body = body.substring(idx+2);
            boolean include = body.startsWith(lang);
            body = body.substring(4);   // skip the start tag
            
            idx = body.indexOf("<?/");
            if(idx==-1) {
                System.out.println("Unable to find the close tag from "+body);
                System.exit(-1);
            }
            
            if(include)
                out.write(body,0,idx);
            
            body = body.substring(idx+7);
        }
        
        out.write(body);
        
        in.close();
        out.close();
    }
}

