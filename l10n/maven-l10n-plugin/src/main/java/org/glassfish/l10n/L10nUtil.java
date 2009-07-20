package org.glassfish.l10n;

import java.io.File;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.LineNumberReader;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

/**
 * Utility class for l10n build tasks.
 *
 * @author Shinya Ogino
 */
public class L10nUtil {

    private L10nUtil() {}

    /**
     * Compare en and l10n resource bundle files and return 
     * word count of unlocalized messages.
     */
    public static int wordCount(File en, File l10n) throws IOException {
        // initialize word counter.
        int wcounter = 0;
        try {
            // Load en messages into List enMessages.
            List<String> enMessages = getMessagesInBundle(en);
            // load l10n messages into List l10nMessages.
            // if l10n was null, create blank List.
            List<String> l10nMessages = new ArrayList<String>();
            if (l10n != null) {
                l10nMessages = getMessagesInBundle(l10n);
            }
            // For each message in enMessages,
            for (String message : enMessages) {
                // Check if the same message exists in l10nMessages.
                // if not,
                if (l10nMessages.indexOf(message) < 0) {
                    // add wordcount to word counter
                    // TODO: make this more accurate.
                    wcounter += message.split("\\s").length;
                }
            }
        } catch (IOException e) {
            throw e;
        }
        return wcounter;
    }

    /**
     * Return word count of a file.
     */
    public static int wordCount(File en) throws IOException {
        return wordCount(en, null);
    }

    /**
     * Load a resource bundle and return a List of messsages.
     * Expects .properties file with additional #EN added english lines.
     * #EN key=value
     * key=value
     */
    public static List<String> getMessagesInBundle(File f) throws IOException {
        // initialize List.
        List<String> l = new ArrayList<String>();
        LineNumberReader reader = null;
        try {
            FileReader r = new FileReader(f);
            reader = new LineNumberReader(r);
            String line;
            StringBuffer buffer = new StringBuffer();
            // For each lines,
            while ((line = reader.readLine()) != null) {
                // if the line starts with #EN,
                if (line.startsWith("#EN")) {
                    // add the line to buffer excluding #EN.
                    buffer.append(line.substring(4));
                    // if the line ends with backslash,
                    if (line.endsWith("\\")) {
                        // remove last backslash from buffer
                        buffer.delete(buffer.length()-1, buffer.length());
                    // else,
                    } else {
                        // add buffer contents to List.
                        l.add(buffer.toString());
                        // clear buffer.
                        buffer.setLength(0);
                    }
                // else continue.
                }
            }
        } catch (IOException e) {
            throw e;
        } finally {
            try {
                if (reader != null) reader.close();
            } catch (IOException e) { /* ignore */ }
        }
        return l;
    }

    /**
     * Find all the files matching pattern starting from directory root.
     */
    public static List<File> findFiles(File root, String pattern, String antipattern) throws IOException {
        List<File> result = new ArrayList<File>();
        try {
            File[] children = root.listFiles();
            for (File child : children) {
                if (child.getAbsolutePath().matches(pattern)
                    && ! child.getAbsolutePath().matches(antipattern)) {
                    result.add(child);
                } else if (child.isDirectory()) {
                    result.addAll(findFiles(child, pattern, antipattern));
                }
            }
        } catch (IOException e) { throw e; }
        return result;
    }

    /**
     * Find all the files matching pattern starting from directory root.
     */
    public static List<File> findFiles(File root, String pattern) throws IOException {
        return findFiles(root, pattern, "");
    }

    /**
     * Return localization status for .properties files.
     */
    public static String getStatusReport(File enDir, File l10nDir) throws IOException{
       StringBuffer report = new StringBuffer("FileName\tTotalWords\tStatus\n");
        try  {
            // get List of *_en.properties files.
            List<File> enFiles = findFiles(enDir, ".*\\.properties", ".*_\\w\\w\\.properties");
            // For each _en file,
            for (File enfile : enFiles) {
                // check if l10n exists for the file.
                String fname = enfile.getAbsolutePath().substring(enDir.getAbsolutePath().length());
                String pattern = ".*" + fname.replaceFirst("\\.properties", "") + "(_\\w\\w)+\\.properties";
                List<File> l10nFiles = findFiles(l10nDir, pattern);
                // get word count.
                int wc = wordCount(enfile);
                // if there were no translatable messages,
                if (wc < 1) {
                    report.append(fname + "\t" + wc + "\tNA\n");
                // else if l10n doesn't exist,
                } else if (l10nFiles.size() < 1) {
                    report.append(fname + "\t" + wc + "\tUntranslated\n");
                // else if exists,
                } else {
                    report.append(fname + "\t" + wc + "\t");
                    StringBuffer statusStr = new StringBuffer();
                    // for each l10n file,
                    for (File l10nfile : l10nFiles) {
                        // l10n status in percentage.
                        int status = 100 * (wc - wordCount(enfile, l10nfile)) / wc;
                        // log the status into report.
                        String name = l10nfile.getName();
                        String locale = name.substring(name.indexOf("_")+1, name.indexOf(".properties"));
                        statusStr.append(locale + ":" + status + "%,");
                    }
                    statusStr.delete(statusStr.length()-1, statusStr.length());
                    report.append(statusStr + "\n");
                }
            }
        } catch (IOException e) {
            throw e;
        }
        return report.toString();
    }
}


