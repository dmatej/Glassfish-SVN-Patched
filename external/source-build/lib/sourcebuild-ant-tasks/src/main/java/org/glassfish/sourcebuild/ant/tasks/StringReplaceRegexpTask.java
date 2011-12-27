package org.glassfish.sourcebuild.ant.tasks;

import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.util.regexp.RegexpMatcher;
import org.apache.tools.ant.util.regexp.RegexpMatcherFactory;

/*
 * Match and apply patterns over a string
 * and return the result as a property
 */
public class StringReplaceRegexpTask extends Task {
    private String str;

    private String match;
    private String replace;
    private String result;

    @Override
    public void execute() throws BuildException {
        RegexpMatcherFactory matcherFactory = new RegexpMatcherFactory();
        RegexpMatcher matcher = matcherFactory.newRegexpMatcher();
        matcher.setPattern(match);

        Vector groups = matcher.getGroups(str);

        if(groups != null){
            String[] replaceTokens = replace.split("\\\\");
            StringBuilder sb = new StringBuilder(replaceTokens[0]);
            for (int i = 1; i < replaceTokens.length; i++) {
                char c = replaceTokens[i].charAt(0);
                if (Character.isDigit(c)
                        && ((int) c) -48 > 0
                        && ((int) c) -48 < groups.size()) {
                    sb.append(groups.elementAt(((int) c)-48));
                    sb.append(replaceTokens[i].substring(1));
                } else {
                    throw new BuildException("invalid replace attribute");
                }
            }
            getProject().setNewProperty(result, sb.toString());
        }
    }

    public void setMatch(String msg) {
        this.match = msg;
    }

    public void setReplace(String replace) {
        this.replace = replace;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public void setStr(String str) {
        this.str = str;
    }
}
