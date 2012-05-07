Here's the general approach:

Run mvn findbugs:findbugs to generate the findbugsXml.xml files.

Find each of those files and run each one through my "findbugstotext.jar"
program to convert the error messages to the format of a compiler error
message:  "file:line:error-message".

Adjust the filenames to account for the module in which the file exists.

Send the list of errors through my "svnowner" script, which determines
who last changed each line with an error and outputs a line of the form:
owner:file:line:error-message

Send that list of owners and errors through my "fbnag" script, which
collects together all the errors for a particular owner and composes an
email message to be sent to the owner.

All the above is encapsulated in a script called "findbugsmail".


The fbnag script depends on several files to configure its behavior:

nag.msg		- a file with the general message to be inserted at the
		  top of each email message telling people why they're
		  getting the mesage, what they're expected to do about
		  it, etc.

users.bad	- a list of user names (java.net user names in this case),
		  one per line, who should never get these messages.
		  usually these are users who no longer exist and for whom
		  the email is just going to bounce.

users.alias	- a mapping file that allows you to redirect mail for one
		  user to another user.  the format is "oldname newname"
		  on each line.  this is good for cases where a user is no
		  longer part of the project and you want to redirect the
		  messages to his manager or replacement.

All of the above files must exist, even if they're empty.

See the fbnag script for how to set it up for testing, so you don't generate
lots of email messages until you're sure it's working correctly.

One of the potential issues with this program is that it depends on Unix
/bin/mail to send email.  That works fine on Solaris systems, but it might
not work without further configuration on other Unix systems.
