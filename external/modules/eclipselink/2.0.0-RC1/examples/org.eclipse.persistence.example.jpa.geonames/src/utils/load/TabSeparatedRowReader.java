/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - initial GeoNames EclipseLink JPA example
 ******************************************************************************/
package utils.load;

import java.io.*;
import java.util.*;

/**
 * Utility class intended to simplify the loading of the various GeoNames data
 * dump files.
 * 
 * The files used are all available from
 * {link}http://download.geonames.org/export/dump/{link}
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class TabSeparatedRowReader {
	private static final String WEB_LOCATION = "http://download.geonames.org/export/dump/";
	private static final String FILE_LOCATION = "./data/";

	private File dataFile;
	private BufferedReader reader;

	public TabSeparatedRowReader(String fileName) {
		this.dataFile = new File(FILE_LOCATION + fileName);
		verifyDataFile();
	}

	public File getDataFile() {
		return this.dataFile;
	}

	/**
	 * Ensure the file exists and if it does not then pull it down from the
	 * GeoNames web site
	 */
	private void verifyDataFile() {
		if (getDataFile().exists()) {
			return;
		}
		throw new RuntimeException("Automatic download not yet supported");
	}

	public BufferedReader getReader() {
		if (this.reader == null) {
			try {
				this.reader = new BufferedReader(new FileReader(getDataFile()));
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return this.reader;
	}

	public boolean ready() {
		try {
			return getReader().ready();
		} catch (IOException e) {
			throw new RuntimeException("TabSeparatedRowReader.ready fialed: "
					+ e.getMessage(), e);
		}
	}

	public void close() {
		try {
			getReader().close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void skipLines(int numLines) {
		for (int index = 0; index < numLines; index++) {
			try {
				getReader().readLine();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	/**
	 * Return a String[] of the 12 elements. Since some of the country lines are
	 * not for real countries they lack all of the columns so the tab parsing
	 * must be more careful to allow null values to be detected.
	 */
	public List<String> readLine() {
		String line = null;
		try {
			line = reader.readLine();
		} catch (IOException ioe) {
			throw new RuntimeException(
					"TabSeparatedRowReader.readLine failed: "
							+ ioe.getMessage(), ioe);
		}
		StringTokenizer tokenizer = new StringTokenizer(line, "\t", true);
		List<String> tokens = new ArrayList<String>();

		String lastToken = null;
		while (tokenizer.hasMoreTokens()) {
			String token = tokenizer.nextToken();

			if (token.equals("\t")) {
				if (lastToken != null && lastToken.equals("\t")) {
					tokens.add(null);
				}
			} else {
				tokens.add(token);
			}
			lastToken = token;

		}
		return tokens;
	}

}
