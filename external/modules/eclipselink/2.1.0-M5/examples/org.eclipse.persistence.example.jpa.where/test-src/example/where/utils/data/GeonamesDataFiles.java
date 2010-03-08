package example.where.utils.data;

import java.io.*;
import java.net.URL;

public class GeonamesDataFiles {

	private static final String WEB_LOCATION = "http://download.geonames.org/export/dump/";
	private static final String FILE_LOCATION = "./data/";

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		new File(FILE_LOCATION).mkdirs();

		download(FeatureLoader.FILE_NAME);
		download(TimeZoneLoader.FILE_NAME);
		download(LanguageLoader.FILE_NAME);
		download(CountryLoader.FILE_NAME);
		download(AdminCodesLoader.FILE_NAME);

		for (int index = 0; index < args.length; index++) {
			download(args[index] + ".zip");
		}
	}

	protected static File getLocalFile(String fileName, boolean mustExist) {
		File file = new File(FILE_LOCATION + fileName);

		if (mustExist && !file.exists()) {
			throw new IllegalArgumentException("Data file does not exist: " + file);
		}
		return file;
	}

	protected static InputStream openStream(String fileName) {
		try {
			URL url = new URL(WEB_LOCATION + fileName);
			return url.openStream();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			throw new RuntimeException("Could not open stream:" + WEB_LOCATION + fileName);
		}
	}

	private static void download(String fileName) {
		System.err.print("Downloading: " + fileName + " ... ");

		InputStream in = openStream(fileName);
		FileOutputStream fileOut = null;
		try {
			fileOut = new FileOutputStream(getLocalFile(fileName, false));
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		try {
			while (in.available() > 0) {
				fileOut.write(in.read());
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} finally {
			try {
				in.close();
				fileOut.close();

				System.out.println("DONE");
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

}
