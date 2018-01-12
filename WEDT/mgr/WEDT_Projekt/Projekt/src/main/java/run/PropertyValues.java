package run;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Created by b.rychalska on 28.02.16.
 */
public class PropertyValues {
    public int iterations = 0;
    public int catSize = 6;
    public int hiddenSize = 50;
    public String dataPath = "";
    public String embeddingsPath = "";
    public String rnnArch;
    public String chainArch;

    InputStream inputStream;

    public void getPropValues() throws IOException {

        try {
            Properties prop = new Properties();
            String propFileName = "config";

            inputStream = getClass().getClassLoader().getResourceAsStream(propFileName);
            if (inputStream != null) {
                prop.load(inputStream);
            } else {
                throw new FileNotFoundException("property file '" + propFileName + "' not found in the classpath");
            }

            iterations = Integer.parseInt(prop.getProperty("iterations"));
            catSize = Integer.parseInt(prop.getProperty("catSize"));
            hiddenSize = Integer.parseInt(prop.getProperty("hiddenSize"));

            dataPath = prop.getProperty("dataPath");
            embeddingsPath = prop.getProperty("embeddingsPath");

            rnnArch = prop.getProperty("rnnArchitecture");
            chainArch = prop.getProperty("chainArchitecture");

        } catch (Exception e) {
            System.out.println("Exception: " + e);
        } finally {
            inputStream.close();
        }
    }
}
