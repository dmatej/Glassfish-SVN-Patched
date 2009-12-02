package testing.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * This Helper is used to simulate serialization of an obejct to create a 
 * detached instance (clone).
 */
public class SerializationHelper {

    public static Object serialize(Serializable object) {
        Object detachedObject = null;

        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream(1000);
            ObjectOutputStream oos = new ObjectOutputStream(bos);
            oos.writeObject(object);
            oos.close();

            ByteArrayInputStream bis =
                new ByteArrayInputStream(bos.toByteArray());
            ObjectInputStream ois = new ObjectInputStream(bis);
            detachedObject = ois.readObject();
            ois.close();
        } catch (IOException ioe) {
            ioe.printStackTrace();
            throw new RuntimeException("SerializationHelper failure: " +
                                       ioe.getMessage());
        } catch (ClassNotFoundException cnfe) {
            cnfe.printStackTrace();
            throw new RuntimeException("SerializationHelper failure: " +
                                       cnfe.getMessage());
        }

        return detachedObject;
    }
}
