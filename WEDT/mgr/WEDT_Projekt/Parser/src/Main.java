import java.io.*;
import java.util.HashMap;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {

    static final String wa = "data/STSint.input.images.wa";

    static HashMap<String, Integer> labelTranslate = new HashMap<>();

    public static void main(String[] args) {

        labelTranslate.put("EQUI", 0);
        labelTranslate.put("OPPO", 1);
        labelTranslate.put("SPE1", 2);
        labelTranslate.put("SPE2", 3);
        labelTranslate.put("SIMI", 4);
        labelTranslate.put("REL", 5);
        labelTranslate.put("NOALI", 6);
        labelTranslate.put("NIL", 6);

        Pattern pat = Pattern.compile("^.+ // (.+) // (.+) // (.+) <==> (.+) $");

        try {
            BufferedReader brwa = new BufferedReader(new FileReader(wa));

            FileWriter train_ph = new FileWriter("images_phrases_train.txt");
            FileWriter tral1_ph = new FileWriter("images_phrases_train_labels.txt");
            FileWriter tral2_ph = new FileWriter("images_phrases_train_labels2.txt");

            FileWriter test_ph = new FileWriter("images_phrases_test.txt");
            FileWriter tstl1_ph = new FileWriter("images_phrases_test_labels.txt");
            FileWriter tstl2_ph = new FileWriter("images_phrases_test_labels2.txt");

            String waLine;

            Random rand = new Random();

            while ((waLine = brwa.readLine()) != null) {
                Matcher m = pat.matcher(waLine);

                if(m.matches()) {
                    //TODO: what to do with noali?
                    //if(!"NOALI".equals(m.group(1))) {
                        if(rand.nextBoolean()) {
                            train_ph.write(m.group(3) + "\t" + m.group(4) + "\n");
                            tral1_ph.write(labelTranslate.get(m.group(1)) + "\n");
                            if(!m.group(2).equals("NIL"))
                                tral2_ph.write(m.group(2) + "\n");
                            else
                                tral2_ph.write(labelTranslate.get(m.group(2)) + "\n");
                        } else {
                            test_ph.write(m.group(3) + "\t" + m.group(4) + "\n");
                            tstl1_ph.write(labelTranslate.get(m.group(1)) + "\n");
                            if(!m.group(2).equals("NIL"))
                                tstl2_ph.write(m.group(2) + "\n");
                            else
                                tstl2_ph.write(labelTranslate.get(m.group(2)) + "\n");
                        }
                    //}
                }
            }
            train_ph.close();
            tral1_ph.close();
            tral2_ph.close();

            test_ph.close();
            tstl1_ph.close();
            tstl2_ph.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
