package data;

import org.jblas.DoubleMatrix;

import java.util.HashMap;

/**
 * Created by b.rychalska on 22.10.15.
 */
public class Vocabulary {

    HashMap<String, Integer> wordMap;
    DoubleMatrix embeddings;

    public Vocabulary() {
        this.wordMap = new HashMap<String, Integer>();
    }


    public void initVectorMatrix(DoubleMatrix unknown, int vectorLength) {
        embeddings = new DoubleMatrix(vectorLength, wordMap.size());

        for(int i = 0; i < embeddings.columns; i++) {
            embeddings.putColumn(i, unknown);
        }
    }
}
