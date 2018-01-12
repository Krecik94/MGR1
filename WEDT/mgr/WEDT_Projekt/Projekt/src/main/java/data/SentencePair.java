package data;

import org.jblas.DoubleMatrix;

import java.util.HashMap;

/**
 * Created by b.rychalska on 22.10.15.
 */
public class SentencePair implements Comparable<SentencePair> {
    Sentence s1;
    Sentence s2;

    float labels[];

    double error;

    public SentencePair(String s1, String s2, float singleLabel, float secondLabel) {
        this.labels = new float[2];
        this.s1 = new Sentence(s1);
        this.s2 = new Sentence(s2);
        this.labels[0] = singleLabel;
        this.labels[1] = secondLabel;
    }

    public HashMap<String, DoubleMatrix> getEmbeddings(Vocabulary vocab) {
        HashMap<String, DoubleMatrix> embeddings = new HashMap<String, DoubleMatrix>();

        for(Sentence sent : this.getSentences()) {
            String[] words = sent.getS().split(" ");
            for(String w : words) {
                embeddings.put(w, vocab.embeddings.getColumn(vocab.wordMap.get(w)));
            }
        }

        return embeddings;
    }

    public Sentence[] getSentences() {
        return new Sentence[] {s1, s2};
    }

    public float getLabel(int idx) {
        return labels[idx];
    }

    public int getPairLength() {
        return s1.getLength() + s2.getLength();
    }

    public double getError() {
        return error;
    }

    public void setError(double error) {
        this.error = error;
    }


    public int compareTo(SentencePair sp) {
        if(error > sp.error) return 1;
        if(error == sp.error) return 0;
        else return -1;
    }
}
