package data;
import java.util.ArrayList;
import java.util.Collections;

/**
 * Created by b.rychalska on 22.10.15.
 */
public class Dataset {
    ArrayList<SentencePair> trainset;
    ArrayList<SentencePair> testset;

    public Dataset() {
        trainset = new ArrayList<SentencePair>();
        testset = new ArrayList<SentencePair>();
    }


    public Dataset(ArrayList<SentencePair> trainset, ArrayList<SentencePair> testset) {
        this.trainset = trainset;
        this.testset = testset;
    }


    public void setSets(ArrayList<SentencePair> trainset, ArrayList<SentencePair> testset) {
        this.trainset = trainset;
        this.testset = testset;
    }

    public ArrayList<SentencePair> getTrainset() {
        return trainset;
    }

    public ArrayList<SentencePair> getTestset() {
        return testset;
    }

    public ArrayList<SentencePair> sortTestsetByError() {
        ArrayList<SentencePair> sorted = (ArrayList<SentencePair>)testset.clone();
        Collections.sort(sorted);
        return sorted;
    }
}
