package rnn;

import data.Sentence;
import data.SentencePair;
import data.Vocabulary;
import org.jblas.DoubleMatrix;
import utils.DLTree;
import utils.DoubleMatrixUtils;

import java.util.HashMap;

/**
 * Created by b.rychalska on 30.10.15.
 */
public abstract class Propagator {
    HashMap<String, DoubleMatrix> W_upd;
    HashMap<String, DoubleMatrix> b_upd;
    double cost;
    double tagging;
    double reg = 1e-3;
    public DoubleMatrix predicted;

    public Propagator() {
        cost = 0.0;
        W_upd = new HashMap<String, DoubleMatrix>();
        b_upd = new HashMap<String, DoubleMatrix>();
    }


    protected DoubleMatrix getRoundedTarget(Theta theta, float label) {
        int roundedLabel = (int) Math.round(label);
        DoubleMatrix target = new DoubleMatrix(theta.catSize, 1);
        target.put(roundedLabel, 1);
        return target;
    }

    protected double getRoundedTagging(DoubleMatrix predicted) {
        return predicted.argmax();
    }

    protected DoubleMatrix getWeightedTarget(Theta theta, float label) {
//        System.out.println("label: " + label);
        int upLabel = 0;
        int downLabel = 0;

        if(label%1 == 0) return getRoundedTarget(theta, label); //if label has no decimal part - no need to do weighting
        else upLabel = (int)label +1; //if label has a decimal part - round to +1;
        downLabel = upLabel - 1;

        double weightedUp = (label - downLabel) / (upLabel - downLabel);
        double weightedDown = 1 - weightedUp;

        DoubleMatrix target = new DoubleMatrix(theta.catSize, 1);
        target.put(upLabel, weightedUp);
        target.put(downLabel, weightedDown);

//        System.out.println("weighted target: " + target);
        return target;
    }

    protected double getWeightedTagging(DoubleMatrix predicted) {
        double tagging = 0.0;
        for(int i = 0; i< predicted.length; i++) {
            tagging += predicted.get(i) * i;
        }
//        tagging /= predicted.length;
//        System.out.println("predicted: " + predicted);

//        System.out.println("predicted: " + tagging);
//        return tagging;

        return predicted.argmax();
    }



    public void forwardPropagate(SentencePair pair, Vocabulary vocab, Theta theta, int chainArchitecture, boolean train, int idx) {

        HashMap<String, DoubleMatrix> embeddings = pair.getEmbeddings(vocab);

        for(Sentence s : pair.getSentences()) {
            forwardPropagateRNN(s.getArch(chainArchitecture), embeddings, theta);
        }

        DLTree t1 = pair.getSentences()[0].getArch(chainArchitecture);
        DLTree t2 = pair.getSentences()[1].getArch(chainArchitecture);

//        System.out.println("tree1: " + t1);
//        System.out.println("tree2: " + t2);
        forwardPropagateToSoftmax(t1, t2, pair.getLabel(idx), theta, train);
    }

    public void backPropagate(DLTree t1, DLTree t2, Theta theta) {
        backPropagateRNN(t1, theta);
        backPropagateRNN(t2, theta);
    }

    protected abstract void backPropagateRNN(DLTree tree, Theta theta);

    protected abstract void forwardPropagateToSoftmax(DLTree t1, DLTree t2, float label, Theta theta, boolean train);

    protected abstract void forwardPropagateRNN(DLTree tree, HashMap<String, DoubleMatrix> embeddings, Theta theta);

    public double getTagging() {
        return tagging;
    }

    public double getCost() {
        return cost;
    }

    public HashMap<String, DoubleMatrix> getWGradients() {
        return W_upd;
    }

    public HashMap<String, DoubleMatrix> getBGradients() { return b_upd; }

    void updateWGrad(String key, DoubleMatrix value) {
        DoubleMatrix toInsert = value.dup();
        if(! W_upd.containsKey(key)) {
            W_upd.put(key, toInsert);
        }
        else {
            DoubleMatrix old = W_upd.get(key);
            W_upd.put(key, old.addi(toInsert));
        }
    }

    void updateBGrad(String key, DoubleMatrix value) {
        DoubleMatrix toInsert = value.dup();
        if(! b_upd.containsKey(key)) {
            b_upd.put(key, toInsert);
        }
        else {
            DoubleMatrix old = b_upd.get(key);
            b_upd.put(key, old.addi(toInsert));
        }
    }

    public DoubleMatrix getPredicted() {
        return predicted;
    }
}
