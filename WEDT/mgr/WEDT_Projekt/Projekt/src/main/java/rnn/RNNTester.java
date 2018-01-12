package rnn;

import data.Dataset;
import data.SentencePair;
import data.Vocabulary;

import java.util.ArrayList;

/**
 * Created by b.rychalska on 04.11.15.
 */
public class RNNTester {
    Dataset data;
    Vocabulary vocab;
    Theta finalTheta;
    int rnnArchitecture;
    int chainArchitecture;


    public RNNTester(Dataset data, Vocabulary vocab, Theta finalTheta, int rnnArchitecture, int chainArchitecture) {
        this.data = data;
        this.vocab = vocab;
        this.finalTheta = finalTheta;
        this.rnnArchitecture = rnnArchitecture;
        this.chainArchitecture = chainArchitecture;
    }

    public double test(int idx) {
        Propagator propagator = PropagatorFactory.createPropagator(rnnArchitecture);
//        System.out.println("rnn arch: " + rnnArchitecture);
        double meanP = 0.0;
        double meanT = 0.0;
        ArrayList<Double> p = new ArrayList<Double>();
        ArrayList<Double> t = new ArrayList<Double>();
        for(SentencePair pair : data.getTestset()) {
            propagator.forwardPropagate(pair, vocab, finalTheta, chainArchitecture, false, idx);
            double predicted = propagator.getTagging();
            //double target =  Math.round(pair.getSingleLabel());
            double target = pair.getLabel(idx);
            p.add(predicted);
            t.add(target);
            meanP += predicted;
            meanT += target;
//            System.out.println("s1: " + pair.getSentences()[0].getS() + " " + pair.getSentences()[1].getS());
//            System.out.println("predicted: " + predicted + " target: " + target);
//            System.out.println("predicted vector: " + propagator.getPredicted());


            pair.setError(Math.abs(target-predicted));
        }

        meanP /= data.getTestset().size();
        meanT /= data.getTestset().size();

        double pearson = countPearson(p, t, meanP, meanT);
        //System.out.println("pearson correlation: " + pearson);

        return pearson;
    }

    public void sortDataByError() {
        ArrayList<SentencePair> sorted = data.sortTestsetByError();
        for(SentencePair pair : sorted) {
//            System.out.println("s1: " + pair.getSentences()[0].getS() + " " + pair.getSentences()[1].getS());
//            System.out.println("error: " + pair.getError());
        }
    }


    public double countPearson(ArrayList<Double> p, ArrayList<Double> t, double meanP, double meanT) {
        double res;
        double sumP = 0.0;
        double sumT = 0.0;
        double up = 0.0;
        for(int i = 0; i < p.size() ; i++) {
            double diffP = p.get(i) - meanP;
            double diffT = t.get(i) - meanT;

            sumP += Math.pow(diffP, 2);
            sumT += Math.pow(diffT, 2);
            up += diffP * diffT;
        }
        sumP = Math.sqrt(sumP);
        sumT = Math.sqrt(sumT);
        res = up / (sumP * sumT);
        return res;
    }


}
