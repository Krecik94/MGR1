package rnn;

import data.Dataset;
import data.SentencePair;
import data.Vocabulary;
import org.jblas.DoubleMatrix;
import utils.ArrayUtils;

/**
 * Created by b.rychalska on 09.11.15.
 */
public class SGD {

    Theta initTheta;
    Dataset dataset;
    Vocabulary vocab;
    int numNodes = 0;
    double cost = 0.0;
    double reg;
    PropagatorFactory propagatorFactory;
    int rnnArchitecture;
    int chainArchitecture;

    double step = 10e-3;
//    double norm;

    public SGD(Theta theta, Dataset dataset, Vocabulary vocab, double reg, int rnnArchitecture, int chainArchitecture) {
        this.initTheta = theta;
        this.dataset = dataset;
        this.vocab = vocab;
        this.reg = reg;
        this.propagatorFactory = new PropagatorFactory(rnnArchitecture);
        this.rnnArchitecture = rnnArchitecture;
        this.chainArchitecture = chainArchitecture;
    }


    public Theta performOnline(int iterations) {
        Theta theta = new Theta(initTheta, rnnArchitecture);

        for(SentencePair pair : dataset.getTrainset()) {
            for(int i = 0; i<iterations; i++) {
                Theta gradient = getGradientOnline(pair, theta);
//                System.out.println("SGD iter: " + i + " cost: " + cost);
                theta.substractGradient(gradient, step);
            }
        }
        return theta;
    }

    private Propagator fullRunOnline(SentencePair pair, Theta theta) {

        Propagator propagator = propagatorFactory.createPropagator();
        propagator.forwardPropagate(pair, vocab, theta, chainArchitecture, true, 0);
        return propagator;
    }

    public Theta getGradientOnline(SentencePair pair, Theta initTheta) {
        Theta theta = new Theta(initTheta, rnnArchitecture);

        Propagator p = fullRunOnline(pair, theta);
        cost = p.getCost();

        Theta gradient = new Theta(p.getWGradients(), p.getBGradients(), rnnArchitecture);
//        Theta grad = new Theta(gradient, initTheta.hiddenSize, initTheta.catSize, rnnArchitecture);

//        checkGradient(pair, gradient, theta);
        return gradient;
    }


    private void checkGradient(SentencePair pair, Theta grad, Theta theta) {
        double epsilon = 10e-6;

        double totalDiff = 0.0;
        for(String s : theta.keySet) {
            totalDiff = 0.0;
            System.out.println("\nGradientChecker: checking matrix " + s);
            DoubleMatrix matrix = theta.getW(s);
            for (int i = 0; i < matrix.rows * matrix.columns; i++) {
                Theta theta1 = new Theta(theta, rnnArchitecture);
                theta1.increaseMatrix(s, i, epsilon);
                Propagator propagator1 = fullRunOnline(pair, theta1);

                Theta theta2 = new Theta(theta, rnnArchitecture);
                theta2.increaseMatrix(s, i, -epsilon);
                Propagator propagator2 = fullRunOnline(pair, theta2);

                double p1cost = propagator1.getCost(); //+ 0.5 * reg * norm;
                double p2cost = propagator2.getCost(); //+ 0.5 * reg * norm;
                double t = (p1cost - p2cost) / (2 * epsilon);
                double p = grad.getW(s).get(i);

                totalDiff += t - p;
//                System.out.println("target: " + t);
//                System.out.println("approximated: " + p);
                System.out.println("diff: " + (t - p));
            }
            double avgDiff = totalDiff / (matrix.rows + matrix.columns);
            System.out.println("+++ matrix " + s + " average diff: " + avgDiff);
            if (avgDiff < 10e-6) {
                System.out.println("average diff ok ( < 10e-6 )");
            } else {
                System.out.println("WARNING: average diff > 10e-6");
            }
        }
    }

}
