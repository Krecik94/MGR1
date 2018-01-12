package rnn;

import data.Dataset;
import data.SentencePair;
import data.Vocabulary;
import edu.stanford.nlp.optimization.DiffFunction;
import org.jblas.DoubleMatrix;
import utils.ArrayUtils;

/**
 * Created by b.rychalska on 27.10.15.
 */
public class RNNTrainer implements DiffFunction {

    Theta initTheta;
    Dataset dataset;
    Vocabulary vocab;

//    int numNodes = 0;

    int numExamples = 0;

    double cost = 0.0;
    double reg;
    PropagatorFactory propagatorFactory;
    int rnnArchitecture;
    int chainArchitecture;
    int labelIdx;
//    double norm;

    public RNNTrainer(Theta theta, Dataset dataset, Vocabulary vocab, double reg, int rnnArchitecture, int chainArchitecture, int labelIdx) {
        this.initTheta = theta;
        this.dataset = dataset;
        this.vocab = vocab;
        this.reg = reg;
        this.propagatorFactory = new PropagatorFactory(rnnArchitecture);
        this.rnnArchitecture = rnnArchitecture;
        this.chainArchitecture = chainArchitecture;
        this.labelIdx = labelIdx;
    }

    private Propagator fullRun(Theta theta) {
        cost = 0.0;
//        numNodes = 0;
        numExamples = 0;

        Propagator propagator = propagatorFactory.createPropagator();

//        VanillaRNNPropagator propagate = new VanillaRNNPropagator();

//        SentencePair pair = dataset.getTrainset().get(1);
        for(SentencePair pair : dataset.getTrainset()) {
//            numNodes += pair.getPairLength();

            numExamples += 1;
            propagator.forwardPropagate(pair, vocab, theta, chainArchitecture, true, labelIdx);
        }
        return propagator;
    }

    public double[] derivativeAt(double[] doubles) {
        Theta theta = new Theta(doubles, initTheta.hiddenSize, initTheta.catSize, rnnArchitecture);
//        System.out.println("theta W after: " + theta.get("W"));
//        System.out.println("theta U after: " + theta.get("U"));

        Propagator p = fullRun(theta);
        cost = p.getCost();

        double[] gradient = (new Theta(p.getWGradients(), p.getBGradients(), rnnArchitecture)).Theta;

//        cost = (1.0 / numNodes) * cost; // + 0.5 * reg * norm;

        ArrayUtils.scale(gradient, (1.0f / numExamples));
        Theta grad = new Theta(gradient, initTheta.hiddenSize, initTheta.catSize, rnnArchitecture);
//        checkGradient(grad, theta);

        return gradient;
    }

    public double valueAt(double[] doubles) {
        Theta theta = new Theta(doubles, initTheta.hiddenSize, initTheta.catSize, rnnArchitecture);
        Propagator p = fullRun(theta);
        cost = p.getCost();
        cost = (1.0 / numExamples) * cost; // + 0.5 * reg * norm;

        return cost;
    }

    public int domainDimension() {
        //System.out.println("domainDimension: " + initTheta.getThetaSize());
        return initTheta.getThetaSize();
    }


    private void checkGradient(Theta grad, Theta theta) {
        double epsilon = 10e-6;

        double totalDiff = 0.0;
        for(String s : theta.keySet) {
            totalDiff = 0.0;
            System.out.println("\nGradientChecker: checking matrix " + s);
            DoubleMatrix matrix = theta.getW(s);
            for (int i = 0; i < matrix.rows * matrix.columns; i++) {
                Theta theta1 = new Theta(theta, rnnArchitecture);
                theta1.increaseMatrix(s, i, epsilon);
                Propagator propagator1 = fullRun(theta1);

                Theta theta2 = new Theta(theta, rnnArchitecture);
                theta2.increaseMatrix(s, i, -epsilon);
                Propagator propagator2 = fullRun(theta2);

                double p1cost = (1.0 / numExamples) * propagator1.getCost(); //+ 0.5 * reg * norm;
                double p2cost = (1.0 / numExamples) * propagator2.getCost(); //+ 0.5 * reg * norm;
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
