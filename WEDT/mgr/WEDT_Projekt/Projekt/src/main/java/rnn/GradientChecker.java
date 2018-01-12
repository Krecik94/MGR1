package rnn;

import data.SentencePair;
import data.Vocabulary;
import org.jblas.DoubleMatrix;

/**
 * Created by b.rychalska on 25.10.15.
 */
public class GradientChecker {


    public void checkGradients(SentencePair pair, Vocabulary vocab, Theta theta, int rnnArchitecture, int chainArchitecture) {
        System.out.println("pair: " +  pair.getSentences()[0].getLinearTree() + "\n" + pair.getSentences()[1].getLinearTree());

        double epsilon = 1e-4;
        PropagatorFactory factory = new PropagatorFactory(rnnArchitecture);

        Propagator propagator = factory.createPropagator();
        propagator.forwardPropagate(pair,vocab,theta, chainArchitecture, true, 0);

        double totalDiff = 0.0;
        for(String s : new String[]{ "W", "U", "Ur", "Uz", "Wr", "Wz" } ) {//theta.keySet) {
            totalDiff = 0.0;
            System.out.println("\nGradientChecker: checking matrix " + s);
            DoubleMatrix matrix = theta.getW(s);
            for (int i = 0; i < matrix.rows * matrix.columns; i++) {
//            for (int i = 0; i < 3; i++) {
                Theta theta1 = new Theta(theta, rnnArchitecture);
                theta1.increaseMatrix(s, i, epsilon);

                Propagator propagator1 = factory.createPropagator();
                propagator1.forwardPropagate(pair, vocab, theta1, chainArchitecture, true, 0);

                Theta theta2 = new Theta(theta, rnnArchitecture);
                theta2.increaseMatrix(s, i, -epsilon);

                Propagator propagator2 = factory.createPropagator();
                propagator2.forwardPropagate(pair, vocab, theta2, chainArchitecture, true, 0);

                double t = (propagator1.getCost() - propagator2.getCost()) / (2 * epsilon);
//            System.out.println("target: " + t);
                double p = propagator.getWGradients().get(s).get(i);
//            System.out.println("approximated: " + p);

                totalDiff += t - p;
//                System.out.println("diff: " + (t - p));
            }
            double avgDiff = totalDiff / (matrix.rows + matrix.columns);
            System.out.println("+++ matrix " + s + " average diff: " + avgDiff);
            if (Math.abs(avgDiff) < 1e-6) {
                System.out.println("average diff ok ( < 1e-6 )");
            } else {
                System.out.println("WARNING: average diff > 1e-6");
            }
        }

    }

}
