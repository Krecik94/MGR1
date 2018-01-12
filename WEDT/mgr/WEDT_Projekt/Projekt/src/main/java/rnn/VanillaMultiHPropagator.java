package rnn;

import data.Sentence;
import data.SentencePair;
import data.Vocabulary;
import edu.stanford.nlp.trees.Tree;
import org.jblas.DoubleMatrix;
import utils.*;

import java.util.HashMap;

/**
 * Created by b.rychalska on 04.11.15.
 */
public class VanillaMultiHPropagator extends Propagator {

    protected void forwardPropagateToSoftmax(DLTree t1, DLTree t2, float label, Theta theta, boolean train) {
        DoubleMatrix softmaxW11 = theta.getW("softmaxW11");
        DoubleMatrix softmaxW12 = theta.getW("softmaxW12");
        DoubleMatrix softmaxW2 = theta.getW("softmaxW2");

        DoubleMatrix h1 = t1.getH1();
        DoubleMatrix h2 = t2.getH1();

        DoubleMatrix firstTransform = Tanh.value(softmaxW11.mmul(h1).add(softmaxW12.mmul(h2)).addColumnVector(theta.getb("softmaxW1")));
        DoubleMatrix secondTransform = softmaxW2.mmul(firstTransform);

//        System.out.println("h1: " + h1);
//        System.out.println("h2: " + h2);
//        System.out.println("second transform: " + secondTransform);
        predicted = Softmax.value(secondTransform);

        double norm = 0.0;
        for(String key : theta.keySet) {
            norm += DoubleMatrixUtils.SquaredNorm(theta.getW(key));
        }

//        tagging = getRoundedTagging(predicted);
//        DoubleMatrix target = getRoundedTarget(theta, label);

        tagging = getWeightedTagging(predicted);
        DoubleMatrix target = getWeightedTarget(theta, label);


        cost += LogLoss.value(predicted, target, theta.catSize) + 0.5 * reg * norm;

//        System.out.println("train: " + train);
//        if(! train) {
//            System.out.println("h1: " + h1);
//            System.out.println("h2: " + h2);
//            System.out.println("euclid dist: " + h1.squaredDistance(h2));
//        }

        if(! train ) return;

        DoubleMatrix delta = LogLoss.derivative(predicted, target);
        DoubleMatrix softmaxW2_upd = delta.mmul(firstTransform.transpose()).add(theta.getW("softmaxW2").mul(reg));

        DoubleMatrix firstTransform_activation = (softmaxW2.transpose()).mmul(delta);
        DoubleMatrix firstTransform_delta = Tanh.derivative(firstTransform).mul(firstTransform_activation);
        DoubleMatrix softmaxW11_upd = firstTransform_delta.mmul(h1.transpose()).add(theta.getW("softmaxW11").mul(reg));
//        softmaxW1_upd.addi(firstTransform_delta.mmul(h2.transpose()));
        DoubleMatrix softmaxW12_upd = firstTransform_delta.mmul(h2.transpose()).add(theta.getW("softmaxW12").mul(reg));

        updateWGrad("softmaxW2", softmaxW2_upd);
        updateWGrad("softmaxW11", softmaxW11_upd);
        updateWGrad("softmaxW12", softmaxW12_upd);
        updateBGrad("softmaxW1", firstTransform_delta);

        DoubleMatrix h1_activation = (softmaxW11.transpose()).mmul(firstTransform_delta);
        DoubleMatrix h2_activation = (softmaxW12.transpose()).mmul(firstTransform_delta);
        DoubleMatrix h1_delta = Tanh.derivative(h1).mul(h1_activation);
        DoubleMatrix h2_delta = Tanh.derivative(h2).mul(h2_activation);


        DoubleMatrix t1childActivation = theta.getW("U1").transpose().mmul(h1_delta);
        DoubleMatrix h1_child_delta = Tanh.derivative(t1.getH()).mul(t1childActivation);
        DoubleMatrix U1_upd1 = h1_delta.mmul(t1.getH().transpose());//.add(theta.get("W").mul(reg));
        updateWGrad("U1", U1_upd1);
//        updatebGrad("W1", h1_delta);

        DoubleMatrix t2childActivation = theta.getW("U1").transpose().mmul(h2_delta);
        DoubleMatrix h2_child_delta = Tanh.derivative(t2.getH()).mul(t2childActivation);
        DoubleMatrix U1_upd2 = h2_delta.mmul(t2.getH().transpose());//.add(theta.get("W").mul(reg));
        updateWGrad("U1", U1_upd2);
//        updatebGrad("W1", h2_delta);

        t1.setDeltaH1(h1_delta);
        t1.setDeltaH(h1_child_delta);
        t2.setDeltaH1(h2_delta);
        t2.setDeltaH(h2_child_delta);
        backPropagate(t1, t2, theta);

        updateWGrad("W", theta.getW("W").mul(reg));
        updateWGrad("U", theta.getW("U").mul(reg));
        updateWGrad("W1", theta.getW("W1").mul(reg));
        updateWGrad("U1", theta.getW("U1").mul(reg));
    }


    protected void backPropagateRNN(DLTree tree, Theta theta) {
        if(tree.isLeaf()) {}
        else {
            DoubleMatrix hiddenDeltaH = tree.getDeltaH1();
            updateBGrad("W1", hiddenDeltaH);
            DoubleMatrix deltaH = tree.getDeltaH();
            updateBGrad("W", deltaH);

            for (Tree t : tree.children()) {
                DLTree dlTree = (DLTree) t;

                if (dlTree.isLeaf()) {
                    DoubleMatrix U_upd = deltaH.mmul(dlTree.getX().transpose());
                    updateWGrad("U", U_upd);
//                    updatebGrad("W", deltaH);
                } else {
                    DoubleMatrix childHiddenActivationH = (theta.getW("W1").transpose()).mmul(hiddenDeltaH);
                    DoubleMatrix childHiddenDelta = Tanh.derivative(dlTree.getH1()).mul(childHiddenActivationH);
                    DoubleMatrix W1_upd = hiddenDeltaH.mmul(dlTree.getH1().transpose());
                    dlTree.setDeltaH1(childHiddenDelta);
                    updateWGrad("W1", W1_upd);


                    DoubleMatrix U1_upd = childHiddenDelta.mmul(dlTree.getH().transpose());
                    updateWGrad("U1", U1_upd);
//                    updatebGrad("W1", childHiddenDelta);

                    DoubleMatrix childActivation = (theta.getW("W").transpose()).mmul(deltaH).addi(theta.getW("U1").transpose().mmul(childHiddenDelta));
                    DoubleMatrix childDelta = Tanh.derivative(dlTree.getH()).mul(childActivation);
                    dlTree.setDeltaH(childDelta);

                    DoubleMatrix W_upd = deltaH.mmul(dlTree.getH().transpose());
                    updateWGrad("W", W_upd);
//                    updatebGrad("W", deltaH);

                    backPropagateRNN(dlTree, theta);
                }
            }
        }
    }

    protected void forwardPropagateRNN(DLTree tree, HashMap<String, DoubleMatrix> embeddings, Theta theta) {
        if(tree.isLeaf()) {
            DoubleMatrix features = embeddings.get(tree.label().toString());
            tree.setX(features);

//            System.out.println("tree: " + tree);
//            System.out.println("vector: " + features);
        }
        else {
            DoubleMatrix totalH = new DoubleMatrix(theta.hiddenSize, 1);
            DoubleMatrix totalHiddenH = new DoubleMatrix(theta.hiddenSize, 1);
            DoubleMatrix x = new DoubleMatrix(theta.hiddenSize, 1);

            for( Tree t : tree.children()) {
                DLTree dlTree = (DLTree)t;
                forwardPropagateRNN(dlTree, embeddings, theta);
                if(dlTree.isLeaf()) {
                    x = dlTree.getX();
//                    System.out.println("tree: " + dlTree);
//                    System.out.println("x: " + x);
                }
                else {
                    totalH.addi(dlTree.getH());
                    totalHiddenH.addi(dlTree.getH1());
                }
            }

            DoubleMatrix actH = theta.getW("U").mmul(x).addi(theta.getW("W").mmul(totalH));
            DoubleMatrix h = Tanh.value(actH.addColumnVector(theta.getb("W")));

            DoubleMatrix actH1 = theta.getW("U1").mmul(h).addi(theta.getW("W1").mmul(totalHiddenH));
            DoubleMatrix h1 = Tanh.value(actH1.addColumnVector(theta.getb("W1")));
            tree.setX(x);
            tree.setH(h);
            tree.setH1(h1);
        }
    }


}
