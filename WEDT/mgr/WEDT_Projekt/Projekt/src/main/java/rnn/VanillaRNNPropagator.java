package rnn;

import data.Sentence;
import data.SentencePair;
import data.Vocabulary;
import edu.stanford.nlp.trees.Tree;
import org.jblas.DoubleMatrix;
import org.jblas.MatrixFunctions;
import utils.*;

import java.util.HashMap;

/**
 * Created by b.rychalska on 22.10.15.
 */
public class VanillaRNNPropagator extends Propagator {

    protected void forwardPropagateToSoftmax(DLTree t1, DLTree t2, float label, Theta theta, boolean train) {
        int roundedLabel = (int) Math.round(label);

        DoubleMatrix softmaxW11 = theta.getW("softmaxW11");
        DoubleMatrix softmaxW12 = theta.getW("softmaxW12");
        DoubleMatrix softmaxW2 = theta.getW("softmaxW2");
        //DoubleMatrix target = new DoubleMatrix(theta.catSize, 1);
        //target.put(roundedLabel, 1);

        DoubleMatrix h1 = t1.getH();
        DoubleMatrix h2 = t2.getH();

        DoubleMatrix firstTransform = Tanh.value(softmaxW11.mmul(h1).add(softmaxW12.mmul(h2)).addColumnVector(theta.getb("softmaxW1")));
        DoubleMatrix secondTransform = softmaxW2.mmul(firstTransform);

        predicted = Softmax.value(secondTransform);

        tagging = predicted.argmax();

        double norm = 0.0;
        for(String key : theta.keySet) {
            norm += DoubleMatrixUtils.SquaredNorm(theta.getW(key));
        }

        //tagging = getWeightedTagging(predicted);
        DoubleMatrix target = getWeightedTarget(theta, label);
//        tagging = getRoundedTagging(predicted);
//        DoubleMatrix target = getRoundedTarget(theta, label);

        cost += LogLoss.value(predicted, target, theta.catSize) + 0.5 * reg * norm;

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

        t1.setDeltaH(h1_delta);
        t2.setDeltaH(h2_delta);
        backPropagate(t1, t2, theta);

        updateWGrad("W", theta.getW("W").mul(reg));
        updateWGrad("U", theta.getW("U").mul(reg));

    }

    protected void backPropagateRNN(DLTree tree, Theta theta) {
        if(tree.isLeaf()) {}
        else {
            DoubleMatrix delta = tree.getDeltaH();
            updateBGrad("W", delta);

            for(Tree t: tree.children()) {
                DLTree dlTree = (DLTree) t;

                if(dlTree.isLeaf()) {
                    DoubleMatrix childLeafActivation = (theta.getW("U").transpose()).mmul(delta);
                    DoubleMatrix childLeafDelta = Tanh.derivative(dlTree.getX()).mul(childLeafActivation);
                    DoubleMatrix U_upd = delta.mmul(dlTree.getX().transpose());
                    updateWGrad("U", U_upd);
                }
                else {
                    DoubleMatrix childActivation = (theta.getW("W").transpose()).mmul(delta);
                    DoubleMatrix childDelta = Tanh.derivative(dlTree.getH()).mul(childActivation);
                    dlTree.setDeltaH(childDelta);

                    DoubleMatrix W_upd = delta.mmul(dlTree.getH().transpose());//.add(theta.get("W").mul(reg));
                    updateWGrad("W", W_upd);

                    backPropagateRNN(dlTree, theta);
                }
            }
        }
    }

    protected void forwardPropagateRNN(DLTree tree, HashMap<String, DoubleMatrix> embeddings, Theta theta) {
        if(tree.isLeaf()) {
            DoubleMatrix features = embeddings.get(tree.label().toString());
            tree.setX(features);
        }
        else {
            DoubleMatrix totalH = new DoubleMatrix(theta.hiddenSize, 1);
            DoubleMatrix x = new DoubleMatrix(theta.hiddenSize, 1);

            for( Tree t : tree.children()) {
                DLTree dlTree = (DLTree) t;
                forwardPropagateRNN(dlTree, embeddings, theta);
                if(dlTree.isLeaf()) x = dlTree.getX();
                else totalH.addi(theta.getW("W").mmul(dlTree.getH()));
            }
            DoubleMatrix h = Tanh.value(theta.getW("U").mmul(x).addi(totalH).addColumnVector(theta.getb("W")));
            tree.setX(x);
            tree.setH(h);
        }
    }
}
