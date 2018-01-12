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
 * Created by b.rychalska on 30.10.15.
 */
public class TestRNNPropagator {// implements IPropagator{
//
//    HashMap<String, DoubleMatrix> upd;
//    double cost;
//    int tagging;
//    double reg = 1e-3;
//    public DoubleMatrix predicted;
//
//    public TestRNNPropagator() {
//        cost = 0.0;
//        upd = new HashMap<String, DoubleMatrix>();
//    }
//
//
//    public void forwardPropagate(SentencePair pair, Vocabulary vocab, Theta theta, int chainArchitecture) {
//        HashMap<String, DoubleMatrix> embeddings = pair.getEmbeddings(vocab);
//
////        System.out.println("got embeddings for pair: " + embeddings);
//
//        for(Sentence s : pair.getSentences()) {
//            forwardPropagateVanilla(s.getParseTree(), embeddings, theta);
//        }
//
//        DLTree t1 = pair.getSentences()[0].getArch(chainArchitecture);
//        DLTree t2 = pair.getSentences()[1].getArch(chainArchitecture);
//
//        forwardPropagateToSoftmax(t1, t2, pair.getSingleLabel(), theta);
//    }
//
//    private void forwardPropagateToSoftmax(DLTree t1, DLTree t2, float label, Theta theta) {
//        int roundedLabel = (int) Math.round(label);
//
//        DoubleMatrix softmaxW1 = theta.get("softmaxW1");
//        DoubleMatrix softmaxW2 = theta.get("softmaxW2");
//        DoubleMatrix target = new DoubleMatrix(theta.catSize, 1);
//        target.put(roundedLabel, 1);
//
//        DoubleMatrix h1 = t1.getH();
//        DoubleMatrix h2 = t2.getH();
//
//        //make it a tanh/sigmoid/..?
//        DoubleMatrix firstTransform = Tanh.value(softmaxW1.mmul(h1).addi(softmaxW1.mmul(h2)));
//        DoubleMatrix secondTransform = softmaxW2.mmul(firstTransform);
//
//        predicted = Softmax.value(secondTransform);
//
//        tagging = predicted.argmax();
//
//        double norm = 0.0;
//        for(String key : theta.keySet) {
//            norm += DoubleMatrixUtils.SquaredNorm(theta.get(key));
//        }
//        cost += LogLoss.value(predicted, target) + 0.5 * reg * norm;
//
//        DoubleMatrix delta = LogLoss.derivative(predicted, target);
//        DoubleMatrix softmaxW2_upd = delta.mmul(firstTransform.transpose()).add(theta.get("softmaxW2").mul(reg));
//
//        DoubleMatrix firstTransform_activation = (softmaxW2.transpose()).mmul(delta);
//        DoubleMatrix firstTransform_delta = Tanh.derivative(firstTransform).mul(firstTransform_activation);
//        DoubleMatrix softmaxW1_upd = firstTransform_delta.mmul(h1.transpose()).add(theta.get("softmaxW1").mul(reg));
//        softmaxW1_upd.addi(firstTransform_delta.mmul(h2.transpose()));
//
//        updateGrad("softmaxW2", softmaxW2_upd);
//        updateGrad("softmaxW1", softmaxW1_upd);
//        DoubleMatrix h1_activation = (softmaxW1.transpose()).mmul(firstTransform_delta);
//
//        DoubleMatrix h1_deltaC = t1.getO().mul(Tanh.derivative(t1.getC())).mul(h1_activation);
//        DoubleMatrix h1_deltaO = t1.getC().mul(Sigmoid.derivative(t1.getO())).mul(h1_activation);
//
//        DoubleMatrix h2_deltaC = t2.getO().mul(Tanh.derivative(t2.getC())).mul(h1_activation);
//        DoubleMatrix h2_deltaO = t2.getC().mul(Sigmoid.derivative(t2.getO())).mul(h1_activation);
//
////        DoubleMatrix h1_deltaH = Tanh.derivative(t1.getH()).mul(h1_activation);
////        DoubleMatrix h2_deltaH = Tanh.derivative(t2.getH()).mul(h1_activation);
//
//        t1.setDeltaO(h1_deltaO);
//        t1.setDeltaC(h1_deltaC);
////        t1.setDeltaH(h1_deltaH);
//
//        t2.setDeltaO(h2_deltaO);
//        t2.setDeltaC(h2_deltaC);
////        t2.setDeltaH(h2_deltaH);
//
//        backPropagate(t1, t2, theta);
//
//        updateGrad("W", theta.get("W").mul(reg));
//        updateGrad("U", theta.get("U").mul(reg));
//        updateGrad("Wo", theta.get("Wo").mul(reg));
//    }
//
//    public void backPropagate(DLTree t1, DLTree t2, Theta theta) {
//        backPropagateVanilla(t1, theta);
//        backPropagateVanilla(t2, theta);
//    }
//
//    private void backPropagateVanilla(DLTree tree, Theta theta) {
//        if(tree.isLeaf()) {}
//        else {
//            DoubleMatrix deltaO = tree.getDeltaO();
//            DoubleMatrix deltaC = tree.getDeltaC();
//
//            for(Tree t: tree.children()) {
//                DLTree dlTree = (DLTree) t;
//
//                if(dlTree.isLeaf()) {
//                    DoubleMatrix U_upd = deltaC.mmul(dlTree.getX().transpose());
//                    updateGrad("U", U_upd);
//                }
//                else {
//                    DoubleMatrix c = dlTree.getC();
//                    DoubleMatrix o = dlTree.getO();
//                    DoubleMatrix h = dlTree.getH();
//
////                    DoubleMatrix outputActivationSumC = (theta.get("W").transpose()).mmul(deltaC);
////                    DoubleMatrix outputActivationSumO = (theta.get("Wo").transpose()).mmul(deltaO);
//
//                    DoubleMatrix outputActivationSum = (theta.get("W").transpose()).mmul(deltaC);
//                    outputActivationSum.addi((theta.get("Wo").transpose()).mmul(deltaO));
//
//                    DoubleMatrix childCDelta = o.mul(Tanh.derivative(c)).mul(outputActivationSum);
//                    dlTree.setDeltaC(childCDelta);
//                    DoubleMatrix W_upd = deltaC.mmul(h.transpose());
//                    updateGrad("W", W_upd);
//
//                    DoubleMatrix oDelta = Sigmoid.derivative(o).mul(c).mul(outputActivationSum);
//                    dlTree.setDeltaO(oDelta);
//                    DoubleMatrix Wo_upd = deltaO.mmul(h.transpose());
//                    updateGrad("Wo", Wo_upd);
//
////                    dlTree.setDeltaH(deltaH);
//
//                    backPropagateVanilla(dlTree, theta);
//                }
//            }
//        }
//    }
//
//    private void updateGrad(String key, DoubleMatrix value) {
//        if(! upd.containsKey(key)) {
//            upd.put(key, value);
//        }
//        else {
//            DoubleMatrix old = upd.get(key);
//            upd.put(key, old.addi(value));
//        }
//    }
//
//    private void forwardPropagateVanilla(DLTree tree, HashMap<String, DoubleMatrix> embeddings, Theta theta) {
//        if(tree.isLeaf()) {
//            DoubleMatrix features = embeddings.get(tree.label().toString());
//            tree.setX(features);
//        }
//        else {
//            DoubleMatrix totalHW = new DoubleMatrix(50, 1);
//            DoubleMatrix totalHWo = new DoubleMatrix(50, 1);
//            DoubleMatrix x = new DoubleMatrix(50, 1);
//
//            for( Tree t : tree.children()) {
//                DLTree dlTree = (DLTree)t;
//                forwardPropagateVanilla(dlTree, embeddings, theta);
//                if(dlTree.isLeaf()) x = dlTree.getX();
//                else {
//                    totalHW.addi(theta.get("W").mmul(dlTree.getH()));
//                    totalHWo.addi(theta.get("Wo").mmul(dlTree.getH()));
//                }
//            }
//            DoubleMatrix c = Tanh.value(theta.get("U").mmul(x).addi(totalHW));
//
//            DoubleMatrix o = Sigmoid.value(totalHWo);
//
//            DoubleMatrix h = o.mul(c);
//
//            tree.setC(c);
//            tree.setO(o);
//            tree.setX(x);
//            tree.setH(h);
//        }
//    }
//
//
//    public int getTagging() {
//        return tagging;
//    }
//
//    public HashMap<String, DoubleMatrix> getGradients() {
//        return upd;
//    }
//
//    public double getCost() {
//        return cost;
//    }

}
