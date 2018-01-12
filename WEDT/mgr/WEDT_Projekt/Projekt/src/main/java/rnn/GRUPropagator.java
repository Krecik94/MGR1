package rnn;

import data.Sentence;
import data.SentencePair;
import data.Vocabulary;
import edu.stanford.nlp.trees.Tree;
import org.jblas.DoubleMatrix;
import utils.*;

import java.util.Arrays;
import java.util.HashMap;

/**
 * Created by b.rychalska on 31.10.15.
 */
public class GRUPropagator extends Propagator{

    protected void forwardPropagateToSoftmax(DLTree t1, DLTree t2, float label, Theta theta, boolean train) {
        int roundedLabel = (int) Math.round(label);

        DoubleMatrix softmaxW11 = theta.getW("softmaxW11");
        DoubleMatrix softmaxW12 = theta.getW("softmaxW12");
        DoubleMatrix softmaxW2 = theta.getW("softmaxW2");
        DoubleMatrix target = new DoubleMatrix(theta.catSize, 1);
        target.put(roundedLabel, 1);

        DoubleMatrix h1 = t1.getH();
        DoubleMatrix h2 = t2.getH();

        DoubleMatrix firstTransform = Tanh.value(softmaxW11.mmul(h1).add(softmaxW12.mmul(h2)));//.addColumnVector(theta.getb("softmaxW1")));
        DoubleMatrix secondTransform = softmaxW2.mmul(firstTransform);

        predicted = Softmax.value(secondTransform);

        tagging = predicted.argmax();

        double norm = 0.0;
        for(String key : theta.keySet) {
            norm += DoubleMatrixUtils.SquaredNorm(theta.getW(key));
        }
        cost += LogLoss.value(predicted, target, theta.catSize);// + 0.5 * reg * norm;

        DoubleMatrix delta = LogLoss.derivative(predicted, target);
        DoubleMatrix softmaxW2_upd = delta.mmul(firstTransform.transpose());//.add(theta.getW("softmaxW2").mul(reg));

        DoubleMatrix firstTransform_activation = (softmaxW2.transpose()).mmul(delta);
        DoubleMatrix firstTransform_delta = Tanh.derivative(firstTransform).mul(firstTransform_activation);
        DoubleMatrix softmaxW11_upd = firstTransform_delta.mmul(h1.transpose());//.add(theta.getW("softmaxW11").mul(reg));
        DoubleMatrix softmaxW12_upd = firstTransform_delta.mmul(h2.transpose());//.add(theta.getW("softmaxW12").mul(reg));

        updateWGrad("softmaxW2", softmaxW2_upd);
        updateWGrad("softmaxW11", softmaxW11_upd);
        updateWGrad("softmaxW12", softmaxW12_upd);

        DoubleMatrix h1_activation = (softmaxW11.transpose()).mmul(firstTransform_delta);
        DoubleMatrix h2_activation = (softmaxW12.transpose()).mmul(firstTransform_delta);


        DoubleMatrix t1C = t1.getC();
        DoubleMatrix t1Z = t1.getZ();
        DoubleMatrix t1R = t1.getR();

        DoubleMatrix h1_deltaC = Tanh.derivative(t1C).mul(t1Z.rsub(1.0)).muli(h1_activation);
        DoubleMatrix h1_deltaZ = Sigmoid.derivative(t1Z).mul(t1.getTotalChildH().sub(t1C)).muli(h1_activation);
        DoubleMatrix h1_deltaR = theta.getW("U").mmul(t1.getTotalChildH()).mul(Sigmoid.derivative(t1R)).mul(h1_deltaC);

        DoubleMatrix t2C = t2.getC();
        DoubleMatrix t2Z = t2.getZ();
        DoubleMatrix t2R = t2.getR();

        DoubleMatrix h2_deltaC = Tanh.derivative(t2C).mul(t2Z.rsub(1.0)).muli(h2_activation);
        DoubleMatrix h2_deltaZ = Sigmoid.derivative(t2Z).mul(t2.getTotalChildH().sub(t2C)).muli(h2_activation);
        DoubleMatrix h2_deltaR = theta.getW("U").mmul(t2.getTotalChildH()).mul(Sigmoid.derivative(t2R)).mul(h2_deltaC);
//        DoubleMatrix h2_deltaR = theta.getW("U").mmul(h2_deltaC).mul(t2.getTotalChildH()).mul(Sigmoid.derivative(t2R));

//        INDArray deltaR = deltaC.mulRowVector(wCdiag).muli(prevOut).muli(sigmaPrimeZr);


        t1.setDeltaZ(h1_deltaZ);
        t1.setDeltaR(h1_deltaR);
        t1.setDeltaC(h1_deltaC);
        t1.setDeltaH(h1_activation);


        t2.setDeltaZ(h2_deltaZ);
        t2.setDeltaR(h2_deltaR);
        t2.setDeltaC(h2_deltaC);
        t2.setDeltaH(h2_activation);


        backPropagate(t1, t2, theta);

//        updateWGrad("W", theta.getW("W").mul(reg));
//        updateWGrad("U", theta.getW("U").mul(reg));
//        updateWGrad("Wz", theta.getW("W").mul(reg));
//        updateWGrad("Uz", theta.getW("U").mul(reg));
//        updateWGrad("Wr", theta.getW("W").mul(reg));
//        updateWGrad("Ur", theta.getW("U").mul(reg));
    }

    public void backPropagate(DLTree t1, DLTree t2, Theta theta) {
        backPropagateRNN(t1, theta);
        backPropagateRNN(t2, theta);
    }

    protected void backPropagateRNN(DLTree tree, Theta theta) {
        if(tree.isLeaf()) {}
        else {
            DoubleMatrix deltaZ = tree.getDeltaZ();
            DoubleMatrix deltaR = tree.getDeltaR();
            DoubleMatrix deltaC = tree.getDeltaC();
            DoubleMatrix deltaH = tree.getDeltaH();
            DoubleMatrix parentZ = tree.getZ();
            DoubleMatrix parentC = tree.getC();
            DoubleMatrix parentR = tree.getR();

            for(Tree t: tree.children()) {
                DLTree dlTree = (DLTree) t;
                if(dlTree.isLeaf()) {
                    DoubleMatrix W_upd = deltaC.mmul(dlTree.getX().transpose());
                    updateWGrad("W", W_upd);

                    DoubleMatrix Wz_upd = deltaZ.mmul(dlTree.getX().transpose());
                    updateWGrad("Wz", Wz_upd);

                    DoubleMatrix Wr_upd = deltaR.mmul(dlTree.getX().transpose());
                    updateWGrad("Wr", Wr_upd);

                }
                else {
                    DoubleMatrix z = dlTree.getZ();
                    DoubleMatrix r = dlTree.getR();
                    DoubleMatrix c = dlTree.getC();
                    DoubleMatrix totalChildH = dlTree.getTotalChildH();
                    DoubleMatrix h = dlTree.getH();

                    DoubleMatrix outputActivationSum = new DoubleMatrix(50, 1);

                    DoubleMatrix diagUr = theta.getW("Ur").diag();

//                    diagUr.print();
//                    theta.getW("Ur").print();
                    DoubleMatrix diagU = theta.getW("U").diag();
                    DoubleMatrix diagUz = theta.getW("Uz").diag();
//                    DoubleMatrixUtils.printSize(diagUr);

                    outputActivationSum.addi( parentZ );
                    outputActivationSum.addi(
                            (h.sub(parentC)).mul(diagUz).mul(Sigmoid.derivative(parentZ)));
                    outputActivationSum.addi(
                            parentZ.rsub(1.0)
                                    .mul(Tanh.derivative(parentC))
                                    .mul( (Sigmoid.derivative(parentR).mul(diagUr).mul(theta.getW("U").mmul(h)).addi(diagU.mul(parentR)) )
                                    )
                    );
                    outputActivationSum.muli(deltaH);

//                    outputActivationSum.addi( parentZ );
//                    outputActivationSum.addi(
//                            h.sub(parentC)
//                                    .mul(theta.getW("Uz").mmul(
//                                            Sigmoid.derivative(parentZ))) );
//                    outputActivationSum.addi(
//                            parentZ.rsub(1.0)
//                                    .mul(Tanh.derivative(parentC))
//                                    .mul(theta.getW("U").mmul(
//                                            parentR.add(theta.getW("Ur").mmul(h).muli(Sigmoid.derivative(parentR))))));
//                    outputActivationSum.muli(deltaH);

                    DoubleMatrix childDeltaC = Tanh.derivative(c).mul(z.rsub(1.0)).muli(outputActivationSum);


                    dlTree.setDeltaC(childDeltaC);

                    DoubleMatrix childDeltaZ = Sigmoid.derivative(z).mul(totalChildH.sub(c)).muli(outputActivationSum);
                    dlTree.setDeltaZ(childDeltaZ);

                    DoubleMatrix childDeltaR = theta.getW("U").mmul(totalChildH).mul(Sigmoid.derivative(r)).mul(childDeltaC);
                    dlTree.setDeltaR(childDeltaR);

                    DoubleMatrix U_upd = deltaC.mul(parentR).mmul(h.transpose());
                    updateWGrad("U", U_upd);
                    DoubleMatrix Uz_upd = deltaZ.mmul(h.transpose());
                    updateWGrad("Uz", Uz_upd);
                    DoubleMatrix Ur_upd = deltaR.mmul(h.transpose());
                    updateWGrad("Ur", Ur_upd);

                    dlTree.setDeltaH(outputActivationSum);
                    backPropagateRNN(dlTree, theta);
                }
            }
        }
    }

    protected void forwardPropagateRNN(DLTree tree, HashMap<String, DoubleMatrix> embeddings, Theta theta) {
        if(tree.isLeaf()) {
//            System.out.println("got leaf: " + tree);
            DoubleMatrix features = embeddings.get(tree.label().toString());
            tree.setX(features);
        }

            DoubleMatrix totalH = new DoubleMatrix(50, 1);
            DoubleMatrix x = new DoubleMatrix(50, 1);

            for( Tree t : tree.children()) {
                DLTree dlTree = (DLTree)t;
                forwardPropagateRNN(dlTree, embeddings, theta);

//                dlTree.setH(DoubleMatrix.ones(50, 1));

                if(dlTree.isLeaf()) {
                    x = embeddings.get(dlTree.label().toString());
                    dlTree.setX(x);
                }
                else {
                    totalH.addi(dlTree.getH());
                }
            }
            DoubleMatrix z = Sigmoid.value(theta.getW("Wz").mmul(x).add( theta.getW("Uz").mmul(totalH) ));
            DoubleMatrix r = Sigmoid.value(theta.getW("Wr").mmul(x).add( theta.getW("Ur").mmul(totalH) ));
            DoubleMatrix c = Tanh.value(theta.getW("W").mmul(x).add(r.mul(theta.getW("U").mmul(totalH) ) ));
            DoubleMatrix h = z.mul(totalH).add(z.rsub(1.0).mul(c));

            tree.setTotalChildH(totalH);
            tree.setZ(z);
            tree.setR(r);
            tree.setC(c);
            tree.setH(h);

//            tree.setTotalChildH(DoubleMatrix.rand(50, 1));
//            tree.setZ(DoubleMatrix.rand(50, 1));
//            tree.setR(DoubleMatrix.rand(50, 1));
//            tree.setC(DoubleMatrix.rand(50, 1));
//            tree.setH(DoubleMatrix.rand(50, 1));
//            DoubleMatrix totalH = new DoubleMatrix(50, 1);
//            DoubleMatrix x = new DoubleMatrix(50, 1);
//
//            for( Tree t : tree.children()) {
//                DLTree dlTree = (DLTree)t;
//                forwardPropagateRNN(dlTree, embeddings, theta);
//                if(dlTree.isLeaf()) x = dlTree.getX();
//                else {
//                    totalH.addi(dlTree.getH());
//                }
//            }
//            DoubleMatrix z = Sigmoid.value(theta.getW("Wz").mmul(x).add( theta.getW("Uz").mmul(totalH) ));
//            DoubleMatrix r = Sigmoid.value(theta.getW("Wr").mmul(x).add( theta.getW("Ur").mmul(totalH) ));
//            DoubleMatrix c = Tanh.value(theta.getW("W").mmul(x).add( r.mul( theta.getW("U").mmul(totalH) ) ));
//            DoubleMatrix h = z.mul(totalH).addi(z.sub(1.0).mul(c) );
//
//            tree.setTotalChildH(totalH);
//            tree.setZ(z);
//            tree.setR(r);
//            tree.setC(c);
//            tree.setH(h);
//        }
    }

}
