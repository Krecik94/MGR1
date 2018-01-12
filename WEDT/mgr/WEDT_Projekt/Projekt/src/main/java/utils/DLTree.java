package utils;

import edu.stanford.nlp.trees.LabeledScoredTreeNode;
import org.jblas.DoubleMatrix;

import java.util.ArrayList;

/**
 * Created by b.rychalska on 23.10.15.
 */
public class DLTree extends LabeledScoredTreeNode {
    private DoubleMatrix x;
//    String dependency;
//    String word;

//    public DLTree(int hiddenLayers) {
//        hiddenH = new DoubleMatrix[hiddenLayers];
//        hiddenHDelta = new DoubleMatrix[hiddenLayers];
//        this.hiddenLayers = hiddenLayers;
//    }
//
//    public int hiddenLayers;
//    public DoubleMatrix[] hiddenH;
//    public DoubleMatrix[] hiddenHDelta;

    private DoubleMatrix c;
    private DoubleMatrix h;
    private DoubleMatrix h1;
    private DoubleMatrix o;

    private DoubleMatrix z;
    private DoubleMatrix r;
    private DoubleMatrix hProp;
    private DoubleMatrix totalChildH;

    private DoubleMatrix deltaH;
    private DoubleMatrix deltaH1;
    private DoubleMatrix deltaC;
    private DoubleMatrix deltaO;

    private DoubleMatrix deltaZ;
    private DoubleMatrix deltaR;
    private DoubleMatrix deltaHprop;
//    public DoubleMatrix getFeatureVector() {
//        return featureVector;
//    }
//
//    public void setFeatureVector(DoubleMatrix featureVector) {
//        this.featureVector = featureVector;
//    }


    public DoubleMatrix getDeltaH1() {
        return deltaH1;
    }

    public void setDeltaH1(DoubleMatrix deltaH1) {
        this.deltaH1 = deltaH1;
    }

    public DoubleMatrix getH1() {
        return h1;
    }

    public void setH1(DoubleMatrix h1) {
        this.h1 = h1;
    }

    public DoubleMatrix getH() {
        return h;
    }

    public void setH(DoubleMatrix h) {
        this.h = h;
    }

    public DoubleMatrix getDeltaH() {
        return deltaH;
    }

    public void setDeltaH(DoubleMatrix delta) {
        this.deltaH = delta;
    }

    public DoubleMatrix getX() {
        return x;
    }

    public void setX(DoubleMatrix x) {
        this.x = x;
    }

    public DoubleMatrix getO() {
        return o;
    }

    public void setO(DoubleMatrix o) {
        this.o = o;
    }

    public DoubleMatrix getC() {
        return c;
    }

    public void setC(DoubleMatrix h1) {
        this.c = h1;
    }

    public DoubleMatrix getDeltaO() {
        return deltaO;
    }

    public void setDeltaO(DoubleMatrix deltaO) {
        this.deltaO = deltaO;
    }

    public DoubleMatrix getDeltaC() {
        return deltaC;
    }

    public void setDeltaC(DoubleMatrix deltaC) {
        this.deltaC = deltaC;
    }

    public DoubleMatrix getZ() {
        return z;
    }

    public void setZ(DoubleMatrix z) {
        this.z = z;
    }

    public DoubleMatrix getR() {
        return r;
    }

    public void setR(DoubleMatrix r) {
        this.r = r;
    }

    public DoubleMatrix gethProp() {
        return hProp;
    }

    public void sethProp(DoubleMatrix hProp) {
        this.hProp = hProp;
    }

    public DoubleMatrix getDeltaZ() {
        return deltaZ;
    }

    public void setDeltaZ(DoubleMatrix deltaZ) {
        this.deltaZ = deltaZ;
    }

    public DoubleMatrix getDeltaR() {
        return deltaR;
    }

    public void setDeltaR(DoubleMatrix deltaR) {
        this.deltaR = deltaR;
    }

    public DoubleMatrix getDeltaHprop() {
        return deltaHprop;
    }

    public void setDeltaHprop(DoubleMatrix deltaHprop) {
        this.deltaHprop = deltaHprop;
    }

    public DoubleMatrix getTotalChildH() {
        return totalChildH;
    }

    public void setTotalChildH(DoubleMatrix totalChildH) {
        this.totalChildH = totalChildH;
    }
}
