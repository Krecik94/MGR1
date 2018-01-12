package run;


import data.Dataset;
import data.ReadInputData;
import data.SentencePair;
import data.Vocabulary;
import edu.stanford.nlp.optimization.QNMinimizer;
import rnn.*;
import utils.Constants;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;

/**
 * Created by b.rychalska on 22.10.15.
 */
public class Main {

    public static void main(String[] args) {
        //set config values first in src/main/resources/config
        PropertyValues conf = new PropertyValues();
        try {
            conf.getPropValues();
        } catch (IOException e) {
            e.printStackTrace();
        }

        int iterations = conf.iterations;
        int catSize = conf.catSize;
        int hiddenSize = conf.hiddenSize;

        int rnnArchitecture = Constants.getValRNN(conf.rnnArch);
        int chainArchitecture = Constants.getValChain(conf.chainArch);

        boolean readDependenciesFromFile = true;
        boolean saveDependenciesToFile = false;
        double regularization = 1e-3;

        System.out.println("reading in sentences and dependencies...");
        ReadInputData r = new ReadInputData();
        Dataset data = r.readData(conf.dataPath, conf.embeddingsPath + hiddenSize + ".txt", readDependenciesFromFile, saveDependenciesToFile, hiddenSize);
        Vocabulary vocab = r.getVocab();

        int testIterations = 1;
        double F1A = 0, F1AT = 0, F1AS = 0, F1AST = 0;
        for(int i = 0; i < testIterations; i++) {
            Theta startTheta = new Theta(hiddenSize, catSize, rnnArchitecture);
            Theta alignNet = trainAndTest(data, vocab, rnnArchitecture, chainArchitecture, iterations, hiddenSize, catSize, regularization, startTheta, 0);
            Theta scoreNet = trainAndTest(data, vocab, rnnArchitecture, chainArchitecture, iterations, hiddenSize, catSize, regularization, startTheta, 1);

            F1Tester f1test = new F1Tester(data, vocab, alignNet, scoreNet, rnnArchitecture, chainArchitecture);
            F1A += f1test.F1A();
            F1AT += f1test.F1AT();
            F1AS += f1test.F1AS();
            F1AST += f1test.F1AST();
        }
        F1A /= testIterations;
        F1AT /= testIterations;
        F1AS /= testIterations;
        F1AST /= testIterations;

        System.out.println("F1A: " + F1A);
        System.out.println("F1AT: " + F1AT);
        System.out.println("F1AS: " + F1AS);
        System.out.println("F1AST: " + F1AST);

        /*
        readDependenciesFromFile = false;
        ArrayList<SentencePair> trainset = data.getTrainset();
        ArrayList<SentencePair> testset = data.getTestset();
        data = r.readData(conf.dataPath, conf.embeddingsPath + hiddenSize + ".txt", readDependenciesFromFile, saveDependenciesToFile, hiddenSize);
        data.setSets(trainset, testset);

        startTheta = new Theta(hiddenSize, catSize, rnnArchitecture);
        Theta scoreNet = trainAndTest(data, vocab, rnnArchitecture, chainArchitecture, iterations, hiddenSize, catSize, regularization, startTheta);
        */
//        try {
//            fullTest1(data, vocab, hiddenSize);
//        } catch (FileNotFoundException e) {
//            e.printStackTrace();
//        } catch (UnsupportedEncodingException e) {
//            e.printStackTrace();
//        }
    }


    public static Theta trainAndTest(Dataset data, Vocabulary vocab, int rnnArchitecture, int chainArchitecture, int iterations, int hiddenSize, int catSize, double regularization, Theta theta, int labelIdx) {
        System.out.println("starting LBFGS...");
        QNMinimizer lbfgs = new QNMinimizer();
        //lbfgs.shutUp();

        RNNTrainer rnnTrainer = new RNNTrainer(theta, data, vocab, regularization, rnnArchitecture, chainArchitecture, labelIdx);

        double[] trainedTheta = lbfgs.minimize(rnnTrainer, 1e-6, theta.Theta, iterations);
        Theta finalTheta = new Theta(trainedTheta, hiddenSize, catSize, rnnArchitecture);

        RNNTester rnnTester = new RNNTester(data, vocab, finalTheta, rnnArchitecture, chainArchitecture);
        double pearson = rnnTester.test(labelIdx);
        System.out.println("Pearson correlation: " +  pearson );
        rnnTester.sortDataByError();

        return finalTheta;
    }

    /*
    public static void fullTest(Dataset data, Vocabulary vocab, int hiddenSize) throws FileNotFoundException, UnsupportedEncodingException {
        int catSize = 6;
        double regularization = 1e-3;

        int nTests = 3;
        int nIter = 45;

        String outCatalog = "test-results/";
        String[] allRnn = new String[] {"VANILLA", "VANILLA_MULTI_H"};
        String[] allChain = new String[] {"LINEAR", "DEPENDENCY_TREE"};

        for (String s : allRnn) {
            for (String c : allChain) {
                String outFile = outCatalog + s + "-" + c + "-" + hiddenSize + "-" + "TASK2-images-iterCheck.txt";
                PrintWriter writer = new PrintWriter(outFile, "UTF-8");

                for (int iter = 2; iter <= nIter; iter+=2) {

                    double avgPearson = 0;
                    System.out.println("testing for: " + s + " " + c + " " + iter);

                    for(int i = 0; i < nTests; i++) {
                        Theta startTheta = new Theta(hiddenSize, catSize, Constants.getValRNN(s));
                        avgPearson += trainAndTest(data, vocab, Constants.getValRNN(s), Constants.getValChain(c), iter, hiddenSize, catSize, regularization, startTheta);
                    }

                    System.out.println("testing for: " + s + " " + c + " " + iter);
                    System.out.println("avg score: " + avgPearson/nTests);
                    writer.println(avgPearson/nTests);
                }

                writer.close();
            }
        }

    }
*/


}