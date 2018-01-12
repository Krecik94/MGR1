package rnn;

import data.Dataset;
import data.SentencePair;
import data.Vocabulary;

import java.util.HashMap;
import java.util.Vector;

public class F1Tester {
    Dataset data;
    Vocabulary vocab;
    Theta alignTheta;
    Theta scoreTheta;
    int rnnArchitecture;
    int chainArchitecture;

    enum OverlapMode { F1A, F1AT, F1AS, F1AST };

    static HashMap<String, Integer> labelTranslate = new HashMap<String, Integer>();

    int typeMatchScore = 0;
    double scoreMatchScore = 0;
    double bothMatchScore = 0;
    int gSet = 0;
    int sSet = 0;

    public F1Tester(Dataset data, Vocabulary vocab, Theta alignTheta, Theta scoreTheta, int rnnArchitecture, int chainArchitecture) {
        this.data = data;
        this.vocab = vocab;
        this.alignTheta = alignTheta;
        this.scoreTheta = scoreTheta;
        this.rnnArchitecture = rnnArchitecture;
        this.chainArchitecture = chainArchitecture;

        labelTranslate.put("EQUI", 0);
        labelTranslate.put("OPPO", 1);
        labelTranslate.put("SPE1", 2);
        labelTranslate.put("SPE2", 3);
        labelTranslate.put("SIMI", 4);
        labelTranslate.put("REL", 5);
        labelTranslate.put("NOALI", 6);

        Vector<Integer> cond1 = new Vector<Integer>();
        cond1.add(labelTranslate.get("SPE1"));
        cond1.add(labelTranslate.get("SPE2"));
        cond1.add(labelTranslate.get("SIMI"));
        cond1.add(labelTranslate.get("REL"));

        Vector<Integer> cond2 = new Vector<Integer>();
        cond2.add(labelTranslate.get("SPE1"));
        cond2.add(labelTranslate.get("SPE2"));
        cond2.add(labelTranslate.get("SIMI"));

        Propagator propagator = PropagatorFactory.createPropagator(rnnArchitecture);

        for(SentencePair pair : data.getTestset()) {
            propagator.forwardPropagate(pair, vocab, alignTheta, chainArchitecture, false, 0);
            double predictedType = propagator.getTagging();
            double targetType = pair.getLabel(0);

            propagator.forwardPropagate(pair, vocab, scoreTheta, chainArchitecture, false, 1);
            double predictedScore = propagator.getTagging();
            double targetScore = pair.getLabel(1);

            boolean alreadyAdded = false;

            double matchDelta = (1 - Math.abs(predictedScore - targetScore)/5);

            if(predictedScore <= 2 && targetScore <= 2 && cond1.contains((int)predictedType) && cond1.contains((int)targetType)) {
                bothMatchScore += matchDelta;
                alreadyAdded = true;
            } else if(predictedType == labelTranslate.get("EQUI") && targetScore == 4 && cond2.contains((int)targetType)) {
                bothMatchScore += matchDelta;
                alreadyAdded = true;
            } else if(targetType == labelTranslate.get("EQUI") && predictedScore == 4 && cond2.contains((int)predictedType)) {
                bothMatchScore += matchDelta;
                alreadyAdded = true;
            }

            if(predictedType == targetType) {
                typeMatchScore++;
                if(!alreadyAdded)
                    bothMatchScore += matchDelta;
            }

            scoreMatchScore += matchDelta;
        }

        gSet = data.getTestset().size();
        sSet = data.getTestset().size();
    }

    private double precision(OverlapMode mode) {
        return overlap(mode)/sSet;
    }

    private double recall(OverlapMode mode) {
        return overlap(mode)/gSet;
    }

    private double overlap(OverlapMode mode) {
        switch(mode) {
            case F1A:
                return sSet;
            case F1AT:
                return typeMatchScore;
            case F1AS:
                return scoreMatchScore;
            case F1AST:
            default:
                return bothMatchScore;
        }
    }

    private double F1(OverlapMode mode) {
        return 2 * (precision(mode)*recall(mode))/(precision(mode)+recall(mode));
    }

    // alignment type and score are ignored (always 1)
    public double F1A() {
        return F1(OverlapMode.F1A);
    }

    // alignment types need to match, but scores are ignored
    public double F1AT() {
        return F1(OverlapMode.F1AT);
    }

    // alignment type is ignored, but each alignment is penalized when scores do not match
    public double F1AS() {
        return F1(OverlapMode.F1AS);
    }

    // alignment types need to match, and each alignment is penalized when scores do not match
    // . there is no type penalty between tags {SPE1, SPE2, REL, SIMI} when scores are (0-2]
    // . there is no type penalty between EQUI and SIMI/SPE with score 4
    public double F1AST() {
        return F1(OverlapMode.F1AST);
    }
}