package data;

import edu.stanford.nlp.trees.Tree;
import utils.Constants;
import utils.DLTree;

/**
 * Created by b.rychalska on 22.10.15.
 */
public class Sentence {

    String s;
    DLTree parseTree;
    DLTree linearTree;

    public Sentence(String s) {
        this.s = s;
    }


    public void setParseTree(DLTree parseTree) {
        this.parseTree = parseTree;
    }
    public void setLinearTree(DLTree linearTree) {
        this.linearTree = linearTree;
    }

    public DLTree getArch(int chainArchitecture) {
        if(chainArchitecture == Constants.DEPENDENCY_TREE) return parseTree;
        else if(chainArchitecture == Constants.LINEAR) return linearTree;
        else {
            System.err.print("Wrong argument for sentence architecture: " + chainArchitecture);
            return null;
        }
    }

    public String getS() {
        return s;
    }

    public int getLength() {
        return s.split(" ").length;
    }

    public DLTree getParseTree() {
        return parseTree;
    }
    public DLTree getLinearTree() {
        return linearTree;
    }
}
