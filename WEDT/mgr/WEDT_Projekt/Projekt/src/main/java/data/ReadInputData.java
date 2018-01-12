package data;

import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.semgraph.SemanticGraph;
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations;
import edu.stanford.nlp.trees.*;
import edu.stanford.nlp.util.CoreMap;
import org.jblas.DoubleMatrix;
import utils.DLTree;

import java.io.*;
import java.util.ArrayList;
import java.util.Properties;

/**
 * Created by b.rychalska on 22.10.15.
 */
public class ReadInputData {

    Vocabulary vocab;


    public Dataset readData(String dataPath, String wordVectorPath, boolean readDependenciesFromFile, boolean saveDependenciesToFile, int vectorLength) {
        vocab = new Vocabulary();
        ArrayList<SentencePair> trainset = null;
        ArrayList<SentencePair> testset = null;
        try {
            trainset = readDataset(dataPath, readDependenciesFromFile, saveDependenciesToFile, "train");
            testset = readDataset(dataPath, readDependenciesFromFile, saveDependenciesToFile, "test");
        } catch (IOException e) {
            e.printStackTrace();
        }
        vocab.wordMap.put("*UNKNOWN*", vocab.wordMap.size());

        Dataset dataset = new Dataset(trainset, testset);

        try {
            readVectors(wordVectorPath, vectorLength);
        } catch (IOException e) {
            e.printStackTrace();
        }

        return dataset;
    }


    private void readVectors(String path, int vectorLength) throws IOException {
        BufferedReader vectors = null;
        try {
            vectors = new BufferedReader(new FileReader(path));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        String line = vectors.readLine();
        String[] elems = line.split(" ");
        String unknown = elems[0];
        DoubleMatrix unknownVector = new DoubleMatrix(vectorLength, 1);
        for(int i = 0; i < vectorLength; i++) {
            float n = Float.parseFloat(elems[i+1]);
            unknownVector.put(i, n);
        }

        vocab.initVectorMatrix(unknownVector, vectorLength);

        try {
            while ((line = vectors.readLine()) != null)
            {
                elems = line.split(" ");
                String word = elems[0];

                if( vocab.wordMap.containsKey(word)) {
//                    System.out.println("getting key: " + word);
//                    System.out.println("its index: " + vocab.wordMap.get(word));
                    DoubleMatrix vector = new DoubleMatrix(vectorLength, 1);
                    for(int i = 0; i < vectorLength; i++) {
                        float n = Float.parseFloat(elems[i+1]);
                        vector.put(i, n);
                    }
                    vocab.embeddings.putColumn(vocab.wordMap.get(word), vector);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    private void annotate(SentencePair pair, StanfordCoreNLP pipeline, FileWriter fw) {
        for( Sentence sent : pair.getSentences()) {
            String newString = "";
            Annotation document = new Annotation(sent.s);
            pipeline.annotate(document);
            CoreMap sentence = document.get(CoreAnnotations.SentencesAnnotation.class).get(0);

            for (CoreLabel token : sentence.get(CoreAnnotations.TokensAnnotation.class)) {
                String word = token.get(CoreAnnotations.TextAnnotation.class);
                newString += word + " ";

                if( ! vocab.wordMap.containsKey(word) ) {
                    vocab.wordMap.put(word, vocab.wordMap.size());
                }
            }
            Tree tree = sentence.get(TreeCoreAnnotations.TreeAnnotation.class);
            SemanticGraph dependencies = sentence.get(SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation.class);
            sent.s = newString;
            DLTree parseTree = dependenciesToTree(dependencies, new DLTree(), dependencies.getFirstRoot(), null);
            DLTree linearTree = linearSentenceToTree(newString);
            sent.parseTree = parseTree;
            sent.linearTree = linearTree;
            try {
                fw.write(tree.pennString() + "\n");
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private DLTree linearSentenceToTree(String sentence) {
        String[] words = sentence.split(" ");
        DLTree rootTree = new DLTree();

        DLTree parent = rootTree;
        for(String w: words) {
            DLTree nodeTree = new DLTree();
            nodeTree.setLabel(new Word("WORD"));

            DLTree wordTree = new DLTree();
            wordTree.setLabel(new Word(w));
            nodeTree.addChild(wordTree);

            parent.addChild(nodeTree);
            parent = nodeTree;
        }

//        System.out.println("linear sentence: " + rootTree);

        return (DLTree)rootTree.firstChild();
    }


    private DLTree dependenciesToTree(SemanticGraph dependencies, Tree tree, IndexedWord node, IndexedWord parent) {
        DLTree nodeTree = new DLTree();
        if( parent != null) {
            String dep = dependencies.reln(parent, node).toString();
            nodeTree.setLabel(new Word(dependencies.reln(parent, node).toString()));
        } else {
            nodeTree.setLabel(new Word("root"));
        }
        tree.addChild(nodeTree);

        DLTree wordTree = new DLTree();
        wordTree.setLabel(new Word(node.value()));
        nodeTree.addChild(wordTree);

        for(IndexedWord child : dependencies.getChildren(node)) {
            dependenciesToTree(dependencies, nodeTree, child, node);
        }

        return nodeTree;
    }

    private ArrayList<SentencePair> readDataset(String path, boolean readDeps, boolean saveDeps, String dataType) throws IOException {
        String ending = "";
        if(dataType.equals("train")) ending = "train.txt";
        if(dataType.equals("test")) ending = "test.txt";

        StanfordCoreNLP pipeline = null;

        if(saveDeps) {
            Properties props = new Properties();
            props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref");
            pipeline = new StanfordCoreNLP(props);
        }



        ArrayList<SentencePair> dataset = new ArrayList<SentencePair>();

        File folder = new File(path);
        File[] listOfFiles = folder.listFiles();

        for (File file : listOfFiles) {
            if (file.isFile()) {
                if(file.getName().endsWith(ending)) {
                    System.out.println(file.getName());
                    String label1FileName = file.getName().replace(".txt", "_labels" + 1 + ".txt");
                    String label2FileName = file.getName().replace(".txt", "_labels" + 2 + ".txt");
                    String treeFileName = file.getName().replace(".txt", "_tree.txt");

                    if(saveDeps) deleteIfExists(path + "/" + treeFileName);

                    FileWriter treeFileWriter = new FileWriter(path + "/" + treeFileName,true);
                    FileReader treeFileReader = new FileReader(path + "/" + treeFileName);
                    PennTreeReader pennTreeReader = new PennTreeReader(treeFileReader);


                    BufferedReader train = new BufferedReader(new FileReader(path + "/" + file.getName()));
                    BufferedReader labels1 = new BufferedReader(new FileReader(path + "/" + label1FileName));
                    BufferedReader labels2 = new BufferedReader(new FileReader(path + "/" + label2FileName));

                    String sentence = "";
                    String label1 = "";
                    String label2 = "";
                    while (((sentence = train.readLine()) != null) && ((label1 = labels1.readLine()) != null) && ((label2 = labels2.readLine()) != null))
                    {
                        String[] pair = sentence.split("\t");
                        float score1 = Float.parseFloat(label1);
                        float score2 = Float.parseFloat(label2);

                        SentencePair p = new SentencePair(pair[0], pair[1], score1, score2);
                        if(saveDeps) {
                            annotate(p, pipeline, treeFileWriter);
                        }
                        if(readDeps) {
                            Tree t1 = pennTreeReader.readTree();
//                            System.out.println("tree value " + t1.yieldWords());
                            Tree t2 = pennTreeReader.readTree();
                            EnglishGrammaticalStructure egs1 = new EnglishGrammaticalStructure(t1);
                            EnglishGrammaticalStructure egs2 = new EnglishGrammaticalStructure(t2);

//                            for(TypedDependency ts: egs1.typedDependencies()) {
//                                System.out.println("dep: " + ts);
//                            }

                            SemanticGraph s1 = new SemanticGraph(egs1.typedDependencies());
                            SemanticGraph s2 = new SemanticGraph(egs2.typedDependencies());
//                            System.out.println("semantic graph 1: " + s1);

                            DLTree depTree1 = dependenciesToTree(s1, new DLTree(), s1.getFirstRoot(), null);
                            DLTree depTree2 = dependenciesToTree(s2, new DLTree(), s2.getFirstRoot(), null);

                            DLTree linearTree1 = linearSentenceToTree(getWordsFromTree(t1));
                            DLTree linearTree2 = linearSentenceToTree(getWordsFromTree(t2));
//                            System.out.println("read tree1: " + depTree1.pennString());
//                            System.out.println("read tree2: " + depTree2.pennString());

                            p.s1.parseTree = depTree1;
                            p.s2.parseTree = depTree2;
                            p.s1.linearTree = linearTree1;
                            p.s2.linearTree = linearTree2;

                            p.s1.s = getWordsFromTree(t1);
                            p.s2.s = getWordsFromTree(t2);
                        }
                        dataset.add(p);
                    }
                    treeFileWriter.close();
                }
            }
        }
        return dataset;
    }

    private void deleteIfExists(String name) {
        File f = new File(name);
        if (f.exists()){
            f.delete();
        }
    }


    private String getWordsFromTree(Tree t) {
        String s = "";
        for(Word w : t.yieldWords()) {
            s += w + " ";

            if( ! vocab.wordMap.containsKey(w.word()) ) {
                vocab.wordMap.put(w.word(), vocab.wordMap.size());
            }
        }

        return s;
    }

    public Vocabulary getVocab() {
        return vocab;
    }
}
