package rnn;

import org.jblas.DoubleMatrix;
import utils.Constants;

import java.util.HashMap;

/**
 * Created by b.rychalska on 25.10.15.
 */
public class Theta {
    String[] keySet = { "W", "U", "softmaxW1", "softmaxW2"};
    String[] bkeySet = { "W", "U", "softmaxW1", "softmaxW2"};

    HashMap<String, DoubleMatrix> W;
    HashMap<String, DoubleMatrix> b;

    //TODO Remove public visibility
    public double[] Theta;

    protected double r1;
    int[] Wbegins, Wends, bbegins, bends;

    int hiddenSize, catSize;
    private static final long serialVersionUID = 752647392162776147L;

    private void initKeyset(int rnnArchitecture) {
        if (rnnArchitecture == Constants.VANILLA) {
            keySet = new String[]{"W", "U", "softmaxW11", "softmaxW12", "softmaxW2"};
            bkeySet = new String[]{ "W", "softmaxW1"};
        }
        else if(rnnArchitecture == Constants.TEST ) {
            keySet = new String[]{"Wo", "W", "U", "softmaxW11", "softmaxW12", "softmaxW2"};
        }
        else if(rnnArchitecture == Constants.GRU ) {
            keySet = new String[]{"Wz", "Uz", "Wr", "Ur", "W", "U", "softmaxW11", "softmaxW12", "softmaxW2"};
            bkeySet = new String[]{"Uz", "Ur", "U", "softmaxW1"};
        }
        else if(rnnArchitecture == Constants.VANILLA_MULTI_H ) {
            keySet = new String[]{"W1", "U1", "W", "U", "softmaxW11", "softmaxW12", "softmaxW2"};
            bkeySet = new String[]{ "W1", "W", "softmaxW1"};
        }
    }

    public int getThetaSize() {
        int s = 0;
        for(String key: keySet) {
            s += W.get(key).rows * W.get(key).columns;
        }
        for(String key: bkeySet) {
            s += b.get(key).rows * b.get(key).columns;
        }
        return s;
    }

    public void increaseMatrix(String s, int i, double val) {
        DoubleMatrix m = W.get(s);
        m.put(i, (m.get(i)+val));
        W.put(s, m);
    }

    public void increaseBMatrix(String s, int i, double val) {
        DoubleMatrix m = b.get(s);
        m.put(i, (m.get(i)+val));
        b.put(s, m);
    }

    /** Dummy constructor, because it is required by Java for subclass **/
    public Theta(int rnnArchitecture) {
        initKeyset(rnnArchitecture);
        W = new HashMap<String, DoubleMatrix>();
        b = new HashMap<String, DoubleMatrix>();
        Wbegins = new int[keySet.length + bkeySet.length];
        Wends = new int[keySet.length + bkeySet.length];
    }

    public Theta(int hiddenSize, int catSize, int rnnArchitecture)
    {
        this(rnnArchitecture);
        this.hiddenSize = hiddenSize;
        this.catSize = catSize;
        r1 = Math.sqrt(6) / Math.sqrt(hiddenSize+hiddenSize+1);

        InitializeMatrices();

        Theta = new double[ getThetaSize() ];
        flatten(Theta);
    }

    /**
     * Reconstruct the Theta from theta vector and populate all the W matrices.
     */
    public Theta(double[] iTheta, int hiddenSize, int catSize, int rnnArchitecture)
    {
        this(rnnArchitecture);
        this.hiddenSize = hiddenSize;
        this.catSize = catSize;
        this.fixIndices();
        DoubleMatrix Full = new DoubleMatrix(iTheta);

        int i = 0;
        for(String key : keySet) {
            if(key.equals("softmaxW2")) W.put(key, Full.getRowRange(Wbegins[i], Wends[i]+1, 0).reshape(catSize, hiddenSize));
            else W.put(key, Full.getRowRange(Wbegins[i], Wends[i]+1, 0).reshape(hiddenSize, hiddenSize));
            i++;
        }
        for(String key : bkeySet) {
            if(key.equals("softmaxW2")) b.put(key, Full.getRowRange(Wbegins[i], Wends[i]+1, 0).reshape(catSize, 1));
            else b.put(key, Full.getRowRange(Wbegins[i], Wends[i]+1, 0).reshape(hiddenSize, 1));
            i++;
        }

        Theta = new double[ getThetaSize() ];
        flatten(Theta);
    }

    public Theta(Theta orig, int rnnArchitecture)
    {
        this(rnnArchitecture);
        hiddenSize = orig.hiddenSize;
        catSize = orig.catSize;

        Wbegins = orig.Wbegins.clone();
        Wends = orig.Wends.clone();
        r1 = orig.r1;

        Theta = orig.Theta.clone();
        for(String key: keySet) {
            W.put(key, orig.W.get(key).dup());
        }
        for(String key: bkeySet) {
            b.put(key, orig.b.get(key).dup());
        }
    }

    public Theta(HashMap<String, DoubleMatrix> W_grad, HashMap<String, DoubleMatrix> b_grad, int rnnArchitecture) {
        this(rnnArchitecture);
        for(String key : keySet) {
            W.put(key, W_grad.get(key));
        }
        for(String key : bkeySet) {
            b.put(key, b_grad.get(key));
        }

        hiddenSize = W.get(keySet[0]).rows;
        catSize = W.get("softmaxW2").rows;
        Theta = new double[ getThetaSize() ];
        this.flatten(Theta);
    }


    protected void InitializeMatrices()
    {
        Wbegins = new int[keySet.length + bkeySet.length];
        Wends = new int[keySet.length * bkeySet.length];
        bbegins = new int[keySet.length];
        bends = new int[keySet.length];

        for(String key: keySet) {
            if(key.equals("softmaxW2")) W.put(key, (DoubleMatrix.rand(catSize, hiddenSize).muli(2 * r1)).subi(r1));
            else W.put(key, (DoubleMatrix.rand(hiddenSize, hiddenSize).muli(2 * r1)).subi(r1));
        }
        for(String key: bkeySet) {
            if(key.equals("softmaxW2")) b.put(key, (DoubleMatrix.zeros(catSize, 1)));
            else b.put(key, (DoubleMatrix.zeros(hiddenSize, 1)));
        }
    }

    protected void flatten(double[] Theta)
    {
        this.fixIndices();

        int i =0;
        for(String key: keySet) {
            DoubleMatrix m = W.get(key);
            if(key.equals("softmaxW2")) System.arraycopy(m.toArray(), 0, Theta, Wbegins[i], catSize * hiddenSize);
            else System.arraycopy(m.toArray(), 0, Theta, Wbegins[i], hiddenSize * hiddenSize);
            i++;
        }
        for(String key: bkeySet) {
            DoubleMatrix m = b.get(key);
            if(key.equals("softmaxW2")) System.arraycopy(m.toArray(), 0, Theta, Wbegins[i], catSize * 1);
            else System.arraycopy(m.toArray(), 0, Theta, Wbegins[i], hiddenSize * 1);
            i++;
        }
    }

    protected void fixIndices()
    {
        Wbegins[0] = 0;
        Wends[0] = hiddenSize * hiddenSize - 1;

        int i = 0;
        for(String key: keySet) {
            if(i == 0) {
                Wbegins[i] = 0;
                Wends[i] = hiddenSize * hiddenSize - 1;
                i++;
                continue;
            }
            if(key.equals("softmaxW2")) {
                Wbegins[i] = Wends[i-1] + 1;
                Wends[i] = Wbegins[i] + catSize * hiddenSize - 1;
            }
            else {
                Wbegins[i] = Wends[i-1] + 1;
                Wends[i] = Wbegins[i] + hiddenSize * hiddenSize - 1;
            }
            i++;
        }
        for(String key: bkeySet) {
            if(key.equals("softmaxW2")) {
                Wbegins[i] = Wends[i-1] + 1;
                Wends[i] = Wbegins[i] + catSize * 1 - 1;
            }
            else {
                Wbegins[i] = Wends[i-1] + 1;
                Wends[i] = Wbegins[i] + hiddenSize * 1 - 1;
            }
            i++;
        }
    }


    public void copyRelevant(Theta theta) {
        for(String key: theta.keySet) {
            this.W.put(key, theta.getW(key));
        }

        for(String key: theta.bkeySet) {
            this.b.put(key, theta.getb(key));
        }
    }

    public DoubleMatrix getW(String key) {
        return W.get(key);
    }
    public DoubleMatrix getb(String key) {
        return b.get(key);
    }


    public void substractGradient(Theta gradient, double step) {
        for(String key: gradient.keySet) {
            this.W.put(key, this.getW(key).sub(gradient.getW(key).mul(step)));
        }

        for(String key: gradient.bkeySet) {
            this.b.put(key, this.getb(key).sub(gradient.getb(key).mul(step)));
        }
    }
}
