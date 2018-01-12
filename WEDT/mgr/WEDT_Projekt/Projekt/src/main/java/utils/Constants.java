package utils;

/**
 * Created by b.rychalska on 30.10.15.
 */
public class Constants {
    public static int VANILLA = 0;
    public static int TEST = 1;
    public static int GRU = 2;
    public static int VANILLA_MULTI_H = 3;

    public static int DEPENDENCY_TREE = 0;
    public static int LINEAR = 1;

    public static int getValRNN(String name) {
        if(name.equals("VANILLA")) return VANILLA;
        if(name.equals("VANILLA_MULTI_H")) return VANILLA_MULTI_H;
        //others are for tests only
        else return -1;
    }

    public static int getValChain(String name) {
        if(name.equals("DEPENDENCY_TREE")) return DEPENDENCY_TREE;
        if(name.equals("LINEAR")) return LINEAR;
        //others are for tests only
        else return -1;
    }
}
