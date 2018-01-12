package utils;

import org.jblas.DoubleMatrix;

/**
 * Created by b.rychalska on 25.10.15.
 */
public class LogLoss {

    public static double value(DoubleMatrix p, DoubleMatrix t, int catSize) {
        double cost = 0.0;
        for(int i = 0; i < catSize; i++) {
//            cost += - (t.get(i) * Math.log(p.get(i)) + (1-t.get(i) * Math.log(1-p.get(i))));
            cost += -t.get(i)*Math.log(p.get(i));
        }
//        System.out.println("value of cost: " + cost);
        return cost;
    }


    public static DoubleMatrix derivative(DoubleMatrix p, DoubleMatrix t) {
        return p.sub(t);
    }
}
