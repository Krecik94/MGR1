package utils;

import org.jblas.DoubleMatrix;

/**
 * Created by b.rychalska on 25.10.15.
 */
public class SquaredError {

    public static double value(DoubleMatrix p, DoubleMatrix t) {
        return 0.5 * Math.pow(p.distance2(t), 2);
    }


    public static DoubleMatrix derivative(DoubleMatrix p, DoubleMatrix t) {
        return p.sub(t);
    }
}
