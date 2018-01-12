package utils;

import org.jblas.DoubleMatrix;
import org.jblas.MatrixFunctions;

/**
 * Created by b.rychalska on 25.10.15.
 */
public class Softmax {
    public static DoubleMatrix value(DoubleMatrix input) {
        DoubleMatrix exp = MatrixFunctions.exp(input);
        double sum = exp.sum();
        return exp.divi(sum);
    }
}
