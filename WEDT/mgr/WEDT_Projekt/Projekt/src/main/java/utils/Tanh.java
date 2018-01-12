package utils;

import org.jblas.DoubleMatrix;
import org.jblas.MatrixFunctions;

/**
 * Created by b.rychalska on 26.10.15.
 */
public class Tanh {

    public static DoubleMatrix value(DoubleMatrix M) {
        return MatrixFunctions.tanh(M);
    }

    /**
     * @param M input double matrix
     * @return tanh_prime = (1-M.^2);
     */
    public static DoubleMatrix derivative(DoubleMatrix M) {
        DoubleMatrix Squared = M.mul(M);
        return (Squared.muli(-1)).addi(1);
    }

}
