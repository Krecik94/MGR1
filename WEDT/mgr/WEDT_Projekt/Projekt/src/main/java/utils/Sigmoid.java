package utils;

import org.jblas.DoubleMatrix;
import org.jblas.MatrixFunctions;

/**
 * Created by b.rychalska on 24.10.15.
 */
public class Sigmoid {

//    public static DoubleMatrix value(DoubleMatrix x) {
//        DoubleMatrix ones = DoubleMatrix.ones(x.rows, x.columns);
//        return ones.div(ones.add(MatrixFunctions.exp(x.neg())));
//    }

//    public static DoubleMatrix derivative(DoubleMatrix x) {
////        DoubleMatrixUtils.printSize(x);
//        DoubleMatrix y = x.dup();
//
//        return y.mul(DoubleMatrix.ones(x.rows, 1).sub(y));
//    }

    public static DoubleMatrix value(DoubleMatrix M) {
        DoubleMatrix Denom = (MatrixFunctions.exp(M.mul(-1))).addi(1);
        return Denom.rdivi(1);
    }

    public static DoubleMatrix derivative(DoubleMatrix M) {
        //DoubleMatrix M = value(X);
        return M.mul( (M.mul(-1)).addi(1) );
    }


}
