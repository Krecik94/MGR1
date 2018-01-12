package utils;

import org.jblas.DoubleMatrix;

/**
 * Created by b.rychalska on 27.10.15.
 */
public class DoubleMatrixUtils {

    public static double SquaredNorm(DoubleMatrix inp)
    {
        return (inp.mul(inp)).sum();
    }

    public static void printSize(DoubleMatrix m) {
        System.out.println("size of matrix: " + m.rows + "x" + m.columns);
    }

    public static DoubleMatrix diag(DoubleMatrix x) {
        if (x.isScalar()) return x.dup();
        else  {
            int vectorLength = x.rows ;
            DoubleMatrix ret = new DoubleMatrix(vectorLength, 1);
            for (int i = 0; i < vectorLength; i++) {
                ret.put(i, x.get(i, i));
            }
            return ret;
        }
    }
}
