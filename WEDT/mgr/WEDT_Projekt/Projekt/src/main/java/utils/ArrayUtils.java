package utils;

/**
 * Created by b.rychalska on 27.10.15.
 */
public class ArrayUtils {

    public static void scale(double[] x, double s) {
        if (s== 1.0) return;
        for (int i = 0; i < x.length; i++) {
            x[i] *= s;
        }
    }

    public static double[] add(double[] x, double[] y) {
        if (x.length != y.length) throw new RuntimeException("diff lengths: " + x.length + " " + y.length);
        double[] result = new double[x.length];
        for (int i = 0; i < x.length; i++) {
            result[i] = x[i] + y[i];
        }
        return result;
    }

}
