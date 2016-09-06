/**
 * Created by dnn on 9/5/16.
 */
package org.github.joshsmak;

public class Josh {
    public static double powerOptimized(int x, int y) {
        if (y == 0) return 1.0;
        boolean flip = false;
        int absY = y;
        double pow = 1.0;

        if (y < 0) {
            flip = true;
            absY = -y;
        }
        if (absY == 1) pow = x;
        else {
            long[] exponents = new long[31];
            double[] values = new double[31];
            int count = 0;
            int lastIndex = 0;
            exponents[count] = 1;
            values[count++] = x;
            while (exponents[count - 1] << 1 <= absY) {
                exponents[count] = exponents[count -1] << 1;
                values[count] =  values[count -1] * values[count - 1];
                lastIndex = count;
                if (Double.isInfinite(values[count])) {
                    if (flip) return 0.0;
                    else {
                        if (x < 0 && y % 2 != 0) return Double.NEGATIVE_INFINITY;
                        return Double.POSITIVE_INFINITY;
                    }
                }
                count++;

            }
            long myY = absY - exponents[lastIndex];
            pow = values[lastIndex--];
            while (myY != 0) {
                while(exponents[lastIndex] > myY) lastIndex--;
                myY = myY - exponents[lastIndex];
                pow = pow * values[lastIndex];
            }
            //System.out.println(Arrays.toString(exponents));
            //System.out.println(Arrays.toString(values));
        }
        if (flip) {
            pow = 1.0 / pow;
        }
        return pow;
    }
}
