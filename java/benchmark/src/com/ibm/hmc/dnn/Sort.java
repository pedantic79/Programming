package com.ibm.hmc.dnn;

/**
 * Created by dnn on 6/23/17.
 */
public class Sort {
    public static void sort(int[] array) {
        sort(array, 0, array.length);
    }

    public static void sort(int[] array, int fromIndex, int toIndex) {
        if (toIndex - fromIndex < 2) {
            return;
        }

        int pivot = array[fromIndex];
        int leftPartitionLength = 0;
        int rightPartitionLength = 0;
        int index = fromIndex;

        while (index < toIndex - rightPartitionLength) {
            int current = array[index];

            if (current > pivot) {
                ++rightPartitionLength;
                int tmp = array[toIndex - rightPartitionLength];
                array[toIndex - rightPartitionLength] = current;
                array[index] = tmp;
            } else if (current < pivot) {
                int tmp = array[fromIndex + leftPartitionLength];
                array[fromIndex + leftPartitionLength] = current;
                array[index] = tmp;

                ++index;
                ++leftPartitionLength;
            } else {
                ++index;
            }
        }

        sort(array, fromIndex, fromIndex + leftPartitionLength);
        sort(array, toIndex - rightPartitionLength, toIndex);
    }


    public static void go(int[] v) {
        sort(v);
    }
}
