import java.util.Arrays;

public class QuickSort {

    public static void main(String[] args) {
        int input = Integer.parseInt(args[0]);

        int[] intArray = getArray(input);

        System.out.println(Arrays.toString(intArray));
        partitionArray(intArray, 0, intArray.length-1);
        System.out.println(Arrays.toString(intArray));
    }

    private static int[] getArray(int input) {
        int length = String.valueOf(input).length();
        int[] result = new int[length];
        int pos = length-1;
        while(input > 0) {
            int lastDigit = input % 10;
            result[pos] = lastDigit;

            pos--;
            input /= 10;
        }
        return result;
    }

//    private static int[] partitionArray(int[] inputArray, int partitionIndex, int endIndex) {
//        int pivot = inputArray[0];
//        while (endIndex < inputArray.length) {
//            if (inputArray[endIndex] < pivot) {
//                swapElements(inputArray, partitionIndex, endIndex);
//                partitionIndex++;
//            } 
//            endIndex++;
//        }
//        swapPivotIntoPlace(inputArray, partitionIndex);
//        return inputArray;
//    }

    private static void partitionArray(int[] inputArray, int min, int max) {
        partitionArray(inputArray, min+1, min+1, min, max);
    }

    private static void partitionArray(int[] inputArray, int partitionIndex, int endIndex, int min, int max)
    {
        if (max - min > 0) {
            int pivot = inputArray[min];
            while (endIndex <= max) {
                if (inputArray[endIndex] < pivot) {
                    swapElements(inputArray, partitionIndex, endIndex);
                    partitionIndex++;
                }
                endIndex++;
            }
            System.out.println(String.valueOf(partitionIndex));
            swapElements(inputArray, min, partitionIndex-1);
            partitionArray(inputArray, min, partitionIndex-2);
            partitionArray(inputArray, partitionIndex, max);
        }
    }

    private static void swapElements(int[] array, int i1, int i2) {
        int temp = array[i1];
        array[i1] = array[i2];
        array[i2] = temp;
    }

//    private static void swapPivotIntoPlace(int[] array, int pivotPosition) {
//        swapElements(array, 0, pivotPosition-1);
//    }
}
