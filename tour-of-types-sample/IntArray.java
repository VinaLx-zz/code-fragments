public interface IntArray {
    int get(int index);
    int set(int index, int n);
    int length();
    int[] toArray();
    // ..
}

class Util {
static boolean Has(IntArray arr, int n) {
    for (int i = 0; i < arr.length(); ++i) {
        if (arr.get(i) == n) return true;
    }
    return false;
}
}
