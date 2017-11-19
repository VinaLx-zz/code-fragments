public class NotSoWeak {
    static public void Test() {
        double d = 123.321;
        int i2 = d; // error
        int i = (int)d; // ok
    }
}