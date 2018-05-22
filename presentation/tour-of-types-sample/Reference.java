public class Reference {
    static class SomeBigObject {}
    static void Reference() {
        SomeBigObject o1 = new SomeBigObject();
        SomeBigObject o2 = o1;
        Foo(o1);
    }
    static void Foo(SomeBigObject o) {
        // do something
    }
}
