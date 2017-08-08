class TryWithEffectivelyFinal {
    void doSomethingWith1(Connection connection) throws Exception {
        try(Connection c = connection) {
            c.doSomething();
        }
    }
    void doSomethingWith2(Connection connection) throws Exception {
        try(connection) {
            c.doSomething();
        }
    }
}