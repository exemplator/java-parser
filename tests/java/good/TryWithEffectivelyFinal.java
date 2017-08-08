class TryWithEffectivelyFinal {
    void doSomethingWith(Connection connection) throws Exception {
        try(connection) {
            c.doSomething();
        }
    }
}