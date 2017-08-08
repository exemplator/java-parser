class DiamondAnonymousClass {
    <T> Box<T> createBox(T content) {
        // Java 9 can infer `T` because it is a denotable type
        return new Box<>(content) { };
    }

    Box<?> createCrazyBox(Object content) {
        List<?> innerList = Arrays.asList(content);
        // we can't do the following because the inferred type is non-denotable:
        // return new Box<>(innerList) { };
        // instead we have to denote the type we want:
        return new Box<List<?>>(innerList) { };
    }
}