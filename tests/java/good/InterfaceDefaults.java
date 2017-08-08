public interface InterfaceDefaults {

    default boolean evenSum(int... numbers) {
        return sum(numbers) % 2 == 0;
    }

    default boolean oddSum(int... numbers) {
        return sum(numbers) % 2 == 1;
    }

    private int sum(int[] numbers) {
        return IntStream.of(numbers).sum();
    }

}