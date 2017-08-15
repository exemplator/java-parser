public interface InterfaceDefaults {

    default boolean evenSum(int... numbers) {
        return sum(numbers) % 2 == 0;
    }

    default <T> boolean typeArg (T numbers) {
        return sum(numbers) % 2 == 0;
    }

    default boolean oddSum(int... numbers) {
        return sum(numbers) % 2 == 1;
    }

    private int sum(int[] numbers) {
        return IntStream.of(numbers).sum();
    }

    public abstract boolean isHoliday(LocalDate date);

    /**
     * Need to get implemented as per ReportGenerator class
     * @param reportData
     * @param schema
     */
    void generateReport(String reportData, String schema);

    private static class Helper{
        static initializeMyClass(MyClass myClass, Params params){
            //do magical things in 100 lines of code to initialize myClass for example
        }
    }
}