import java.util.*;

class NonGeneric {

    Collection<Number> myNumbers(){return null;}
}
abstract class RawMembers<T> extends NonGeneric implements Collection<String> {
    static Collection<NonGeneric> cng = new ArrayList<NonGeneric>();

    public static void main(String[] args) {
                RawMembers rw = null;
                Collection<Number> cn = rw.myNumbers(); // ok
                Iterator<String> is = rw.iterator(); // unchecked warning
                Collection<NonGeneric> cnn = rw.cng; // ok - static member
    }
}