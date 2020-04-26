package ca.utoronto.utm.floatingpoint;

public class Questions {
    public static void main(String[] args) {
        example();
//        q1(); // 1 MARK
//        q2(); // 3 MARKS
//        q3(); // 5 MARKS
    }

    public static void report(String label, String bits, String interpretation, float value) {
        System.out.println(label + "=" + bits + "=" + interpretation + "=" + value);
    }

    public static void example() {
        String label, bits, interpretation;
        float value;

        float f1 = 1.27f;
        label = "f1";
        bits = "0[01111111]01000101000111101011100";
        interpretation = "+1.01000101000111101011100x2^(0)";
        value = f1;
        report(label, bits, interpretation, value);

        float f2 = (float) Math.pow(2, 11);
        label = "f2";
        bits = "0[10001010]00000000000000000000000";
        interpretation = "+1.00000000000000000000000x2^(11)";
        value = f2;
        report(label, bits, interpretation, value);

        float f3 = (f1 * f2);
        label = "f3";
        bits = "0[10001010]01000101000111101011100";
        interpretation = "+1.01000101000111101011100x2^(11)";
        value = f3;
        report(label, bits, interpretation, value);

        float f4 = 7.199f;
        label = "f4";
        bits = "0[10000001]11001100101111000110101";
        interpretation = "+1.11001100101111000110101x2^(2)";
        value = f4;
        report(label, bits, interpretation, value);

        float f5 = f3 + f4;
        label = "f5";
        bits = "0[10001010]01000110000001010001011";
        interpretation = "+1.01000110000001010001011x2^(11)";
        value = f5;
        report(label, bits, interpretation, value);

        /**
         * Output:
         * f1=0[01111111]01000101000111101011100=+1.01000101000111101011100x2^(0)=1.27
         * f2=0[10001010]00000000000000000000000=+1.00000000000000000000000x2^(11)=2048.0
         * f3=0[10001010]01000101000111101011100=+1.01000101000111101011100x2^(11)=2600.96
         * f4=0[10000001]11001100101111000110101=+1.11001100101111000110101x2^(2)=7.199
         * f5=0[10001010]01000110000001010001011=+1.01000110000001010001011x2^(11)=2608.159
         */
    }

    public static void q1() {
        String label, bits, interpretation;
        float value;

        float q1f1 = 7.35f;
        label = "q1f1";
        bits = "0[10000001]11010110011001100110011"; // FIXTHIS
        interpretation = "+1.11010110011001100110011x2^(2)"; // FIXTHIS
        value = q1f1;
        report(label, bits, interpretation, value);
    }

    public static void q2() {
        String label, bits, interpretation;
        float value;

        float q2f1 = 121.91f;
        label = "q2f1";
        bits = "0[10000101]11100111101000111101100"; // FIXTHIS
        interpretation = "+1.11100111101000111101100x2^(6)"; // FIXTHIS
        value = q2f1;
        report(label, bits, interpretation, value);

        float q2f2 = 7.3f;
        label = "q2f2";
        bits = "0[10000001]11010011001100110011010"; // FIX THIS
        interpretation = "+1.11010011001100110011010x2^(2)"; // FIXTHIS
        value = q2f2;
        report(label, bits, interpretation, value);

        float q2f3 = q2f1 + q2f2;
        label = "q2f3";
        bits = "0[10000110]00000010011010111000011"; // FIXTHIS
        interpretation = "+1.00000010011010111000011x2^(7)"; // FIXTHIS
        value = q2f3;
        report(label, bits, interpretation, value);
    }

    public static void q3() {
        /**
         * As in the lecture notes example, we are going to find a and b such that
         * a,b>0 and (a+b)-a == 0 and (a-a)+b==b
         */
        String label, bits, interpretation;
        float value;

        float a = 1.76f * (float) Math.pow(2, 33); // DONT CHANGE THIS
        label = "a";
        bits = "0[01111111]10000011000001110011001"; // FIXTHIS
        interpretation = "+1.10000011000001110011001x2^(0)"; // FIXTHIS
        value = a;
        report(label, bits, interpretation, value);

        float b = 7.3f; // MAKE THIS AS LARGE AS POSSIBLE
        label = "b";
        bits = "0[10001000]00000000000000000000000"; // FIXTHIS
        interpretation = "+1.00000000000000000000000x2^(9)"; // FIXTHIS
        value = b;
        report(label, bits, interpretation, value);
    }
}
