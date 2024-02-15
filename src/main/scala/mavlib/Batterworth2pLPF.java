package mavlib;

/**
 * 2-nd order Batterworth low pass filter.
 */
public class Batterworth2pLPF {
    private double a1 = 0.0f;
    private double a2 = 0.0f;
    private double b0 = 0.0f;
    private double b1 = 0.0f;
    private double b2 = 0.0f;
    private final double[] elements = new double[2];    // buffered elements
    protected double cutoffFreqFactor;

    public void setCutoffFreqFactor(double cutoffFreqFactor) {
        if (this.cutoffFreqFactor != cutoffFreqFactor) {
            this.cutoffFreqFactor = cutoffFreqFactor;
            double r = (double) Math.tan(Math.PI * this.cutoffFreqFactor);
            double c = (double)(1.0 + 2.0 * Math.cos(Math.PI / 4.0) * r + r * r);
            b0 = r * r / c;
            b1 = (double)(2.0 * b0);
            b2 = b0;
            a1 = (double)(2.0 * (r * r - 1.0) / c);
            a2 = (double)((1.0 - 2.0 * Math.cos(Math.PI / 4.0) * r + r * r) / c);
            reset(0.0f);
        }
    }

    /**
     * Add a new raw value to the filter
     *
     * @return retrieve the filtered result
     */
    public double apply(double sample) {
        if (cutoffFreqFactor <= 0.0) {
            return sample;
        }

        double element_0 = sample - elements[0] * a1 - elements[1] * a2;
//        System.out.print(element_0 + " " + elements[0] + " " + elements[1] + "\n");

        if (Double.isNaN(elements[1])) {
            System.out.println("sdgfasdf");
        }
        if (Double.isInfinite(elements[1])) {
            System.out.println("sdgfasdf");
        }
        if (Double.isNaN(elements[0])) {
            System.out.println("sdgfasdf");
        }
        if (Double.isInfinite(elements[0])) {
            System.out.println("sdgfasdf");
        }
        if (Double.isNaN(element_0)) {
            element_0 = sample;
        }
        if (Double.isInfinite(element_0)) {
            element_0 = sample;
        }

        double output = element_0 * b0 + elements[0] * b1 + elements[1] * b2;

        elements[1] = elements[0];
        elements[0] = element_0;

        return output;
    }

    /**
     * Reset the filter state to this value
     */
    public double reset(double sample) {
        double v = sample / (b0 + b1 + b2);
        elements[0] = v;
        elements[1] = v;
        return apply(sample);
    }
}