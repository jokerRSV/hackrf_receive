package mavlib;

/**
 * 2-nd order Batterworth low pass filter.
 */
public class Batterworth2pLPF {
    private float a1 = 0.0f;
    private float a2 = 0.0f;
    private float b0 = 0.0f;
    private float b1 = 0.0f;
    private float b2 = 0.0f;
    private final float[] elements = new float[2];    // buffered elements
    protected float cutoffFreqFactor;

    public void setCutoffFreqFactor(float cutoffFreqFactor) {
        if (this.cutoffFreqFactor != cutoffFreqFactor) {
            this.cutoffFreqFactor = cutoffFreqFactor;
            float r = (float) Math.tan(Math.PI * this.cutoffFreqFactor);
            float c = (float)(1.0 + 2.0 * Math.cos(Math.PI / 4.0) * r + r * r);
            b0 = r * r / c;
            b1 = (float)(2.0 * b0);
            b2 = b0;
            a1 = (float)(2.0 * (r * r - 1.0) / c);
            a2 = (float)((1.0 - 2.0 * Math.cos(Math.PI / 4.0) * r + r * r) / c);
            reset(0.0f);
        }
    }

    /**
     * Add a new raw value to the filter
     *
     * @return retrieve the filtered result
     */
    public float apply(float sample) {
        if (cutoffFreqFactor <= 0.0) {
            return sample;
        }

        float element_0 = sample - elements[0] * a1 - elements[1] * a2;
//        System.out.print(element_0 + " " + elements[0] + " " + elements[1] + "\n");

        if (Float.isNaN(elements[1])) {
            System.out.println("sdgfasdf");
        }
        if (Float.isInfinite(elements[1])) {
            System.out.println("sdgfasdf");
        }
        if (Float.isNaN(elements[0])) {
            System.out.println("sdgfasdf");
        }
        if (Float.isInfinite(elements[0])) {
            System.out.println("sdgfasdf");
        }
        if (Float.isNaN(element_0)) {
            element_0 = sample;
        }
        if (Float.isInfinite(element_0)) {
            element_0 = sample;
        }

        float output = element_0 * b0 + elements[0] * b1 + elements[1] * b2;

        elements[1] = elements[0];
        elements[0] = element_0;

        return output;
    }

    /**
     * Reset the filter state to this value
     */
    public float reset(float sample) {
        float v = sample / (b0 + b1 + b2);
        elements[0] = v;
        elements[1] = v;
        return apply(sample);
    }
}