package hackrf;

import com.sun.jna.CallbackThreadInitializer;
import com.sun.jna.Native;
import com.sun.jna.NativeLibrary;
import com.sun.jna.Platform;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.FloatByReference;

public class HackRFSweepNativeBridge {
    public static final String JNA_LIBRARY_NAME = "hackrf-transfer";

    static {
        final NativeLibrary JNA_NATIVE_LIB;
        /**
         * to make sure unpacked jnidispatch.dll is properly loaded
         * jnidispatch.dll is used directly instead of JNA bundled jar, because it is much faster to load
         */
        String pathPrefix = "./" + Platform.RESOURCE_PREFIX + "/";
//        String pathPrefix = "/usr/local/bin/";
//        String pathPrefix = "";
        System.setProperty("jna.boot.library.path", pathPrefix);
        System.setProperty("jna.nosys", "true");
//		Native.DEBUG_JNA_LOAD	= true;
//		Native.DEBUG_LOAD	= true;

        NativeLibrary.addSearchPath(JNA_LIBRARY_NAME, pathPrefix);
        try {
            JNA_NATIVE_LIB = NativeLibrary.getInstance(JNA_LIBRARY_NAME);
            Native.register(HackrfSweepLibrary.class, JNA_NATIVE_LIB);
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    public static synchronized void start(HackRFSweepDataCallback dataCallback, int centerFreq, int sample_rate_hz, int fft_size,
                                          int lna_gain, int vga_gain) {
        HackrfSweepLibrary.hackrf_sweep_lib_start__fft_power_callback_callback callback = new HackrfSweepLibrary.hackrf_sweep_lib_start__fft_power_callback_callback() {
            @Override
            public void apply(int bins, DoubleByReference freqStart, FloatByReference powerdBm) {
                double[] freqStartArr = bins == 0 ? null : freqStart.getPointer().getDoubleArray(0, bins);
                float[] powerArr = bins == 0 ? null : powerdBm.getPointer().getFloatArray(0, bins);
//                double[] freqStartArr = new double[100];
//                float[] powerArr = new float[100];
                dataCallback.newSpectrumData(freqStartArr, powerArr);
            }
        };
        Native.setCallbackThreadInitializer(callback, new CallbackThreadInitializer(true));

        HackrfSweepLibrary.hackrf_lib_start(callback, centerFreq, sample_rate_hz, fft_size, lna_gain, vga_gain);
    }

    public static void stop() {
        HackrfSweepLibrary.hackrf_lib_stop();
    }
}
