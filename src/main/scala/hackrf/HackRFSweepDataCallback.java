package hackrf;

public interface HackRFSweepDataCallback {
    /**
     * Called by native code when new spectrum data is available
     *
     * @param frequencyStart array of fft bin's start frequencies, null if no data
     * @param signalPowerdBm array of fft bin's power in dB, null if no data
     * @param sweepDone
     */
    void newSpectrumData(double[] frequencyStart, double[] signalPowerdBm, boolean sweepDone);
}
