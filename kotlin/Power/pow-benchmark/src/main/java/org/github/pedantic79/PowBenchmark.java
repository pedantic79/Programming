package org.github.pedantic79;

/**
 * Created by dnn on 9/5/16.
 */

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
//@Warmup(iterations = 1)
//@Measurement(iterations = 2)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
//@SuppressWarnings("unused")
public class PowBenchmark {
    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(PowBenchmark.class.getSimpleName() + ".*")
                .build();

        new Runner(opt).run();
    }

    @Benchmark
    @Fork(1)
    public void testDan_1_31() {
        org.github.pedantic79.IntegerPow.fracPow(1, 31);
    }

    @Benchmark
    @Fork(1)
    public void testDan_1_30000() {
        org.github.pedantic79.IntegerPow.fracPow(1, 30000);
    }


    @Benchmark
    @Fork(1)
    public void testDan_3_3() {
        org.github.pedantic79.IntegerPow.fracPow(3, 3);
    }

    @Benchmark
    @Fork(1)
    public void testDan_3_31() {
        org.github.pedantic79.IntegerPow.fracPow(3, 31);
    }

    @Benchmark
    @Fork(1)
    public void testJosh_1_31() {
        org.github.joshsmak.Josh.powerOptimized(1, 31);
    }

    @Benchmark
    @Fork(1)
    public void testJosh_1_30000() {
        org.github.joshsmak.Josh.powerOptimized(1, 30000);
    }

    @Benchmark
    @Fork(1)
    public void testJosh_3_3() {
        org.github.joshsmak.Josh.powerOptimized(3, 3);
    }

    @Benchmark
    @Fork(1)
    public void testJosh_3_31() {
        org.github.joshsmak.Josh.powerOptimized(3, 31);
    }

}
