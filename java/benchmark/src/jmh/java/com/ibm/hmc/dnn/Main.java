package com.ibm.hmc.dnn;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

import static java.util.Arrays.sort;

@BenchmarkMode(Mode.Throughput)
//@Warmup(iterations = 1)
//@Measurement(iterations = 2)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@SuppressWarnings("unused")
@Fork(1)
public class Main {
    @State(Scope.Benchmark)
    public static class ArrayContainer {
        @Param({"10", "100"})
        private int length;

        private int[] array;
        private int[] arrayToSort;

        @Setup(Level.Iteration)
        public void setUp() {
            ThreadLocalRandom random = ThreadLocalRandom.current();
            array = random.ints(length).toArray();
        }

        @Setup(Level.Invocation)
        public void cloneArray() {
            arrayToSort = array.clone();
        }

    }

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(Main.class.getSimpleName() + ".*")
                .build();

        new Runner(opt).run();
    }

    @Benchmark
    public void benchmarkCustom(Blackhole blackhole, ArrayContainer c) {
        Sort.go(c.arrayToSort);
        blackhole.consume(c.arrayToSort);
    }


    @Benchmark
    public void benchmarkBuiltIn(Blackhole blackhole, ArrayContainer c) {
        sort(c.arrayToSort);
        blackhole.consume(c.arrayToSort);
    }
}
