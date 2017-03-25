package ru.ifmo.peresadin.test;

import java.math.BigInteger;

public class Fib {
    public BigInteger compute(int n) {
        if (n <= 0)
            return BigInteger.ONE;
        return compute(n - 1).add(compute(n - 2));
    }
}
