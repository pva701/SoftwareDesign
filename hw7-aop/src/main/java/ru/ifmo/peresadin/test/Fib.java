package ru.ifmo.peresadin.test;

import ru.ifmo.peresadin.aspect.Profile;

import java.math.BigInteger;

/**
 * @author akirakozov
 */
public class Fib {

    @Profile
    public BigInteger compute(int n) {
        if (n <= 0)
            return BigInteger.ONE;
        return compute(n - 1).add(compute(n - 2));
    }
}
