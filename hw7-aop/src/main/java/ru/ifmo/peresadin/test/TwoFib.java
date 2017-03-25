package ru.ifmo.peresadin.test;

import org.springframework.beans.factory.annotation.Autowired;


import java.math.BigInteger;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 24.03.17
 */
public class TwoFib {
    @Autowired
    private Fib fib;

    public BigInteger compute(int n) {
        BigInteger res = BigInteger.ZERO;

        for (int i = n; i < 2 * n; ++i)
            res = res.add(fib.compute(i));
        return res;
    }
}
