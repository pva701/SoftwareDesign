package ru.ifmo.peresadin.aspect;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;

import java.util.*;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 24.03.17
 */
@Aspect
public class ProfileAspect {
    public static class MethodInfo {
        private int calls;
        private long time;

        public MethodInfo(int calls, long time) {
            this.calls = calls;
            this.time = time;
        }

        public int getCalls() {
            return calls;
        }

        public long getTime() {
            return time;
        }

        public double getAverage() {
            return time * 1.0 / calls;
        }
    }

    Map<String, MethodInfo> methods = new TreeMap<>();

    @Around("@annotation(ru.ifmo.peresadin.aspect.Profile)")
    public Object logExecutionTime(ProceedingJoinPoint joinPoint) throws Throwable {
        long startNs = System.nanoTime();
        String method = joinPoint.getSignature().getDeclaringTypeName() + "." + joinPoint.getSignature().getName();

        Object result = joinPoint.proceed(joinPoint.getArgs());
        long totalTime = System.nanoTime() - startNs;

        MethodInfo minfo = methods.get(method);

        if (minfo == null) {
            methods.put(method, new MethodInfo(1, totalTime));
        } else {
            minfo.calls++;
            minfo.time += totalTime;
            methods.put(method, minfo);
        }
        return result;
    }

    public void printStatistic() {
        int SPACES = 2;
        String[] path = new String[0];
        for (Map.Entry<String, ProfileAspect.MethodInfo> entry: methods.entrySet()) {
            int l = 0;
            String[] cur = entry.getKey().split("\\.");

            for (; l < path.length && path[l].equals(cur[l]); ++l);
            StringBuilder sp = new StringBuilder();
            for (int j = 0; j < SPACES * l; ++j) sp.append(' ');

            for (; l < cur.length; l++) {
                System.out.print(sp.toString());
                if (l + 1 != cur.length) System.out.println(cur[l]);
                else {
                    System.out.println(
                            cur[l] + ": " +
                                    ", total time: " + entry.getValue().getTime() / 1000000.0 + " ms" +
                                    ", total calls: " + entry.getValue().getCalls() +
                                    ", average time: " + entry.getValue().getAverage() / 1000000.0 + " ms");
                }
                for (int j = 0; j < SPACES; ++j)
                    sp.append(' ');
            }
            path = cur;
        }
    }
}
