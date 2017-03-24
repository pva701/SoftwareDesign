package ru.ifmo.peresadin.aspect;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;

import java.util.HashMap;
import java.util.Map;

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

    Map<String, MethodInfo> methods = new HashMap<>();

    @Around("@annotation(ru.ifmo.peresadin.aspect.Profile)")
    public Object logExecutionTime(ProceedingJoinPoint joinPoint) throws Throwable {
        long startNs = System.nanoTime();
        String method = joinPoint.getSignature().toShortString();

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

    public Map<String, MethodInfo> getStatistic() {
        return methods;
    }
}
