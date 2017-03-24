package ru.ifmo.peresadin;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import ru.ifmo.peresadin.aspect.ProfileAspect;
import ru.ifmo.peresadin.test.Fib;
import ru.ifmo.peresadin.test.TwoFib;

import java.util.Map;

/**
 * @author akirakozov
 */
public class Main {

    public static void main(String[] args) {
        ApplicationContext ctx =
                new AnnotationConfigApplicationContext(ContextConfiguration.class);

        Fib fib = ctx.getBean(Fib.class);
        TwoFib twoFib = ctx.getBean(TwoFib.class);
        //fib.compute(10);
        twoFib.compute(20);

        ProfileAspect profiler = ctx.getBean(ProfileAspect.class);
        for (Map.Entry<String, ProfileAspect.MethodInfo> entry: profiler.getStatistic().entrySet())
            System.out.println(
                    "Method: " + entry.getKey() +
                    ", total time: " + entry.getValue().getTime() / 1000000.0 + " ms" +
                    ", total calls: " + entry.getValue().getCalls() +
                    ", average time: " + entry.getValue().getAverage() / 1000000.0 + " ms");
    }
}
