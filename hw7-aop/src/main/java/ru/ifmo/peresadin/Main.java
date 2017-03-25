package ru.ifmo.peresadin;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import ru.ifmo.peresadin.aspect.ProfileAspect;
import ru.ifmo.peresadin.test.TwoFib;

public class Main {

    public static void main(String[] args) {
        ApplicationContext ctx =
                new AnnotationConfigApplicationContext(ContextConfiguration.class);

        TwoFib twoFib = ctx.getBean(TwoFib.class);
        twoFib.compute(16);

        ProfileAspect profiler = ctx.getBean(ProfileAspect.class);
        profiler.printStatistic();
    }
}
