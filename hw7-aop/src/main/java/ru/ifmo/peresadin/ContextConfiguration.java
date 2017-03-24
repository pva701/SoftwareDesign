package ru.ifmo.peresadin;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import ru.ifmo.peresadin.aspect.*;
import ru.ifmo.peresadin.test.Fib;
import ru.ifmo.peresadin.test.TwoFib;

/**
 * @author akirakozov
 */
@Configuration
@EnableAspectJAutoProxy
public class ContextConfiguration {
    @Bean
    public Fib fibBean() {
        return new Fib();
    }

    @Bean
    public TwoFib twoFibBean() {
        return new TwoFib();
    }

    @Bean
    public ProfileAspect profileAspect() {
        return new ProfileAspect();
    }
}
