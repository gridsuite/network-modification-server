package org.gridsuite.modification.server;

import javax.sql.DataSource;

import net.ttddyy.dsproxy.listener.ChainListener;
import net.ttddyy.dsproxy.listener.DataSourceQueryCountListener;
import net.ttddyy.dsproxy.support.ProxyDataSourceBuilder;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.stereotype.Component;

@Component
// Decorator datasource bug
// See https://vladmihalcea.com/how-to-detect-the-n-plus-one-query-problem-during-testing
public class DatasourceProxyBeanPostProcessor implements BeanPostProcessor {
    @Override
    public Object postProcessAfterInitialization(final Object bean, final String beanName) throws BeansException {
        if (bean instanceof DataSource) {
            ChainListener listener = new ChainListener();
            listener.addListener(new DataSourceQueryCountListener());
            return ProxyDataSourceBuilder
                    .create((DataSource) bean)
                    .listener(listener)
                    .build();
        }
        return bean;
    }
}
