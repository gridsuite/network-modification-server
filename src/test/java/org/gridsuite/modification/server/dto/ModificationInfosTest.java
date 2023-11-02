package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.ReporterModel;
import org.assertj.core.api.Condition;
import org.assertj.core.api.SoftAssertions;
import org.assertj.core.api.WithAssertions;
import org.assertj.core.api.WithAssumptions;
import org.assertj.core.api.junit.jupiter.SoftAssertionsExtension;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationApplication;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.NetworkModificationException.Type;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.junit.jupiter.api.DisplayNameGeneration;
import org.junit.jupiter.api.DisplayNameGenerator;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.SimpleBeanDefinitionRegistry;
import org.springframework.context.annotation.ClassPathBeanDefinitionScanner;
import org.springframework.core.type.filter.AssignableTypeFilter;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

@DisplayNameGeneration(DisplayNameGenerator.Simple.class)
@ExtendWith(SoftAssertionsExtension.class)
@Tag("UnitTest")
class ModificationInfosTest implements WithAssertions, WithAssumptions {
    @Test
    void testGetTypeNullWhenNoAnnotation() {
        final ModificationInfos modification = new ModificationInfos() { };
        assertThat(modification.getType()).as("modification type").isNull();
    }

    @Test
    void testGetTypeValueWhenAnnotation() {
        final ModificationInfos modification = new ModificationWithAnnotations();
        assertThat(modification.getType()).as("modification type").isEqualTo(ModificationType.VSC_CREATION);
    }

    @Test
    void testGetErrorTypeNullWhenNoAnnotation() {
        final ModificationInfos modification = new ModificationInfos() { };
        assertThat(modification.getErrorType()).as("modification error type").isNull();
    }

    @Test
    void testGetErrorTypeValueWhenAnnotation() {
        final ModificationInfos modification = new ModificationWithAnnotations();
        assertThat(modification.getErrorType()).as("modification error type").isEqualTo(Type.CREATE_VSC_ERROR);
    }

    @ModificationErrorTypeName("CREATE_VSC_ERROR")
    @JsonTypeName("VSC_CREATION")
    private static class ModificationWithAnnotations extends ModificationInfos {
        // ...
    }

    private static List<Arguments> provideModificationInfosImplementationsWithClasses() throws ClassNotFoundException {
        final BeanDefinitionRegistry bdr = new SimpleBeanDefinitionRegistry();
        final ClassPathBeanDefinitionScanner scanner = new ClassPathBeanDefinitionScanner(bdr);
        scanner.resetFilters(false); //remove the default Spring filters that would include all classes with @Component, @Service, and some other annotations
        scanner.setIncludeAnnotationConfig(false); //filter Spring own classes out
        scanner.addIncludeFilter(new AssignableTypeFilter(ModificationInfos.class)); //we search all children classes
        scanner.scan(NetworkModificationApplication.class.getPackageName()); //we scan the application package

        final List<Arguments> arguments = new ArrayList<>(bdr.getBeanDefinitionCount());
        for (final String beanName : bdr.getBeanDefinitionNames()) {
            final String clazzName = bdr.getBeanDefinition(beanName).getBeanClassName();
            if (ModificationInfos.class.getCanonicalName().equals(clazzName) ||
                    ModificationWithAnnotations.class.getName().equals(clazzName)) {
                continue; //skip parent, we want only children and no test class
            }
            final Class<? extends ModificationInfos> clazz = (Class<? extends ModificationInfos>) Class.forName(clazzName);
            arguments.add(Arguments.of(clazz.getSimpleName(), clazz));
        }
        return arguments;
    }

    private static List<Arguments> provideModificationInfosImplementationsWithInstances()
            throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException {
        final List<Arguments> srcArgs = provideModificationInfosImplementationsWithClasses();

        final List<Arguments> newArgs = new ArrayList<>(srcArgs.size());
        for (final Arguments argumentInput : srcArgs) {
            Object[] arguments = argumentInput.get();
            final Class<? extends ModificationInfos> clazz = (Class<? extends ModificationInfos>) arguments[1];
            final ModificationInfos instance = clazz.getDeclaredConstructor().newInstance(); //use default (empty) constructor
            arguments[1] = instance;
            newArgs.add(Arguments.of(arguments));
        }
        return newArgs;
    }

    @ParameterizedTest(name = "for class {0}")
    @MethodSource("provideModificationInfosImplementationsWithInstances")
    void checkMethodsUnsupportedOperationOverrides(final String name, final ModificationInfos instance, final SoftAssertions softly) throws UnsupportedOperationException {
        try {
            softly.assertThat(instance.toEntity()).isNotNull();
        } catch (final NullPointerException rEx) {
            //Assumptions.abort("conversion append on uninitialized fields");
        } catch (final NetworkModificationException ex) {
            //Assumptions.abort("internal check on uninitialized field prevent verification");
        }
        softly.assertThat(instance.toModification()).isNotNull();
        try {
            softly.assertThat(instance.createSubReporter(new ReporterModel("test", "test reporter"))).isNotNull();
        } catch (final NullPointerException rEx) {
            //Assumptions.abort("sub reporter template need uninitialized value");
        }
    }

    @Test
    void isAllSubTypesDeclared() throws Exception {
        final Class[] implementations = provideModificationInfosImplementationsWithClasses().stream()
                .map(args -> (Class<?>) args.get()[1])
                .toArray(Class[]::new);
        assertThat(ModificationInfos.class.getAnnotation(JsonSubTypes.class).value())
                .as("ModificationInfos Jackson SubTypes declared")
                .isNotEmpty()
                .extracting(JsonSubTypes.Type::value)
                .doesNotHaveDuplicates()
                .containsExactlyInAnyOrder(Arrays.stream(implementations).filter(Predicate.not(_class -> Arrays.asList(
                        ScalingInfos.class, BasicEquipmentModificationInfos.class, BranchCreationInfos.class,
                        EquipmentModificationInfos.class, InjectionModificationInfos.class, InjectionCreationInfos.class,
                        EquipmentCreationInfos.class, BranchModificationInfos.class
                    ).contains(_class))).toArray(Class[]::new)); //TODO check if theses classes are forget intentionally or not
    }

    /*
     * because else error in ModificationInfos.getType()
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("provideModificationInfosImplementationsWithClasses")
    void isAllJsonNamesValid(final String name, final Class<? extends ModificationInfos> clazz) throws IllegalArgumentException {
        assumeThat(clazz).as("implementations class").hasAnnotation(JsonTypeName.class); //TODO check if classes without @JsonTypeName wanted or forgotten
        final Condition<String> isInModificationType = new Condition<>(s -> ModificationType.valueOf(s) != null, "is a ModificationType value");
        assertThat(clazz.getAnnotation(JsonTypeName.class).value()).as("implementations class Json name type")
                .satisfies(isInModificationType);
    }

    /*
     * because else error in ModificationInfos.getErrorType()
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("provideModificationInfosImplementationsWithClasses")
    void isAllErrorTypesValid(final String name, final Class<? extends ModificationInfos> clazz) throws IllegalArgumentException {
        assumeThat(clazz).as("implementations class").hasAnnotation(ModificationErrorTypeName.class); //TODO check if classes without @ModificationErrorTypeName wanted or forgotten
        final Condition<String> isInModificationType = new Condition<>(s -> Type.valueOf(s) != null, "is a NetworkModificationException Type value");
        assertThat(clazz.getAnnotation(ModificationErrorTypeName.class).value()).as("implementations class Json name type")
                .satisfies(isInModificationType);
    }
}
